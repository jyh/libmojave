(* Lm_parser against a model, from a fixed LCG.

   Random grammars over terminals a b c and nonterminals S A B: each
   nonterminal has 1-3 productions with right-hand sides of length 0-3.
   The start production is "prog -> S $eof", the shape the engine needs to
   accept (see test_parser.ml). A grammar the generator refuses with a
   conflict is counted and skipped; the model is for the grammars it
   builds, which are LALR(1) and so unambiguous.

   For every built grammar and every string over {a,b,c} of length <= 5:
   - accept: the parser accepts exactly when an Earley recognizer does;
   - tree: an accepted parse's semantic actions form a derivation of the
     input (each node's children match its production's right-hand side,
     and the leaves are the input).
   Then the same check on grammars made by remove_production and by union,
   against the grammar built directly from the resulting productions.
   Then precedence: random expressions over 1-9 + - * ^ ( ), the parser's
   value against a precedence-climbing evaluator. *)
open Pin

module Arg = struct
  type symbol = string
  let eof = "$eof"
  let to_string s = s
  let pp_print_symbol out s = Lm_printf.pp_print_string out s
  let hash_symbol = Hashtbl.hash
  let compare_symbol = compare
  type action = string
  let pp_print_action out a = Lm_printf.pp_print_string out a
  let hash_action = Hashtbl.hash
  let compare_action = compare
end
module P = Lm_parser.MakeParser (Arg) (Lm_parser.ParserPrecedence)

let seed = ref 88
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n

let bad = Hashtbl.create 8 and first = Hashtbl.create 8 and order = ref [] and steps = Hashtbl.create 8
let check op ok d =
  if not (List.mem op !order) then order := op :: !order;
  Hashtbl.replace steps op (1 + try Hashtbl.find steps op with Not_found -> 0);
  if not ok then begin
    Hashtbl.replace bad op (1 + try Hashtbl.find bad op with Not_found -> 0);
    if not (Hashtbl.mem first op) then Hashtbl.replace first op (d ())
  end

(* The generator prints each conflict, and the state, through
   Lm_printf.stderr, which is Format.err_formatter: discard its output
   while compiling. (Redirecting the file descriptor failed under MSVC:
   Sys_error "Bad file descriptor".) *)
let quiet f =
  let saved = Format.pp_get_formatter_out_functions Format.err_formatter () in
  Format.pp_print_flush Format.err_formatter ();
  Format.pp_set_formatter_out_functions Format.err_formatter
    { saved with Format.out_string = (fun _ _ _ -> ()); out_flush = (fun () -> ()) };
  let restore () =
    Format.pp_print_flush Format.err_formatter ();
    Format.pp_set_formatter_out_functions Format.err_formatter saved in
  match f () with
  | v -> restore (); v
  | exception e -> restore (); raise e

(* A production: action name, left side, right side. *)
type prod = { name : string; lhs : string; rhs : string list }

let terms = [| "a"; "b"; "c" |]
let nonterms = [| "S"; "A"; "B" |]
let is_nonterm prods x = List.exists (fun p -> p.lhs = x) prods

let show_prods prods =
  String.concat ", " (List.map (fun p -> p.lhs ^ "->" ^ String.concat " " p.rhs) prods)

let build prods =
  let g = P.add_start P.empty "prog" in
  let g = List.fold_left (fun g p -> P.add_production g p.name p.lhs p.rhs None) g prods in
  P.add_production g "prog" "prog" [ "S"; Arg.eof ] None

let random_grammar () =
  let k = ref 0 in
  List.concat_map (fun n ->
      List.init (1 + rnd 3) (fun _ ->
          incr k;
          let rhs = List.init (rnd 4) (fun _ ->
              let i = rnd 6 in if i < 3 then terms.(i) else nonterms.(i - 3)) in
          { name = Printf.sprintf "p%d" !k; lhs = n; rhs }))
    (Array.to_list nonterms)

(* Earley recognizer for S over the input, each set closed to a fixpoint
   (so nullable nonterminals complete correctly). Items are
   (production index, dot, origin). *)
let earley prods input =
  let prods = Array.of_list prods in
  let n = Array.length input in
  let sets = Array.init (n + 1) (fun _ -> Hashtbl.create 16) in
  let add i it = if Hashtbl.mem sets.(i) it then false else (Hashtbl.replace sets.(i) it (); true) in
  let predict i x = let ch = ref false in
    Array.iteri (fun k p -> if p.lhs = x && add i (k, 0, i) then ch := true) prods; !ch in
  ignore (predict 0 "S");
  for i = 0 to n do
    let changed = ref true in
    while !changed do
      changed := false;
      let items = Hashtbl.fold (fun it () acc -> it :: acc) sets.(i) [] in
      List.iter (fun (k, d, o) ->
          let p = prods.(k) in
          if d = List.length p.rhs then
            (* complete *)
            Hashtbl.iter (fun (k2, d2, o2) () ->
                let p2 = prods.(k2) in
                if d2 < List.length p2.rhs && List.nth p2.rhs d2 = p.lhs then
                  if add i (k2, d2 + 1, o2) then changed := true)
              (Hashtbl.copy sets.(o))
          else begin
            let x = List.nth p.rhs d in
            if is_nonterm (Array.to_list prods) x then begin
              if predict i x then changed := true
            end
          end) items
    done;
    if i < n then
      Hashtbl.iter (fun (k, d, o) () ->
          let p = prods.(k) in
          if d < List.length p.rhs && List.nth p.rhs d = input.(i) then
            ignore (add (i + 1) (k, d + 1, o))) sets.(i)
  done;
  Hashtbl.fold (fun (k, d, o) () acc ->
      acc || (o = 0 && prods.(k).lhs = "S" && d = List.length prods.(k).rhs)) sets.(n) false

type tree = Leaf of string | Node of string * tree list

let loc = Lm_location.bogus_loc "test"

let parse g input =
  let lexer i =
    if i < Array.length input then (input.(i), loc, i + 1, Leaf input.(i))
    else (Arg.eof, loc, i, Leaf Arg.eof) in
  let eval i action _ args = (i, Node (action, args)) in
  match P.parse g "prog" lexer eval 0 with
  | _, t -> Ok t
  | exception Lm_parser.ParseError _ -> Error "reject"
  | exception e -> Error ("EXN " ^ Printexc.to_string e)

(* The tree is a derivation of prog -> S $eof whose S-yield is the input. *)
let derivation prods input t =
  let sym = function Leaf x -> x | Node (a, _) -> (try (List.find (fun p -> p.name = a) prods).lhs with Not_found -> "?" ^ a) in
  let rec ok = function
    | Leaf _ -> true
    | Node (a, kids) ->
        (match List.find_opt (fun p -> p.name = a) prods with
         | Some p -> List.map sym kids = p.rhs && List.for_all ok kids
         | None -> false) in
  let rec leaves = function Leaf x -> [ x ] | Node (_, ks) -> List.concat_map leaves ks in
  match t with
  | Node ("prog", [ s; Leaf e ]) when e = Arg.eof -> sym s = "S" && ok s && leaves s = Array.to_list input
  | _ -> false

let rec strings n =
  if n = 0 then [ [||] ]
  else
    let shorter = strings (n - 1) in
    shorter @ List.concat_map (fun s -> if Array.length s = n - 1 then List.map (fun t -> Array.append s [| t |]) (Array.to_list terms) else []) shorter

let all_strings = List.sort_uniq compare (strings 5)
let show_input s = "\"" ^ String.concat "" (Array.to_list s) ^ "\""

let built = ref 0 and conflicts = ref 0 and accepted = ref 0 and inputs = ref 0

(* Compare a parser against the model over every input; tag names the check. *)
let compare_all tag g prods =
  List.iter (fun input ->
      incr inputs;
      let want = earley prods input in
      let got = parse g input in
      (match got with Ok _ -> if tag = "accept" then incr accepted | Error _ -> ());
      check tag (want = (match got with Ok _ -> true | Error _ -> false)) (fun () ->
          Printf.sprintf "%s on %s: model %b, parser %s" (show_prods prods) (show_input input) want
            (match got with Ok _ -> "accept" | Error e -> e));
      match got with
      | Ok t -> check (tag ^ " tree") (derivation prods input t) (fun () ->
          Printf.sprintf "%s on %s" (show_prods prods) (show_input input))
      | Error _ -> ()) all_strings

let compiles prods =
  let g = build prods in
  match quiet (fun () -> P.compile g) with
  | () -> Some g
  | exception Invalid_argument m when String.length m >= 16 && String.sub m 0 16 = "Lm_parser.shift_"
                                      || String.length m >= 17 && String.sub m 0 17 = "Lm_parser.reduce_" -> None
  | exception e -> check "compile" false (fun () -> show_prods prods ^ ": " ^ Printexc.to_string e); None

let () =
  for _ = 1 to 300 do
    let prods = random_grammar () in
    match compiles prods with
    | None -> incr conflicts
    | Some g ->
        incr built;
        compare_all "accept" g prods;
        (* remove one production, by name, against the grammar without it *)
        let victim = List.nth prods (rnd (List.length prods)) in
        let rest = List.filter (fun p -> p.name <> victim.name) prods in
        (match compiles rest with
         | None -> ()
         | Some _ -> compare_all "remove_production" (P.remove_production g victim.name) rest);
        (* union with a second grammar sharing no action names *)
        let extra = List.map (fun p -> { p with name = "q" ^ p.name }) (random_grammar ()) in
        let extra = List.filteri (fun i _ -> i < 2) extra in
        (match compiles (prods @ extra) with
         | None -> ()
         | Some _ ->
             let g2 = List.fold_left (fun g p -> P.add_production g p.name p.lhs p.rhs None) P.empty extra in
             compare_all "union" (P.union g g2) (prods @ extra))
  done

(* Precedence: + - left, * left, ^ right, tighter in that order. *)
let expr_grammar =
  let g = P.add_start P.empty "prog" in
  let g, plus = P.create_prec_gt g P.prec_min Lm_parser.LeftAssoc in
  let g, times = P.create_prec_gt g plus Lm_parser.LeftAssoc in
  let g, pow = P.create_prec_gt g times Lm_parser.RightAssoc in
  let g = P.add_prec g plus "+" in
  let g = P.add_prec g plus "-" in
  let g = P.add_prec g times "*" in
  let g = P.add_prec g pow "^" in
  let g = P.add_production g "add" "e" [ "e"; "+"; "e" ] None in
  let g = P.add_production g "sub" "e" [ "e"; "-"; "e" ] None in
  let g = P.add_production g "mul" "e" [ "e"; "*"; "e" ] None in
  let g = P.add_production g "pow" "e" [ "e"; "^"; "e" ] None in
  let g = P.add_production g "paren" "e" [ "("; "e"; ")" ] None in
  let g = P.add_production g "num" "e" [ "num" ] None in
  P.add_production g "prog" "prog" [ "e"; Arg.eof ] None

(* Square-and-multiply: a tower like 5^3^3^3 has an exponent near 10^12.
   Both sides wrap the same way on overflow, so the comparison is exact. *)
let ipow b e =
  let rec go acc b e = if e <= 0 then acc else go (if e land 1 = 1 then acc * b else acc) (b * b) (e lsr 1) in
  go 1 b e

(* A random expression as tokens; exponents kept small by the generator
   (a ^ only takes a digit or a parenthesised digit on its right). *)
let rec gen_expr depth =
  if depth = 0 || rnd 3 = 0 then [ string_of_int (1 + rnd 9) ]
  else match rnd 5 with
    | 0 -> [ "(" ] @ gen_expr (depth - 1) @ [ ")" ]
    | 1 -> gen_expr (depth - 1) @ [ "^"; string_of_int (1 + rnd 3) ]
    | k -> gen_expr (depth - 1) @ [ [| "+"; "-"; "*" |].(k - 2) ] @ gen_expr (depth - 1)

(* Precedence climbing over the token list. *)
let climb toks =
  let toks = ref toks in
  let peek () = match !toks with t :: _ -> Some t | [] -> None in
  let next () = match !toks with t :: r -> toks := r; t | [] -> failwith "eof" in
  let prec = function "+" | "-" -> Some (1, true) | "*" -> Some (2, true) | "^" -> Some (3, false) | _ -> None in
  let rec primary () =
    match next () with
    | "(" -> let v = expr 0 in ignore (next ()); v
    | t -> int_of_string t
  and expr minp =
    let lhs = ref (primary ()) in
    let continue = ref true in
    while !continue do
      match peek () with
      | Some op -> (match prec op with
          | Some (p, left) when p >= minp ->
              ignore (next ());
              let rhs = expr (if left then p + 1 else p) in
              lhs := (match op with "+" -> !lhs + rhs | "-" -> !lhs - rhs | "*" -> !lhs * rhs | _ -> ipow !lhs rhs)
          | _ -> continue := false)
      | None -> continue := false
    done;
    !lhs in
  expr 0

let () =
  for _ = 1 to 2000 do
    let toks = gen_expr 4 in
    let arr = Array.of_list toks in
    let lexer i =
      if i < Array.length arr then
        let t = arr.(i) in
        if t.[0] >= '0' && t.[0] <= '9' then ("num", loc, i + 1, int_of_string t) else (t, loc, i + 1, 0)
      else (Arg.eof, loc, i, 0) in
    let eval i action _ args =
      (i, match action, args with
        | "num", [ n ] -> n
        | "add", [ a; _; b ] -> a + b
        | "sub", [ a; _; b ] -> a - b
        | "mul", [ a; _; b ] -> a * b
        | "pow", [ a; _; b ] -> ipow a b
        | "paren", [ _; e; _ ] -> e
        | "prog", [ e; _ ] -> e
        | _ -> failwith action) in
    let want = climb toks in
    let got = try Ok (snd (P.parse expr_grammar "prog" lexer eval 0)) with e -> Error (Printexc.to_string e) in
    check "precedence" (got = Ok want) (fun () ->
        Printf.sprintf "%s: model %d, parser %s" (String.concat " " toks) want
          (match got with Ok v -> string_of_int v | Error e -> e))
  done

let () =
  Printf.printf "grammars: %d built, %d refused with a conflict; %d accepted of the accept inputs\n" !built !conflicts !accepted;
  List.iter (fun op ->
      let k = try Hashtbl.find bad op with Not_found -> 0 in
      line op (string_of_int (Hashtbl.find steps op) ^ " checks")
        (if k = 0 then "0 disagree" else Printf.sprintf "%d disagree (first: %s)" k (Hashtbl.find first op)))
    (List.rev !order)
