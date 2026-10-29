(* Lm_parser's precedence and associativity against a model, from a fixed
   LCG. Each round draws a random precedence table: 1-4 levels, each
   LeftAssoc, RightAssoc or NonAssoc, created in increasing order with
   create_prec_gt, and each of the operators + - * / < = given one level
   with add_prec. The grammar is e -> e op e for each operator,
   e -> ( e ), e -> num, prog -> e $eof. Random token strings, most of
   them well-formed expressions, are parsed into a bracketed tree and
   compared with a precedence-climbing parser: a higher level binds
   tighter, LeftAssoc groups to the left, RightAssoc to the right, and two
   operators of one NonAssoc level side by side are a syntax error. *)
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

let seed = ref 93
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n

let ops = [ "+"; "-"; "*"; "/"; "<"; "=" ]
let loc = Lm_location.bogus_loc "test"

type assoc = L | R | N
let assoc_name = function L -> "left" | R -> "right" | N -> "nonassoc"

(* levels: assoc of each level, lowest first; level_of: operator -> level *)
let build levels level_of =
  let g = P.add_start P.empty "prog" in
  let g, precs, _ =
    List.fold_left (fun (g, precs, base) a ->
        let g, p = P.create_prec_gt g base (match a with L -> Lm_parser.LeftAssoc | R -> Lm_parser.RightAssoc | N -> Lm_parser.NonAssoc) in
        (g, precs @ [ p ], p)) (g, [], P.prec_min) levels in
  let g = List.fold_left (fun g op -> P.add_prec g (List.nth precs (level_of op)) op) g ops in
  let g = List.fold_left (fun g op -> P.add_production g ("bin" ^ op) "e" [ "e"; op; "e" ] None) g ops in
  let g = P.add_production g "paren" "e" [ "("; "e"; ")" ] None in
  let g = P.add_production g "num" "e" [ "num" ] None in
  P.add_production g "prog" "prog" [ "e"; Arg.eof ] None

let parse g toks =
  let arr = Array.of_list toks in
  let lexer i =
    if i < Array.length arr then
      let t = arr.(i) in
      if t.[0] >= '0' && t.[0] <= '9' then ("num", loc, i + 1, t) else (t, loc, i + 1, t)
    else (Arg.eof, loc, i, "") in
  let eval i action _ args =
    (i, match action, args with
      | "num", [ n ] -> n
      | "paren", [ _; e; _ ] -> e
      | "prog", [ e; _ ] -> e
      | _, [ a; op; b ] -> "(" ^ a ^ op ^ b ^ ")"
      | _ -> failwith action) in
  match P.parse g "prog" lexer eval 0 with
  | _, t -> t
  | exception Lm_parser.ParseError _ -> "syntax error"
  | exception e -> "EXN " ^ Printexc.to_string e

exception Syntax

(* Precedence climbing over the tokens: the bracketed tree, or Syntax. *)
let climb levels level_of toks =
  let toks = ref toks in
  let peek () = match !toks with t :: _ -> Some t | [] -> None in
  let next () = match !toks with t :: r -> toks := r; t | [] -> raise Syntax in
  let is_op t = List.mem t ops in
  let rec primary () =
    match next () with
    | "(" -> let v = expr 0 in if next () <> ")" then raise Syntax; v
    | t when t.[0] >= '0' && t.[0] <= '9' -> t
    | _ -> raise Syntax
  and expr minp =
    let lhs = ref (primary ()) in
    let last_nonassoc = ref (-1) in
    let continue = ref true in
    while !continue do
      match peek () with
      | Some op when is_op op ->
          let p = level_of op in
          if p < minp then continue := false
          else begin
            if !last_nonassoc = p then raise Syntax;
            ignore (next ());
            let a = List.nth levels p in
            let rhs = expr (if a = R then p else p + 1) in
            lhs := "(" ^ !lhs ^ op ^ rhs ^ ")";
            last_nonassoc := (if a = N then p else -1)
          end
      | _ -> continue := false
    done;
    !lhs in
  let v = expr 0 in
  if !toks <> [] then raise Syntax;
  v

let rec gen depth =
  if depth = 0 || rnd 4 = 0 then [ string_of_int (1 + rnd 9) ]
  else if rnd 6 = 0 then [ "(" ] @ gen (depth - 1) @ [ ")" ]
  else gen (depth - 1) @ [ List.nth ops (rnd 6) ] @ gen (depth - 1)

(* Occasionally break a well-formed string: drop or duplicate a token. *)
let mutate toks =
  match rnd 8 with
  | 0 when List.length toks > 1 -> let k = rnd (List.length toks) in List.filteri (fun i _ -> i <> k) toks
  | 1 -> let k = rnd (List.length toks) in List.concat (List.mapi (fun i t -> if i = k then [ t; t ] else [ t ]) toks)
  | _ -> toks

let () =
  let bad = ref 0 and first = ref "" and total = ref 0 and errors = ref 0 and refused = ref 0 in
  for _ = 1 to 300 do
    let nlev = 1 + rnd 4 in
    let levels = List.init nlev (fun _ -> match rnd 3 with 0 -> L | 1 -> R | _ -> N) in
    let table = List.map (fun op -> (op, rnd nlev)) ops in
    let level_of op = List.assoc op table in
    let desc () =
      String.concat " " (List.mapi (fun i a ->
          Printf.sprintf "%d:%s{%s}" i (assoc_name a)
            (String.concat "" (List.filter_map (fun (op, l) -> if l = i then Some op else None) table))) levels) in
    match build levels level_of with
    | exception e -> incr refused; if !first = "" then first := desc () ^ ": " ^ Printexc.to_string e
    | g ->
        for _ = 1 to 40 do
          incr total;
          let toks = mutate (gen 4) in
          let want = try climb levels level_of toks with Syntax -> "syntax error" in
          if want = "syntax error" then incr errors;
          let got = parse g toks in
          if want <> got then begin
            incr bad;
            if !first = "" then
              first := Printf.sprintf "%s on %s: model %s, parser %s" (desc ()) (String.concat " " toks) want got
          end
        done
  done;
  Printf.printf "tables refused: %d; inputs: %d, of which the model rejects %d\n" !refused !total !errors;
  line "precedence" (string_of_int !total ^ " inputs")
    (if !bad = 0 then "0 disagree" else Printf.sprintf "%d disagree (first: %s)" !bad !first)
