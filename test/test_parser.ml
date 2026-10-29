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

(* The engine accepts only when it reaches a state holding a single
   complete item whose lookahead is eof (Lm_parser.reduce_early). So the
   start production must consume the eof token itself: "prog -> e $eof"
   accepts, while "s -> e" and a recursive start symbol "e" do not. *)
let grammar start =
  let g = P.add_start P.empty start in
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
  let g = P.add_production g "top" "s" [ "e" ] None in
  P.add_production g "prog" "prog" [ "e"; Arg.eof ] None

let loc = Lm_location.bogus_loc "test"

let tokens s =
  List.filter_map (fun c -> match c with
    | '0' .. '9' -> Some ("num", Char.code c - 48)
    | ' ' -> None
    | c -> Some (String.make 1 c, 0)) (List.init (String.length s) (String.get s))

let parse start s =
  let trace = Buffer.create 32 in
  let lexer toks = match toks with
    | [] -> (Arg.eof, loc, [], 0)
    | (sym, v) :: rest -> (sym, loc, rest, v) in
  let rec ipow b e = if e = 0 then 1 else b * ipow b (e - 1) in
  let eval toks action _ args =
    Buffer.add_string trace (action ^ " ");
    let v = match action, args with
      | "num", [ n ] -> n
      | "add", [ a; _; b ] -> a + b
      | "sub", [ a; _; b ] -> a - b
      | "mul", [ a; _; b ] -> a * b
      | "pow", [ a; _; b ] -> ipow a b
      | "paren", [ _; e; _ ] -> e
      | "top", [ e ] -> e
      | "prog", [ e; _ ] -> e
      | _ -> failwith ("bad action " ^ action) in
    (toks, v) in
  guard (fun () -> let _, v = P.parse (grammar start) start lexer eval (tokens s) in
                   string_of_int v ^ " via " ^ Buffer.contents trace)

let () =
  List.iter (fun s -> line "parse prog" (str s) (parse "prog" s))
    [ "1+2*3"; "2*3+4"; "(1+2)*3"; "8-3-2"; "2^3^2"; "2*3^2"; "((7))"; "7"; "1+"; "+1"; "1 2"; "(1"; "" ];
  List.iter (fun s -> line "parse s (s -> e, no eof in the production)" (str s) (parse "s" s)) [ "7"; "1+2" ];
  List.iter (fun s -> line "parse e (recursive start symbol)" (str s) (parse "e" s)) [ "7"; "1+2" ]

(* Locations: token k of the input sits at its column c, as 1:c-1:c+1, and
   each semantic action records the location the parser passes it. A
   reduction should span its symbols, from the first one's start to the
   last one's end. *)
let parse_locs s =
  let trace = Buffer.create 64 in
  let file = Lm_symbol.add "t" in
  let toks =
    List.filter_map (fun (c, ch) -> match ch with
      | '0' .. '9' -> Some ("num", c, Char.code ch - 48)
      | ' ' -> None
      | ch -> Some (String.make 1 ch, c, 0))
      (List.mapi (fun c ch -> (c, ch)) (List.init (String.length s) (String.get s))) in
  let lexer toks = match toks with
    | [] -> (Arg.eof, Lm_location.create_loc file 1 (String.length s) 1 (String.length s), [], 0)
    | (sym, c, v) :: rest -> (sym, Lm_location.create_loc file 1 c 1 (c + 1), rest, v) in
  let show l = let _, _, a, _, b = Lm_location.dest_loc l in Printf.sprintf "%d-%d" a b in
  let eval toks action loc args =
    Buffer.add_string trace (action ^ "@" ^ show loc ^ " ");
    let v = match action, args with
      | "num", [ n ] -> n
      | ("add" | "sub" | "mul" | "pow"), [ a; _; b ] -> a + b
      | "paren", [ _; e; _ ] -> e
      | "prog", [ e; _ ] -> e
      | _ -> 0 in
    (toks, v) in
  guard (fun () -> let _ = P.parse (grammar "prog") "prog" lexer eval toks in Buffer.contents trace)

let () =
  List.iter (fun s -> line "parse locations" (str s) (parse_locs s)) [ "7"; "1+2"; "1+2*3"; "(4)"; "2 * 3" ]

(* remove_production: removing a nonterminal's only production should give
   the grammar that never had it. The error message lists the tokens the
   parser expects, so it shows whether "b" is still taken for a
   nonterminal. *)
let run_g g s =
  let lexer toks = match toks with
    | [] -> (Arg.eof, loc, [], 0)
    | (sym, v) :: rest -> (sym, loc, rest, v) in
  let eval toks action _ _ = (toks, String.length action) in
  guard (fun () -> let _, v = P.parse g "prog" lexer eval (tokens s) in string_of_int v)

let () =
  let base = P.add_production (grammar "prog") "useb" "e" [ "b" ] None in
  let removed = P.remove_production (P.add_production base "bx" "b" [ "x" ] None) "bx" in
  List.iter (fun s ->
      line "remove_production: b -> x removed" (str s) (run_g removed s);
      line "remove_production: b -> x never added" (str s) (run_g base s)) [ "x"; "b"; "x+1" ]

(* Acceptance. A state holding one complete item whose lookahead is only
   $eof is reduced without reading a token, and the parse ends there
   (ReduceAccept). That is right for "prog -> S $eof", whose reduction
   leaves the stack empty; it is the parse's result. It also holds for
   "S -> a" when S is followed only by $eof, and for a start symbol that
   is reduced inside itself. Each line prints the tree the actions built,
   or the error. *)
type tree = Leaf of string | Node of string * tree list
let rec show_tree = function
  | Leaf x -> x
  | Node (a, ks) -> a ^ "(" ^ String.concat "," (List.map show_tree ks) ^ ")"

let run_tree prods s =
  let g = P.add_start P.empty "prog" in
  let g = List.fold_left (fun g (a, l, r) -> P.add_production g a l r None) g prods in
  let input = Array.init (String.length s) (fun i -> String.make 1 s.[i]) in
  let lexer i =
    if i < Array.length input then (input.(i), loc, i + 1, Leaf input.(i))
    else (Arg.eof, loc, i, Leaf Arg.eof) in
  let eval i action _ args = (i, Node (action, args)) in
  guard (fun () -> show_tree (snd (P.parse g "prog" lexer eval 0)))

let () =
  let sa = [ ("top", "prog", [ "S"; Arg.eof ]); ("sa", "S", [ "a" ]) ] in
  List.iter (fun s -> line "accept: prog -> S $eof, S -> a" (str s) (run_tree sa s)) [ "a"; "aa"; "aaa"; "ab"; "" ];
  let sab = [ ("top", "prog", [ "S"; Arg.eof ]); ("sab", "S", [ "a"; "b" ]) ] in
  List.iter (fun s -> line "accept: prog -> S $eof, S -> a b" (str s) (run_tree sab s)) [ "ab"; "abab"; "abc" ];
  let rec_start = [ ("pa", "prog", [ "a"; "prog" ]); ("pb", "prog", [ "b"; Arg.eof ]) ] in
  List.iter (fun s -> line "accept: prog -> a prog | b $eof" (str s) (run_tree rec_start s)) [ "b"; "ab"; "aab"; "abb" ]
