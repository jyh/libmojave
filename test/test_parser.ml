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
