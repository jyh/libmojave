(* Lm_parser's remaining API, never reached by a test (the 11-02 coverage
   page): ParserPrecedence.add_assoc and assoc, MakeParser's add_assoc,
   build, hash, and the debug_parsetiming report. add_assoc does not change
   a level: it checks that the level exists and has the given associativity,
   and fails otherwise. Cases:
   - the precedence table alone: assoc of the built-in and created levels;
     add_assoc with the same associativity (the table is returned), a
     different one, and a level the table does not hold;
   - a grammar (e -> e op e for + and *, + below *, both left): add_assoc
     with the same associativity leaves every parse as it was, a different
     one fails; build compiles eagerly and restores debug_parse; hash agrees
     for two grammars built alike and after add_assoc, and differs once a
     production is added;
   - debug_parsetiming during a build writes one timing line per phase to
     stderr (captured in a file; the phase names are printed, the times
     not). *)
open Pin

module Pr = Lm_parser.ParserPrecedence

let assoc_name = function
  | Lm_parser.LeftAssoc -> "LeftAssoc" | Lm_parser.RightAssoc -> "RightAssoc"
  | Lm_parser.NonAssoc -> "NonAssoc" | Lm_parser.NoneAssoc -> "NoneAssoc"

let attempt f = try f () with Failure m -> "Failure " ^ m | e -> "raised " ^ Printexc.to_string e

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

let loc = Lm_location.bogus_loc "test"

let grammar () =
  let g = P.add_start P.empty "prog" in
  let g, plus = P.create_prec_gt g P.prec_min Lm_parser.LeftAssoc in
  let g, times = P.create_prec_gt g plus Lm_parser.LeftAssoc in
  let g = P.add_prec g plus "+" in
  let g = P.add_prec g times "*" in
  let g = P.add_production g "bin+" "e" [ "e"; "+"; "e" ] None in
  let g = P.add_production g "bin*" "e" [ "e"; "*"; "e" ] None in
  let g = P.add_production g "num" "e" [ "num" ] None in
  P.add_production g "prog" "prog" [ "e"; Arg.eof ] None, plus, times

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
      | "prog", [ e; _ ] -> e
      | _, [ a; op; b ] -> "(" ^ a ^ op ^ b ^ ")"
      | _ -> failwith action) in
  match P.parse g "prog" lexer eval 0 with
  | _, t -> t
  | exception Lm_parser.ParseError _ -> "syntax error"
  | exception e -> "EXN " ^ Printexc.to_string e

let inputs = [ [ "1"; "+"; "2"; "*"; "3" ]; [ "1"; "*"; "2"; "+"; "3" ]; [ "1"; "+"; "2"; "+"; "3" ]; [ "1"; "+" ] ]
let parses g = String.concat "  " (List.map (parse g) inputs)

(* Run f with stderr going to a file; return the file's lines. *)
let with_stderr f =
  let file = "test_parser_assoc.stderr" in
  flush stderr;
  let saved = Unix.dup Unix.stderr in
  let fd = Unix.openfile file [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644 in
  Unix.dup2 fd Unix.stderr; Unix.close fd;
  (try f () with e -> flush stderr; Unix.dup2 saved Unix.stderr; Unix.close saved; raise e);
  Format.pp_print_flush Format.err_formatter (); flush stderr;
  Unix.dup2 saved Unix.stderr; Unix.close saved;
  let ic = open_in_bin file in
  let rec lines acc = match input_line ic with l -> lines (l :: acc) | exception End_of_file -> List.rev acc in
  let l = lines [] in close_in ic; Sys.remove file; l

let () =
  (* the precedence table alone *)
  let t = Pr.empty in
  let t, p1 = Pr.create_prec_gt t Pr.prec_min Lm_parser.RightAssoc in
  let t, p2 = Pr.create_prec_lt t Pr.prec_max Lm_parser.NonAssoc in
  line "assoc" "prec_min, prec_max, created right, created nonassoc"
    (String.concat ", " (List.map (fun p -> assoc_name (Pr.assoc t p)) [ Pr.prec_min; Pr.prec_max; p1; p2 ]));
  line "add_assoc" "same associativity, the table returned"
    (attempt (fun () -> let t' = Pr.add_assoc t p1 Lm_parser.RightAssoc in
                if t' == t then "the same table" else "a different table"));
  line "add_assoc" "different associativity"
    (attempt (fun () -> ignore (Pr.add_assoc t p1 Lm_parser.LeftAssoc); "accepted"));
  let other, p3 = Pr.create_prec_gt t p2 Lm_parser.LeftAssoc in
  ignore other;
  line "add_assoc" "a level created in another table"
    (attempt (fun () -> ignore (Pr.add_assoc t p3 Lm_parser.LeftAssoc); "accepted"));
  line "assoc" "a level created in another table"
    (attempt (fun () -> assoc_name (Pr.assoc t p3)));
  (* a grammar *)
  let g, plus, times = grammar () in
  line "grammar" "parses" (parses g);
  let g' = P.add_assoc g plus Lm_parser.LeftAssoc in
  line "grammar" "add_assoc + left (same), parses" (parses g');
  line "grammar" "add_assoc * right (different)"
    (attempt (fun () -> ignore (P.add_assoc g times Lm_parser.RightAssoc); "accepted"));
  let g2, _, _ = grammar () in
  line "build" "build g2 false, debug_parse after, parses"
    (P.build g2 false; Printf.sprintf "%b  %s" !Lm_parser.debug_parse (parses g2));
  let g3, _, _ = grammar () in
  let err = with_stderr (fun () -> P.build g3 true) in
  line "build" "build g3 true: debug_parse and debug_parsegen after, stderr written, parses"
    (Printf.sprintf "%b %b  %b  %s" !Lm_parser.debug_parse !Lm_parser.debug_parsegen (err <> []) (parses g3));
  let g4, _, _ = grammar () and g5, _, _ = grammar () in
  line "hash" "two grammars built alike" (string_of_bool (P.hash g4 = P.hash g5));
  line "hash" "after add_assoc (same)" (string_of_bool (P.hash g4 = P.hash (P.add_assoc g4 plus Lm_parser.LeftAssoc)));
  line "hash" "one more production" (string_of_bool (P.hash g4 = P.hash (P.add_production g4 "neg" "e" [ "-"; "e" ] None)));
  let g6, _, _ = grammar () in
  let err = with_stderr (fun () ->
      Lm_parser.debug_parsetiming := true; P.build g6 false; Lm_parser.debug_parsetiming := false) in
  (* each phase prints "Time: <figures> (<phase>)"; keep the phase *)
  let phase l =
    if String.length l > 6 && String.sub l 0 6 = "Time: " && l.[String.length l - 1] = ')' then
      match String.rindex_opt l '(' with
      | Some k -> Some (String.sub l (k + 1) (String.length l - k - 2))
      | None -> None
    else None in
  let phases = List.filter_map phase err in
  line "debug_parsetiming" "build: timed phases, in order"
    (Printf.sprintf "%d of %d lines: %s" (List.length phases) (List.length err) (String.concat " / " phases));
  line "debug_parsetiming" "build: the other lines"
    (String.concat " / " (List.filter (fun l -> phase l = None) err))
