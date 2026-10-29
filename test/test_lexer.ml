open Pin
module Action = struct
  type action = string
  let pp_print_action out a = Lm_printf.pp_print_string out a
  let hash = Hashtbl.hash
  let compare = compare
  let choose = min
end
module L = Lm_lexer.MakeLexer (Lm_channel.LexerInput) (Action)

let lexer =
  List.fold_left (fun l (a, re) -> snd (L.add_clause l a re)) L.empty
    [ ("num", "[0-9]+"); ("id", "[a-z][a-z0-9_]*"); ("ws", "[ \\t\\n]+"); ("op", "[-+*/=]");
      ("lp", "[(]"); ("rp", "[)]"); ("kw_let", "let"); ("str", "\"[^\"]*\""); ("eof", "\\'") ]

let lex_all input =
  let ch = Lm_channel.of_string input in
  let rec loop acc n =
    if n > 50 then List.rev ("...TRUNCATED" :: acc)
    else match L.lex lexer ch with
      | ("eof", _, _, _) -> List.rev ("EOF" :: acc)
      | (a, _, s, _) -> loop ((a ^ ":" ^ str s) :: acc) (n + 1)
      | exception e -> List.rev (("END " ^ Printexc.to_string e) :: acc)
  in
  loop [] 0

let () =
  List.iter (fun s -> line "lex" (str s) (String.concat " " (lex_all s)))
    [ "x1 + 23*(y)"; "let x = \"a b\""; "letter"; "let"; "12ab"; ""; "  "; "a$b"; "\"open" ]

(* union: the second lexer's clauses join after the first's. Between two
   of its own clauses that match the same text, the one that wins should
   be the one that wins in that lexer alone: the first added under
   choose = min, the last added under choose = max (metaprl's
   filter_grammar uses max). *)
module ActionMax = struct include Action let choose = max end
module LMax = Lm_lexer.MakeLexer (Lm_channel.LexerInput) (ActionMax)

let () =
  let mk add empty l = List.fold_left (fun lx (a, re) -> snd (add lx a re)) empty l in
  let base = [ ("num", "[0-9]+"); ("eof", "\\'") ] in
  let second = [ ("id", "[a-z]+"); ("kw_let", "let") ] in
  let first_min lx = match L.lex lx (Lm_channel.of_string "let") with (a, _, _, _) -> a | exception e -> Printexc.to_string e in
  let first_max lx = match LMax.lex lx (Lm_channel.of_string "let") with (a, _, _, _) -> a | exception e -> Printexc.to_string e in
  let m1 = mk L.add_clause L.empty base and m2 = mk L.add_clause L.empty second in
  line "union, choose = min: second alone" (str "let") (first_min m2);
  line "union, choose = min: union first second" (str "let") (first_min (L.union m1 m2));
  let x1 = mk LMax.add_clause LMax.empty base and x2 = mk LMax.add_clause LMax.empty second in
  line "union, choose = max: second alone" (str "let") (first_max x2);
  line "union, choose = max: union first second" (str "let") (first_max (LMax.union x1 x2))
