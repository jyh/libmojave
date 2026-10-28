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
