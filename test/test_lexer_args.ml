(* Lm_lexer's \( ... \) arguments. add_clause returns the clause's arity
   (its number of groups, counting only the first branch of an
   alternation, since the branches must have equal counts); lex returns
   the text of each group. Each line shows the arity, the lexeme and the
   argument list. *)
open Pin

module Action = struct
  type action = string
  let pp_print_action out a = Lm_printf.pp_print_string out a
  let hash = Hashtbl.hash
  let compare = compare
  let choose = min
end
module L = Lm_lexer.MakeLexer (Lm_channel.LexerInput) (Action)

let run re inputs =
  match L.add_clause L.empty "c" re with
  | exception e -> line "add_clause" (str re) ("EXN " ^ Printexc.to_string e)
  | n, l ->
      List.iter (fun s ->
          line (Printf.sprintf "lex %s (arity %d)" re n) (str s)
            (guard (fun () ->
                 let _, _, x, args = L.lex l (Lm_channel.of_string s) in
                 str x ^ " " ^ list str args))) inputs

let () =
  run "\\(a*\\)b" [ "aab"; "b" ];
  run "\\(a\\)*b" [ "aab"; "ab"; "b" ];
  run "\\([ab]\\)*c" [ "abc"; "c" ];
  run "\\(a*\\)\\(a*\\)" [ "aaa" ];
  run "\\(a\\(b\\)\\)c" [ "abc" ];
  run "\\(\\(a\\)b\\)*" [ "abab"; "" ];
  run "x\\(a\\)?\\(b\\)" [ "xab"; "xb" ];
  run "\\(a\\)|\\(b\\)" [ "a"; "b" ];
  run "\\(a\\)x|\\(b\\)y" [ "ax"; "by" ];
  run "\\(a\\)\\(b\\)|\\(c\\)\\(d\\)" [ "ab"; "cd" ];
  run "q\\(\\(a\\)|\\(b\\)\\)*" [ "q"; "qab"; "qba" ];
  (* metaprl's one lexer group, in theories/itt/extensions: a quoted string *)
  run "\"\\(\\\\.|[^\"]*\\)\"" [ "\"abc\""; "\"\"" ]
