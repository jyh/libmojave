open Pin
module F = Lm_rformat
module H = Lm_rformat_html
module X = Lm_rformat_tex

let words b n =
  for k = 1 to n do
    if k > 1 then F.format_space b;
    F.format_string b ("w" ^ string_of_int k)
  done

let docs : (string * (F.buffer -> unit)) list =
  [ ("plain", fun b -> F.format_string b "hello world");
    ("html specials", fun b -> F.format_string b "a<b & c>d \"q\"");
    ("tex specials", fun b -> F.format_string b "\\x {y} $z$ a_b c^d 10% #1 ~");
    ("szone", fun b -> F.format_szone b; words b 6; F.format_ezone b);
    ("pushm", fun b -> F.format_pushm b 2; F.format_szone b; words b 6; F.format_ezone b; F.format_popm b);
    ("newline", fun b -> F.format_string b "a"; F.format_newline b; F.format_string b "b");
    ("tzone", fun b -> F.format_tzone b "kw"; F.format_string b "let"; F.format_ezone b; F.format_string b " x");
    ("izone", fun b -> F.format_string b "v"; F.format_izone b; F.format_string b "<hidden>"; F.format_ezone b; F.format_string b "w") ]

let build f = let b = F.new_buffer () in f b; b

let tagger =
  Some { H.html_tag_begin = H.FunTagger (fun t -> "<span class=\"" ^ t ^ "\">");
         H.html_tag_end = H.StringTagger "</span>" }

let () =
  List.iter (fun (name, f) ->
      List.iter (fun w ->
          let tag = name ^ " w=" ^ string_of_int w in
          line "html" tag (guard (fun () -> str (H.print_html_string w None (build f))));
          line "html+tagger" tag (guard (fun () -> str (H.print_html_string w tagger (build f))));
          line "tex" tag (guard (fun () -> str (X.print_tex_string w (build f)))))
        [ 80; 10 ])
    docs;
  (* format_string with newlines inside it (its String.index branch) must
     print as the same text with explicit format_newline calls. *)
  let embedded b = F.format_string b "ab\ncd\n\nef" in
  let explicit b =
    F.format_string b "ab"; F.format_newline b; F.format_string b "cd";
    F.format_newline b; F.format_newline b; F.format_string b "ef" in
  List.iter (fun w ->
      let h f = H.print_html_string w None (build f) and t f = X.print_tex_string w (build f) in
      line "html" ("embedded newlines w=" ^ string_of_int w) (guard (fun () -> str (h embedded)));
      line "html" ("embedded = explicit newlines w=" ^ string_of_int w) (guard (fun () -> string_of_bool (h embedded = h explicit)));
      line "tex" ("embedded newlines w=" ^ string_of_int w) (guard (fun () -> str (t embedded)));
      line "tex" ("embedded = explicit newlines w=" ^ string_of_int w) (guard (fun () -> string_of_bool (t embedded = t explicit))))
    [ 80; 10 ];
  List.iter (fun s -> line "escape" (str s) (guard (fun () -> str (H.escape s))))
    [ ""; "plain"; "<a href=\"x\">"; "a & b"; "it's"; "caf\xc3\xa9" ]
