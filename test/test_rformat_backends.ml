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
  List.iter (fun s -> line "escape" (str s) (guard (fun () -> str (H.escape s))))
    [ ""; "plain"; "<a href=\"x\">"; "a & b"; "it's"; "caf\xc3\xa9" ]
