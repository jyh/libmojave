open Pin
module F = Lm_rformat
module T = Lm_rformat_text

let widths = [ 80; 20; 10; 4 ]

let words buf n =
  for k = 1 to n do
    if k > 1 then F.format_space buf;
    F.format_string buf ("word" ^ string_of_int k)
  done

(* Each document is built by a function on a fresh buffer. *)
let docs : (string * (F.buffer -> unit)) list =
  [ ("plain", fun b -> F.format_string b "hello"; F.format_char b ' '; F.format_string b "world");
    ("spaces", fun b -> words b 8);
    ("hzone hbreak", fun b ->
       F.format_hzone b;
       for k = 1 to 6 do
         if k > 1 then F.format_hbreak b "" " ";
         F.format_string b ("item" ^ string_of_int k)
       done;
       F.format_ezone b);
    ("szone sbreak", fun b ->
       F.format_szone b;
       for k = 1 to 6 do
         if k > 1 then F.format_sbreak b "" " ";
         F.format_string b ("item" ^ string_of_int k)
       done;
       F.format_ezone b);
    ("lzone", fun b -> F.format_lzone b; words b 6; F.format_ezone b);
    ("cbreak", fun b ->
       F.format_szone b;
       F.format_string b "f";
       F.format_cbreak b "(" "\\";
       F.format_string b "argument_one";
       F.format_cbreak b "," ";";
       F.format_string b "argument_two";
       F.format_ezone b);
    ("pushm 2", fun b ->
       F.format_pushm b 2;
       F.format_szone b;
       F.format_string b "let x =";
       F.format_space b;
       words b 5;
       F.format_ezone b;
       F.format_popm b);
    ("pushm_str", fun b ->
       F.format_pushm_str b "> ";
       F.format_szone b;
       words b 6;
       F.format_ezone b;
       F.format_popm b);
    ("nested", fun b ->
       F.format_hzone b;
       F.format_string b "outer";
       F.format_hbreak b "" " ";
       F.format_szone b;
       words b 4;
       F.format_ezone b;
       F.format_hbreak b "" " ";
       F.format_string b "end";
       F.format_ezone b);
    ("newline", fun b -> F.format_string b "a"; F.format_newline b; F.format_string b "b");
    ("atoms", fun b ->
       F.format_int b (-42);
       F.format_space b;
       F.format_num b (Lm_num.num_of_string "123456789012345678901234567890");
       F.format_space b;
       F.format_quoted_string b "q\"uote\n";
       F.format_space b;
       F.format_raw_string b "raw");
    ("izone", fun b -> F.format_string b "vis"; F.format_izone b; F.format_string b "INVISIBLE"; F.format_ezone b; F.format_string b "ible");
    ("tzone", fun b -> F.format_tzone b "tag"; F.format_string b "tagged"; F.format_ezone b; F.format_string b " after");
    ("format_buffer", fun b ->
       let inner = F.new_buffer () in
       F.format_szone inner; words inner 3; F.format_ezone inner;
       F.format_string b "[";
       F.format_buffer b inner;
       F.format_string b "]") ]

let build f = let b = F.new_buffer () in f b; b

(* A printer that records every call, to show what reaches a backend. *)
let record buf width =
  let log = ref [] in
  let add s = log := s :: !log in
  let p =
    { F.print_string = (fun s -> add ("S" ^ str s));
      print_invis = (fun s -> add ("I" ^ str s));
      print_atomic = (fun s -> add ("A" ^ str s));
      print_tab = (fun (n, s) l -> add ("T" ^ string_of_int n ^ str s ^ list str l));
      print_begin_tag = (fun s -> add ("<" ^ s));
      print_end_tag = (fun s -> add (">" ^ s)) }
  in
  F.print_to_printer buf width p;
  String.concat " " (List.rev !log)

let () =
  List.iter (fun (name, f) ->
      List.iter (fun w ->
          line "text" (name ^ " w=" ^ string_of_int w) (guard (fun () -> str (T.print_text_string w (build f)))))
        widths)
    docs;
  List.iter (fun (name, f) -> line "printer" (name ^ " w=20") (guard (fun () -> record (build f) 20))) docs;
  (* Pinned as it stands (CHANGES): when a string needs quoting, only its
     first character is printed, and a tab is escaped as \r. *)
  List.iter (fun s -> line "format_quoted_string" (str s)
      (guard (fun () -> str (T.print_text_string 80 (build (fun b -> F.format_quoted_string b s))))))
    [ "plain"; ""; "two words"; "tab\there"; "line\nbreak"; "q\"uote"; "'prime" ];
  (* buffer operations *)
  let b = build (fun b -> words b 3) in
  line "buffer_is_empty" "new" (string_of_bool (F.buffer_is_empty (F.new_buffer ())));
  line "buffer_is_empty" "3 words" (string_of_bool (F.buffer_is_empty b));
  let c = F.clone_buffer b in
  F.clear_buffer b;
  line "clear_buffer" "then empty" (string_of_bool (F.buffer_is_empty b));
  line "clone_buffer" "survives clear" (guard (fun () -> str (T.print_text_string 80 c)));
  let z = F.new_buffer () in
  F.format_szone z; F.format_hzone z;
  line "zone_depth" "two open" (string_of_int (F.zone_depth z));
  F.format_ezone z;
  line "zone_depth" "one closed" (string_of_int (F.zone_depth z));
  line "default_width" "" (string_of_int F.default_width);
  line "line_format" "w=10" (guard (fun () -> str (T.line_format 10 (fun b -> words b 4))));
  (* marshalling round trip *)
  let bufs = List.map (fun (_, f) -> build f) docs in
  line "marshal/unmarshal" "all docs, text w=20 equal"
    (guard (fun () ->
         let back = F.unmarshal_buffers (F.marshal_buffers bufs) in
         string_of_bool
           (List.length back = List.length bufs
            && List.for_all2 (fun a b -> T.print_text_string 20 a = T.print_text_string 20 b) bufs back)))
