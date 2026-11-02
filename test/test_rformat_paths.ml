(* The output paths of the three Lm_rformat backends. Each backend prints
   a document to a string (through Lm_rformat_raw's buffer printer), to a
   Buffer.t, and to an out_channel (through the channel printer, a
   separate implementation); the three texts must be byte-identical.

   make_*_printer is checked against a raw printer written here. For text
   it gives the same text. For html and tex it does NOT: those printers
   hold the current line in a private buffer that only the module's own
   print_*_raw flushes (html_flush / tex_flush, and tex's prologue and
   epilogue), and make_html_printer / make_tex_printer return the printer
   without it. So their output is a strict prefix of the full text: the
   last line is lost (all of it, for a one-line document). Nothing in
   libmojave, metaprl or omake calls those two. That is pinned here as it
   is, by "prefix" and "equal" lines, not endorsed. *)
open Pin
module F = Lm_rformat
module R = Lm_rformat_raw
module T = Lm_rformat_text
module H = Lm_rformat_html
module X = Lm_rformat_tex

let words b n =
  for k = 1 to n do
    if k > 1 then F.format_space b;
    F.format_string b ("w" ^ string_of_int k)
  done

let docs : (string * (F.buffer -> unit)) list =
  [ ("plain", fun b -> F.format_string b "hello world");
    ("specials", fun b -> F.format_string b "a<b & c>d \"q\" \\x {y} $z$ 10% #1 ~");
    ("pushm", fun b -> F.format_pushm b 4; F.format_szone b; words b 9; F.format_ezone b; F.format_popm b);
    ("newline", fun b -> F.format_string b "a"; F.format_newline b; F.format_string b "b");
    ("tzone", fun b -> F.format_tzone b "kw"; F.format_string b "let"; F.format_ezone b; F.format_string b " x");
    ("izone", fun b -> F.format_string b "v"; F.format_izone b; F.format_string b "<hidden>"; F.format_ezone b; F.format_string b "w") ]

let build f = let b = F.new_buffer () in f b; b

(* A raw printer of our own: pieces in a list, spaces as one string. *)
let recording () =
  let acc = ref [] in
  let raw =
    { R.raw_print_string = (fun s off len -> acc := String.sub s off len :: !acc);
      R.raw_print_flush = (fun () -> ());
      R.raw_print_newline = (fun () -> acc := "\n" :: !acc);
      R.raw_print_spaces = (fun n -> acc := String.make (max n 0) ' ' :: !acc) }
  in
  raw, (fun () -> String.concat "" (List.rev !acc))

let via_channel print =
  let file = Filename.temp_file "rformat_paths" ".txt" in
  let oc = open_out_bin file in
  print oc; close_out oc;
  let ic = open_in_bin file in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic; Sys.remove file; s

let tagger =
  Some { H.html_tag_begin = H.FunTagger (fun t -> "<span class=\"" ^ t ^ "\">");
         H.html_tag_end = H.StringTagger "</span>" }

type backend =
  { name : string;
    to_string : int -> F.buffer -> string;
    to_buffer : int -> F.buffer -> Buffer.t -> unit;
    to_channel : int -> F.buffer -> out_channel -> unit;
    make : R.raw_printer -> F.printer }

let backends =
  [ { name = "text"; to_string = T.print_text_string; to_buffer = T.print_text_buffer;
      to_channel = T.print_text_channel; make = T.make_text_printer };
    { name = "html"; to_string = (fun w b -> H.print_html_string w None b);
      to_buffer = (fun w b o -> H.print_html_buffer w None b o);
      to_channel = (fun w b o -> H.print_html_channel w None b o); make = H.make_html_printer None };
    { name = "html+tagger"; to_string = (fun w b -> H.print_html_string w tagger b);
      to_buffer = (fun w b o -> H.print_html_buffer w tagger b o);
      to_channel = (fun w b o -> H.print_html_channel w tagger b o); make = H.make_html_printer tagger };
    { name = "tex"; to_string = X.print_tex_string; to_buffer = X.print_tex_buffer;
      to_channel = X.print_tex_channel; make = X.make_tex_printer } ]

let () =
  List.iter (fun be ->
      List.iter (fun (dname, f) ->
          List.iter (fun w ->
              let s = be.to_string w (build f) in
              let b = Buffer.create 64 in
              be.to_buffer w (build f) b;
              let c = via_channel (be.to_channel w (build f)) in
              let raw, contents = recording () in
              F.print_to_printer (build f) w (be.make raw);
              let r = contents () in
              let tag = Printf.sprintf "%s w=%d" dname w in
              let three = s = Buffer.contents b && s = c in
              line be.name (tag ^ ": string = buffer = channel")
                (if three then "true"
                 else Printf.sprintf "false (string %S buffer %S channel %S)" s (Buffer.contents b) c);
              (* tex's print_tex_raw writes a prologue the bare printer never
                 does, so the printer is compared with what follows it. *)
              let body =
                let p = "\\texfalse\\iftex%\n" in
                if be.name = "tex" && String.length s >= String.length p && String.sub s 0 (String.length p) = p
                then String.sub s (String.length p) (String.length s - String.length p) else s
              in
              let prefix = String.length r <= String.length body && String.sub body 0 (String.length r) = r in
              line be.name (tag ^ ": make_printer is a prefix of string (tex: after its prologue)") (string_of_bool prefix);
              line be.name (tag ^ ": make_printer = string") (string_of_bool (r = s)))
            [ 80; 10 ])
        docs)
    backends
