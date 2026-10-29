(* Lm_format_util and Lm_print_util pinned, through buffers and the
   standard formatter: paragraphs at a narrow margin, the y formatter
   (output to two formatters), and separated lists, each inside a box
   the caller opens (print_sep_list's breaks belong to its caller's box). *)
open Pin
module F = Lm_format_util
module P = Lm_print_util

let to_buffer margin f =
  let b = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer b in
  Format.pp_set_margin fmt margin;
  f fmt;
  Format.pp_print_flush fmt ();
  Buffer.contents b

let () =
  let text = "the quick brown fox jumps over the lazy dog" in
  line "pp_print_paragraph at margin 12" "" (str (to_buffer 12 (fun fmt -> F.pp_print_paragraph fmt text)));
  line "pp_print_paragraph_bare at margin 12" "" (str (to_buffer 12 (fun fmt -> F.pp_print_paragraph_bare fmt text)));
  line "pp_print_paragraph of \"\"" "" (str (to_buffer 12 (fun fmt -> F.pp_print_paragraph fmt "")));
  let b1 = Buffer.create 16 and b2 = Buffer.create 16 in
  let f1 = Format.formatter_of_buffer b1 and f2 = Format.formatter_of_buffer b2 in
  let y = F.y_formatter f1 f2 in
  Format.fprintf y "hello %d@." 42;
  line "y_formatter: both buffers" "" (str (Buffer.contents b1) ^ " " ^ str (Buffer.contents b2));
  line "tabstop" "" (string_of_int P.tabstop);
  Lm_printf.print_string "sep_list: "; Lm_printf.open_hovbox 0; P.print_sep_list "," Lm_printf.print_string [ "a"; "b"; "c" ]; Lm_printf.close_box (); Lm_printf.print_newline ();
  Lm_printf.print_string "sep_list_no_space: "; Lm_printf.open_hovbox 0; P.print_sep_list_no_space "," Lm_printf.print_string [ "a"; "b"; "c" ]; Lm_printf.close_box (); Lm_printf.print_newline ();
  Lm_printf.print_string "sep_list_box: "; Lm_printf.open_hovbox 0; P.print_sep_list_box "," Lm_printf.print_string [ "a"; "b"; "c" ]; Lm_printf.close_box (); Lm_printf.print_newline ();
  Lm_printf.print_string "sep_list of []: "; Lm_printf.open_hovbox 0; P.print_sep_list "," Lm_printf.print_string []; Lm_printf.close_box (); Lm_printf.print_newline ();
  Lm_printf.print_string "print_fst_symbol: "; P.print_fst_symbol (Lm_symbol.add "x", ()); Lm_printf.print_newline ()
