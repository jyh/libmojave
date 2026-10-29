(* Lm_printf's own wrappers that no other test calls (the rest of
   Lm_printf is OCaml's Format, included): out_channel_of_formatter,
   pp_print_any_list, eflush, and the global
   set/get_all_formatter_output_functions (restored afterwards).
   extra_formatting, a debug logger writing a log and a pid file under a
   lock, is not called. The line breaks below, with no box open, are
   OCaml Format's own: plain Format prints the same. *)
open Pin
module P = Lm_printf

let () =
  let b = Buffer.create 16 in
  let f = Format.formatter_of_buffer b in
  let out = P.out_channel_of_formatter f in
  P.pp_print_any_list P.pp_print_int out [ 1; 2; 3 ];
  P.output_string out "|";
  P.pp_print_any_list P.pp_print_int out [];
  P.output_string out "|";
  P.pp_print_any_list P.pp_print_string out [ "only" ];
  P.eflush out;
  line "pp_print_any_list [1;2;3] | [] | [only], then eflush" "" (str (Buffer.contents b));
  let saved = P.get_all_formatter_output_functions () in
  let got = Buffer.create 16 in
  P.set_all_formatter_output_functions (fun s o l -> Buffer.add_substring got s o l) (fun () -> ())
    (fun () -> Buffer.add_string got "<nl>") (fun n -> Buffer.add_string got (Printf.sprintf "<%d>" n));
  P.printf "a@ b@\n%!";
  let (o, fl, nl, sp) = saved in
  P.set_all_formatter_output_functions o fl nl sp;
  line "set_all_formatter_output_functions: what printf \"a@ b@\\n\" sends" "" (str (Buffer.contents got))
