open Pin
module R = Lm_rprintf

let pp_pair out (a, b) = R.fprintf out "<%d,%s>" a b

let () =
  (* Lm_ref_util *)
  let r = ref [] in
  Lm_ref_util.push 1 r;
  Lm_ref_util.push 2 r;
  line "Lm_ref_util push 1; push 2" "" (list string_of_int !r);
  line "Lm_ref_util pop" "" (guard (fun () -> string_of_int (Lm_ref_util.pop r)));
  line "Lm_ref_util pop" "" (guard (fun () -> string_of_int (Lm_ref_util.pop r)));
  line "Lm_ref_util pop (empty)" "" (guard (fun () -> string_of_int (Lm_ref_util.pop r)));
  (* formatted output to strings and buffers *)
  (* No %b here: Lm_make_printf hands "%b" and the string "true"/"false" to
     C's snprintf, where glibc reads %b as a binary unsigned int (undefined
     behaviour, a different number each run) and macOS prints a literal b. *)
  line "sprintf %d %s %c" "" (guard (fun () -> str (R.sprintf "%d %s %c" (-3) "x y" 'z')));
  line "sprintf %5d|%-5d|%05d" "" (guard (fun () -> str (R.sprintf "%5d|%-5d|%05d" 42 42 42)));
  line "sprintf %x %o %X" "" (guard (fun () -> str (R.sprintf "%x %o %X" 255 8 255)));
  line "sprintf %S" "" (guard (fun () -> str (R.sprintf "%S" "a\"b\n")));
  line "sprintf %C" "" (guard (fun () -> str (R.sprintf "%C" '\'')));
  line "sprintf %B" "" (guard (fun () -> str (R.sprintf "%B" true)));
  line "sprintf %f %.2f %e %g" "" (guard (fun () -> str (R.sprintf "%f %.2f %e %g" 1.5 3.14159 12345.678 0.0001)));
  line "sprintf %%" "" (guard (fun () -> str (R.sprintf "100%%")));
  line "Printf.sprintf (reference) %d %s %c %b %S %C" "" (str (Printf.sprintf "%d %s %c %b %S %C" (-3) "x y" 'z' true "a\"b\n" '\''));
  line "sprintf %a" "" (guard (fun () -> str (R.sprintf "pair=%a" (fun () (a, b) -> Printf.sprintf "<%d,%s>" a b) (1, "x"))));
  line "bprintf %a" "" (guard (fun () ->
      let b = Buffer.create 8 in R.bprintf b "[%a]" pp_pair (2, "y"); str (Buffer.contents b)));
  (* the string channel *)
  R.fprintf R.stdstr "one %d" 1;
  R.output_string R.stdstr " two";
  R.output_char R.stdstr '!';
  R.output_int R.stdstr 3;
  line "stdstr, flush_stdstr" "" (str (R.flush_stdstr ()));
  line "flush_stdstr again (emptied)" "" (str (R.flush_stdstr ()));
  (* lists *)
  R.print_string_list R.stdstr [ "a"; "b"; "c" ];
  line "print_string_list" "[a; b; c]" (str (R.flush_stdstr ()));
  R.print_int_list R.stdstr [ 1; 2; 3 ];
  line "print_int_list" "[1; 2; 3]" (str (R.flush_stdstr ()));
  R.print_any_list pp_pair R.stdstr [ (1, "a"); (2, "b") ];
  line "print_any_list pp_pair" "" (str (R.flush_stdstr ()));
  R.print_int_list R.stdstr [];
  line "print_int_list" "[]" (str (R.flush_stdstr ()));
  (* an Lm_rformat buffer written to a channel *)
  let fb = Lm_rformat.new_buffer () in
  Lm_rformat.format_szone fb;
  Lm_rformat.format_string fb "from";
  Lm_rformat.format_space fb;
  Lm_rformat.format_string fb "rformat";
  Lm_rformat.format_ezone fb;
  R.output_rbuffer R.stdstr fb;
  line "output_rbuffer" "" (str (R.flush_stdstr ()));
  (* boxes through a buffer formatter *)
  let b = Buffer.create 32 in
  let f = R.formatter_of_buffer b in
  R.pp_set_margin f 12;
  R.pp_open_hovbox f 2;
  List.iter (fun w -> R.pp_print_string f w; R.pp_print_space f ()) [ "alpha"; "beta"; "gamma"; "delta" ];
  R.pp_close_box f ();
  R.pp_print_flush f ();
  line "hovbox margin 12" "" (str (Buffer.contents b));
  line "pp_get_margin" "" (string_of_int (R.pp_get_margin f ()))
