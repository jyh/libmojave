(* Lm_flags: named bool and int flags with defaults, set directly or by
   "name=value,..." expressions. The two printers are pinned by their
   output, between "--- begin" and "--- end" lines (Lm_printf's formatter,
   flushed before each "end"). Names and sections come in StringTable's
   order: shorter first, then by bytes. *)
open Pin
open Lm_flags

let g f = guard f
let () =
  let fl = ref flags_empty in
  Lm_flags.std_flags := flags_empty;
  std_flags_register_list [ "debug", FlagBool false; "level", FlagInt 3 ];
  std_flags_register_list_help "opt" [ "fast", FlagBool true, "go fast" ];
  line "defaults: debug level fast" "" (g (fun () -> Printf.sprintf "%b %d %b" (std_flags_get_bool "debug") (std_flags_get_int "level") (std_flags_get_bool "fast")));
  std_flags_set_bool "debug" true;
  std_flags_set_int "level" 7;
  line "set debug true, level 7" "" (g (fun () -> Printf.sprintf "%b %d" (std_flags_get_bool "debug") (std_flags_get_int "level")));
  line "get_bool undefined" "" (g (fun () -> string_of_bool (std_flags_get_bool "nope")));
  line "set_bool undefined" "" (g (fun () -> std_flags_set_bool "nope" true; "set"));
  line "get_int on a bool flag" "" (g (fun () -> string_of_int (std_flags_get_int "debug")));
  std_flags_set_expr "debug=no,level=12,fast=YES";
  line "set_expr \"debug=no,level=12,fast=YES\"" "" (g (fun () -> Printf.sprintf "%b %d %b" (std_flags_get_bool "debug") (std_flags_get_int "level") (std_flags_get_bool "fast")));
  line "set_expr \"level=x\"" "" (g (fun () -> std_flags_set_expr "level=x"; "set"));
  line "set_expr \"level\" (no =)" "" (g (fun () -> std_flags_set_expr "level"; "set"));
  line "set_expr \"nope=3\" (undefined flag)" "" (g (fun () -> std_flags_set_expr "nope=3"; "set"));
  line "set_expr \"nope=true\" (undefined flag)" "" (g (fun () -> std_flags_set_expr "nope=true"; "set"));
  (* types are not checked when setting *)
  line "set_int on a bool flag" "" (g (fun () -> std_flags_set_int "fast" 5; "set"));
  line "  then get_bool fast" "" (g (fun () -> string_of_bool (std_flags_get_bool "fast")));
  line "  then get_int fast" "" (g (fun () -> string_of_int (std_flags_get_int "fast")));
  (* the functional interface *)
  fl := flags_empty;
  line "flags_empty: get_bool x" "" (g (fun () -> string_of_bool (flags_get_bool !fl "x")));
  line "flags_empty: set_bool x" "" (g (fun () -> ignore (flags_set_bool !fl "x" true); "set"));
  (* the error branches the lines above do not reach *)
  Lm_flags.std_flags := flags_empty;
  std_flags_register_list [ "b", FlagBool true; "n", FlagInt 4 ];
  line "set_bool on an int flag" "" (g (fun () -> std_flags_set_bool "n" true; "set"));
  line "get_bool on an int flag" "" (g (fun () -> string_of_bool (std_flags_get_bool "n")));
  line "get_int undefined" "" (g (fun () -> string_of_int (std_flags_get_int "nope")));
  line "set_expr \"b=f\" then get_bool b" "" (g (fun () -> std_flags_set_expr "b=f"; string_of_bool (std_flags_get_bool "b")));
  line "set_expr \"b=t\" then get_bool b" "" (g (fun () -> std_flags_set_expr "b=t"; string_of_bool (std_flags_get_bool "b")));
  (* help sections and the printers *)
  std_flags_register_list_help "opt" [ "fast", FlagBool false, "go fast when the input is large enough to matter"; "depth", FlagInt 2, "how deep the search goes before it gives up; a larger depth finds more and takes longer, and past twenty it rarely pays" ];
  std_flags_help_section_text "opt" "Optimizer flags.";
  std_flags_help_section_text "new" "A section with no flags.";
  std_flags_set_int "depth" 9;
  let show name f =
    Printf.printf "--- begin %s\n%!" name;
    f ();
    Lm_printf.print_flush ();
    Printf.printf "--- end %s\n%!" name in
  show "print_std_flag_defaults" print_std_flag_defaults;
  show "print_std_flag_values" print_std_flag_values
