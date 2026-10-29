(* Lm_flags: named bool and int flags with defaults, set directly or by
   "name=value,..." expressions. The printing functions are not pinned. *)
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
  line "flags_empty: set_bool x" "" (g (fun () -> ignore (flags_set_bool !fl "x" true); "set"))
