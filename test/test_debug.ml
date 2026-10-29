(* Lm_debug: the registry of named debug flags, and timing_wrap. Only the
   flags this test creates (prefix t_) and "load" (created by Lm_debug
   itself) are listed, since other linked modules may register more.
   Not pinned: set_debug_flags with an unknown name prints usage and
   calls exit 1. *)
open Pin
module D = Lm_debug

let info name desc v = { D.debug_name = name; D.debug_description = desc; D.debug_value = v }
let show { D.debug_name = n; D.debug_description = d; D.debug_value = v } = Printf.sprintf "%s: %s: %b" n d v
let ours () =
  List.filter (fun i -> i.D.debug_name = "load" || String.length i.D.debug_name > 2 && String.sub i.D.debug_name 0 2 = "t_")
    (Array.to_list (D.debuggers ()))

let () =
  line "debug_enabled" "" (string_of_bool D.debug_enabled);
  line "debuggers (ours, at start)" "" (list show (ours ()));
  let a = D.create_debug (info "t_a" "flag a" false) in
  let b = D.create_debug (info "t_b" "flag b" true) in
  line "create t_a false, t_b true; debug a, debug b" "" (Printf.sprintf "%b %b" (D.debug a) (D.debug b));
  line "create t_a again, same description: same ref" "" (string_of_bool (D.create_debug (info "t_a" "flag a" true) == a));
  line "  and its value" "" (string_of_bool !a);
  line "create t_a again, other description" "" (guard (fun () -> ignore (D.create_debug (info "t_a" "other" false)); "ok"));
  line "load_debug t_b == b" "" (guard (fun () -> string_of_bool (D.load_debug "t_b" == b)));
  line "load_debug t_none" "" (guard (fun () -> string_of_bool !(D.load_debug "t_none")));
  D.set_debug "t_a" true;
  line "set_debug t_a true; !a" "" (string_of_bool !a);
  line "set_debug t_none" "" (guard (fun () -> D.set_debug "t_none" true; "ok"));
  line "get_debug t_b" "" (guard (fun () -> show (D.get_debug "t_b")));
  line "get_debug t_none" "" (guard (fun () -> show (D.get_debug "t_none")));
  (* a flag set before it is created (from a command line, say) *)
  D.set_possible_debug "t_c" true;
  line "set_possible_debug t_c true; get_debug t_c" "" (guard (fun () -> show (D.get_debug "t_c")));
  let c = D.create_debug (info "t_c" "flag c" false) in
  line "then create t_c false: value" "" (string_of_bool !c);
  line "  get_debug t_c" "" (guard (fun () -> show (D.get_debug "t_c")));
  D.set_possible_debug "t_b" false;
  line "set_possible_debug t_b false (exists): !b" "" (string_of_bool !b);
  (* levels *)
  let lvl = ref 2 in
  line "debug_level (ref 2) 1, 2, 3" "" (Printf.sprintf "%b %b %b" (D.debug_level lvl 1) (D.debug_level lvl 2) (D.debug_level lvl 3));
  (* colon-separated flags *)
  D.set_debug "t_a" false; D.set_debug "t_b" false; D.set_debug "t_c" false;
  D.set_debug_flags "t_a::t_c:";
  line "set_debug_flags \"t_a::t_c:\"" "" (Printf.sprintf "a=%b b=%b c=%b" !a !b !c);
  line "debuggers (ours, at end)" "" (list show (ours ()));
  (* timing_wrap returns the value or re-raises; the timings go to stderr at exit *)
  line "timing_wrap t succ 41" "" (string_of_int (D.timing_wrap "t" succ 41));
  line "timing_wrap t raises" "" (guard (fun () -> string_of_int (D.timing_wrap "t" (fun () -> failwith "boom") ())))
