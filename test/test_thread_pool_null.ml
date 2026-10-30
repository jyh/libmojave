(* Lm_thread_pool, null variant (the default profile): not enabled;
   create, wait and waitpid raise; the sections just apply f. *)
open Pin
module T = Lm_thread_pool

let () =
  line "enabled" "" (string_of_bool T.enabled);
  line "create" "" (guard (fun () -> string_of_int (T.create true (fun () -> ()))));
  line "wait" "" (guard (fun () -> list string_of_int (T.wait ())));
  line "waitpid 1" "" (guard (fun () -> T.waitpid 1; "returned"));
  line "blocking_section (fun x -> x + 1) 41; resume_inner_section" "" (guard (fun () ->
      Printf.sprintf "%d %d" (T.blocking_section (fun x -> x + 1) 41) (T.resume_inner_section (fun x -> x * 2) 21)))
