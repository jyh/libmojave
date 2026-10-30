(* Lm_thread_pool, threaded variant (the "threads" profile). The main
   thread holds the pool's lock; workers run jobs while it waits. Pinned:
   enabled; one job and wait; five jobs collected by repeated wait (every
   id once, every job run); waitpid on a job just created returns after
   the job has run; blocking_section and resume_inner_section return f's
   value and pass its exception on. *)
open Pin
module T = Lm_thread_pool

let () =
  line "enabled" "" (string_of_bool T.enabled);
  let ran = ref 0 in
  let id = T.create true (fun () -> incr ran) in
  let got = T.wait () in
  line "create a job, wait: wait's ids = [id]; the job ran" "" (Printf.sprintf "%b %d" (got = [ id ]) !ran);
  let ids = List.init 5 (fun _ -> T.create true (fun () -> incr ran)) in
  let rec collect acc = if List.length acc >= 5 then acc else collect (T.wait () @ acc) in
  let seen = collect [] in
  line "5 jobs, wait until all: each id once; all ran" "" (Printf.sprintf "%b %d"
      (List.sort compare seen = List.sort compare ids) !ran);
  let flag = ref false in
  let id = T.create false (fun () -> Thread.delay 0.05; flag := true) in
  T.waitpid id;
  line "waitpid on a job just created: it has run when waitpid returns" "" (string_of_bool !flag);
  line "blocking_section (fun x -> x + 1) 41" "" (guard (fun () -> string_of_int (T.blocking_section (fun x -> x + 1) 41)));
  line "blocking_section raising Exit" "" (guard (fun () -> T.blocking_section (fun () -> raise Exit) ()));
  line "blocking_section then resume_inner_section inside it" "" (guard (fun () ->
      string_of_int (T.blocking_section (fun () -> T.resume_inner_section (fun y -> y * 2) 21) ())))
