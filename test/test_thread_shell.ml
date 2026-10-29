(* Lm_thread_shell: named jobs, each with its own Lm_thread.State. Runs in
   every profile (null threads by default, system threads under
   "threads"); nothing here depends on scheduling. *)
open Pin
module S = Lm_thread_shell

let pid p = let s, i = S.dest_pid p in Printf.sprintf "(%s,%d)" s i

let () =
  line "get_pid (root)" "" (pid (S.get_pid ()));
  line "get_pids (root is hidden)" "" (list pid (S.get_pids ()));
  let a1 = S.create "a" S.VisibleJob in
  let a2 = S.create "a" S.VisibleJob in
  let h = S.create "h" S.HiddenJob in
  let b = S.create "b" S.VisibleJob in
  line "create a, a, h (hidden), b" "" (list pid [ a1; a2; h; b ]);
  line "get_pids" "" (list pid (S.get_pids ()));
  line "get_pid after create (unchanged)" "" (pid (S.get_pid ()));
  line "with_pid a2 get_pid" "" (pid (S.with_pid a2 S.get_pid ()));
  line "make_pid a 2" "" (guard (fun () -> pid (S.make_pid "a" 2)));
  line "make_pid a 9" "" (guard (fun () -> pid (S.make_pid "a" 9)));
  line "make_pid id 1 (root)" "" (guard (fun () -> pid (S.make_pid "id" 1)));
  line "create_or_find a 1 (exists)" "" (pid (S.create_or_find "a" 1 S.VisibleJob));
  line "create_or_find c 5 (new)" "" (pid (S.create_or_find "c" 5 S.VisibleJob));
  line "create_or_find c 5 again" "" (pid (S.create_or_find "c" 5 S.VisibleJob));
  S.set_pid b;
  line "set_pid b; get_pid" "" (pid (S.get_pid ()));
  line "with_current get_pid" "" (pid (S.with_current S.get_pid ()));
  line "with_pid a1 get_pid (from b)" "" (pid (S.with_pid a1 S.get_pid ()));
  line "get_pid after with_pid" "" (pid (S.get_pid ()));
  line "set_pid to an unknown pid" "" (guard (fun () -> S.set_pid (S.make_pid "zz" 1); "set"));
  line "get_pids at the end" "" (list pid (S.get_pids ()))
