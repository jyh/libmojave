(* Lm_thread_event under the default profile, where Lm_thread's threads
   are null: a sync that would block cannot wait for a peer, and raises.
   Each case after the first failure checks that the failed sync left
   nothing behind for a later peer to complete against.
   (The threads profile is pinned in test_thread_event.ml.) *)
open Pin
module E = Lm_thread_event

let () =
  line "Thread.enabled" "" (string_of_bool Lm_thread.Thread.enabled);
  let c : int E.channel = E.new_channel () in
  line "sync (always 1)" "" (string_of_int (E.sync 0 (E.always 1)));
  line "sync (receive c), no sender" "" (guard (fun () -> string_of_int (E.sync 0 (E.receive c))));
  line "sync (send c 1), no receiver" "" (guard (fun () -> E.sync 0 (E.send c 1); "sent"));
  line "poll (receive c) after both" "" (opt string_of_int (E.poll (E.receive c)));
  (* a failed sync must not leave a live entry for a later peer *)
  let d : int E.channel = E.new_channel () in
  line "sync (send d 2), no receiver" "" (guard (fun () -> E.sync 0 (E.send d 2); "sent"));
  line "  then poll (receive d)" "" (opt string_of_int (E.poll (E.receive d)))
