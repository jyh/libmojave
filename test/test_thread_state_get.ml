(* Lm_thread.State.get, which no test called: it returns an entry's value
   only while this thread holds the entry's lock (read or write), and
   raises Invalid_argument otherwise. Also the same calls with debug_lock
   on, which reaches print_thread_id (its messages go to stderr). Both
   thread profiles. *)
open Pin
module S = Lm_thread.State

let get e = guard (fun () -> string_of_int (S.get e))

let run tag =
  let e = S.shared_val "e" 41 in
  let p = S.private_val "p" 7 (fun x -> x) in
  line (tag ^ "get, no lock") "" (get e);
  line (tag ^ "get inside read") "" (S.read e (fun _ -> get e));
  line (tag ^ "get inside write") "" (S.write e (fun _ -> get e));
  line (tag ^ "get inside unlock inside read") "" (S.read e (fun _ -> S.unlock e (fun () -> get e)));
  line (tag ^ "get inside read, after an inner unlock") "" (S.read e (fun _ -> ignore (S.unlock e (fun () -> ())); get e));
  line (tag ^ "get after read returned") "" (ignore (S.read e (fun _ -> ())); get e);
  line (tag ^ "get of a private value inside read") "" (S.read p (fun _ -> guard (fun () -> string_of_int (S.get p))))

(* An exception escaping a run is printed here, so the expected file shows it. *)
let () =
  (try run "" with e -> print_endline ("EXN " ^ Printexc.to_string e));
  Lm_thread.debug_lock := true;
  (try run "debug_lock: " with e -> print_endline ("EXN " ^ Printexc.to_string e));
  Lm_thread.debug_lock := false
