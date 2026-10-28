(* Runs only under the "threads" profile, where Lm_thread uses system
   threads. Records results that do not depend on scheduling. *)
open Pin
module T = Lm_thread

let () =
  line "Thread.enabled" "" (string_of_bool T.Thread.enabled);
  let m = T.Mutex.create "counter" in
  T.Mutex.lock m;
  line "Mutex.try_lock while held" "" (string_of_bool (T.Mutex.try_lock m));
  T.Mutex.unlock m;
  line "Mutex.try_lock when free" "" (string_of_bool (T.Mutex.try_lock m));
  T.Mutex.unlock m;
  let counter = ref 0 and ids = ref [] in
  let work () =
    T.Mutex.lock m;
    ids := T.Thread.id (T.Thread.self ()) :: !ids;
    T.Mutex.unlock m;
    for _ = 1 to 1000 do
      T.Mutex.lock m; incr counter; T.Mutex.unlock m
    done in
  let ts = List.init 4 (fun _ -> T.Thread.create work ()) in
  List.iter T.Thread.join ts;
  line "4 threads x 1000 locked increments" "" (string_of_int !counter);
  line "distinct thread ids" "" (string_of_int (List.length (List.sort_uniq compare !ids)))
