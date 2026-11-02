(* Lm_thread.State with real threads (--profile threads only): the parts
   the 2026-11-02 threads coverage run did not reach.

   - private_val: a thread that enters a new state gets the fork function
     applied to its parent thread's value (get_parent_info); the main
     thread in a new state gets the fork of the default; state 1 keeps its
     own value; shared_val is one value everywhere.
   - read/write exclusion under contention: 8 threads, random reads and
     writes on one entry, a writer never overlapping a reader or another
     writer (counted with a separate mutex).
   - unlock inside a write lets another thread read.
   - readers queued behind a writer all finish once it releases.
   - writer precedence: a reader arriving after a queued writer waits for it.
   A watchdog ends the run with "HUNG" after 60 s, so a deadlock is a
   red line rather than a stuck job. *)
open Pin
module T = Lm_thread.Thread
module S = Lm_thread.State

let () =
  ignore (Thread.create (fun () -> Unix.sleepf 60.; print_endline "HUNG"; exit 2) ())

(* private_val and shared_val. *)
let () =
  let e = S.private_val "x" (ref 0) (fun r -> ref (!r + 100)) in
  let y = S.shared_val "y" (ref 0) in
  S.write e (fun r -> r := 5);
  let in_child = ref (-1) in
  let t = T.create (fun () ->
      S.set (S.create ());
      in_child := S.read e (fun r -> !r);
      S.write y (fun r -> r := 7)) () in
  T.join t;
  line "private_val" "a child thread in a new state" (string_of_int !in_child ^ " (fork of the parent's 5: 105)");
  line "private_val" "main thread, state 1" (string_of_int (S.read e (fun r -> !r)) ^ " (5)");
  let s2 = S.create () in
  (* recorded as it is: the fork of state 1's value, not of the default *)
  line "private_val" "main thread in a new state" (string_of_int (S.with_state s2 (fun () -> S.read e (fun r -> !r)) ()));
  line "shared_val" "written 7 by the child" (string_of_int (S.read y (fun r -> !r)))

(* Exclusion under contention. *)
let () =
  let e = S.shared_val "z" (ref 0) in
  let m = Mutex.create () in
  let readers = ref 0 and writers = ref 0 and reads = ref 0 and writes = ref 0 in
  let ww = ref 0 and wr = ref 0 and rw = ref 0 in
  let crit f = Mutex.lock m; let x = f () in Mutex.unlock m; x in
  let worker seed () =
    let st = ref seed in
    let rnd n = st := (!st * 1103515245 + 12345) land 0x3fffffff; (!st lsr 4) mod n in
    for _ = 1 to 300 do
      if rnd 3 = 0 then
        S.write e (fun r ->
            crit (fun () -> if !writers <> 0 then incr ww; if !readers <> 0 then incr wr; incr writers; incr writes);
            incr r; Thread.yield ();
            crit (fun () -> decr writers))
      else
        S.read e (fun _ ->
            crit (fun () -> if !writers <> 0 then incr rw; incr readers; incr reads);
            Thread.yield ();
            crit (fun () -> decr readers))
    done in
  let ts = List.init 8 (fun i -> T.create (worker (i + 1)) ()) in
  List.iter T.join ts;
  (* the counts vary from run to run, so the line says only none or some *)
  let some n = if n = 0 then "none" else "some" in
  line "exclusion" "8 threads x 300 steps"
    (Printf.sprintf "a writer entering beside a writer: %s; beside readers: %s; a reader entering beside a writer: %s"
       (some !ww) (some !wr) (some !rw));
  Printf.eprintf "exclusion counts: ww %d wr %d rw %d in %d reads %d writes\n%!" !ww !wr !rw !reads !writes

(* unlock inside a write. *)
let () =
  let e = S.shared_val "u" (ref 0) in
  let r = S.write e (fun _ ->
      S.unlock e (fun () ->
          let seen = ref (-1) in
          let t = T.create (fun () -> seen := S.read e (fun r -> !r)) () in
          T.join t; !seen)) in
  line "unlock" "inside a write, another thread reads" (string_of_int r)

(* Readers queued behind a writer. *)
let () =
  let e = S.shared_val "q" (ref 0) in
  let done_ = ref 0 and m = Mutex.create () in
  let started = ref false in
  let w = T.create (fun () ->
      S.write e (fun r -> started := true; Unix.sleepf 0.2; r := 1)) () in
  while not !started do Thread.yield () done;
  let rs = List.init 4 (fun _ -> T.create (fun () ->
      let v = S.read e (fun r -> !r) in
      Mutex.lock m; if v = 1 then incr done_; Mutex.unlock m) ()) in
  T.join w; List.iter T.join rs;
  line "queued readers" "4 behind a writer" (Printf.sprintf "%d finished, after the write" !done_)

(* Writer precedence: A holds a read lock; W asks to write and queues; R
   asks to read after W, and must wait behind W, not join A. The order the
   three enter is recorded. Timing (A holds 0.4 s, W asks at 0.1 s, R at
   0.2 s) only arranges the order of arrival. *)
let () =
  let e = S.shared_val "f" (ref 0) in
  let order = ref [] and m = Mutex.create () in
  let enter name = Mutex.lock m; order := name :: !order; Mutex.unlock m in
  let a = T.create (fun () -> S.read e (fun _ -> enter "A"; Unix.sleepf 0.4)) () in
  Unix.sleepf 0.1;
  let w = T.create (fun () -> S.write e (fun _ -> enter "W")) () in
  Unix.sleepf 0.1;
  let r = T.create (fun () -> S.read e (fun _ -> enter "R")) () in
  List.iter T.join [ a; w; r ];
  line "writer precedence" "A reading, then W asks to write, then R to read" (String.concat " " (List.rev !order));
  exit 0
