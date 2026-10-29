(* C stubs that allocate while building their result, called many times
   under a small minor heap with collections between, so a stub that
   breaks the GC's rules has many chances to show it. Each line is a
   count, the same on every platform.
   - lm_uname (Lm_uname at start-up): a 5-field record of strings;
   - ml_getrusage (Lm_rusage.getrusage): 16 fields, two of them floats;
   - caml_term_size (Lm_termsize at start-up, on stdout and stderr): on
     a descriptor that is not a terminal it fails, here a pipe. *)
open Pin

(* The layout of Lm_uname's (unexported) record: five strings. *)
type uname = { sysname : string; nodename : string; release : string; version : string; machine : string }
external lm_uname : unit -> uname = "lm_uname"
external term_size : Unix.file_descr -> int * int = "caml_term_size"

let () =
  Gc.set { (Gc.get ()) with Gc.minor_heap_size = 256 };
  let first = lm_uname () in
  let same = ref 0 in
  for i = 1 to 20000 do
    let u = lm_uname () in
    if u = first then incr same;
    if i mod 97 = 0 then Gc.minor ()
  done;
  line "lm_uname" "20000 calls" (Printf.sprintf "%d equal to the first" !same);
  let ok = ref 0 in
  for i = 1 to 200 do
    let r = Lm_rusage.getrusage () in
    if r.Lm_rusage.ru_utime >= 0.0 && r.Lm_rusage.ru_stime >= 0.0 then incr ok;
    if i mod 7 = 0 then Gc.minor ()
  done;
  line "getrusage" "200 calls" (Printf.sprintf "%d with non-negative times" !ok);
  let r, w = Unix.pipe () in
  let failed = ref 0 and other = ref "" in
  for i = 1 to 20000 do
    (match term_size r with
     | _ -> other := "returned a size"
     | exception Failure _ -> incr failed
     | exception e -> other := Printexc.to_string e);
    if i mod 97 = 0 then Gc.minor ()
  done;
  Unix.close r; Unix.close w;
  line "term_size on a pipe" "20000 calls"
    (Printf.sprintf "%d raised Failure%s" !failed (if !other = "" then "" else "; also " ^ !other));
  Gc.compact ();
  line "after Gc.compact" "" "alive"
