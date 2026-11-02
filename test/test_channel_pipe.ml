(* Lm_channel.select on pipe channels. On Windows, create asks the OS
   whether a descriptor is a pipe (GetNamedPipeInfo) and select then takes
   its pipe branch: PeekNamedPipe for readers, an output thread for a writer
   holding buffered output, and a 50 ms poll until the timeout. On Unix
   every descriptor is selected as a socket, so the same calls take the
   socket branch. Each platform has its own expected file; the Windows one
   was written from the code before any Windows run (a prediction), and the
   Windows CI lanes (the default, no-threads profile) measure it. In that
   profile the output thread cannot start: Lm_thread_pool_null.create
   raises. A watchdog ends the test after 10 s, so a hang fails loudly. *)
open Pin
module C = Lm_channel

let () =
  ignore (Thread.create (fun () -> Thread.delay 10.0; print_endline "WATCHDOG: 10 s, stopping"; exit 3) ())

let names l = list (fun c -> C.name c) l
let sel r w e t =
  let r, w, e = C.select r w e t in
  Printf.sprintf "%s %s %s" (names r) (names w) (names e)
let timed f =
  let t0 = Unix.gettimeofday () in
  let x = f () in
  x, Unix.gettimeofday () -. t0

let () =
  let rfd, wfd = Unix.pipe () in
  let r = C.create "r" C.PipeChannel C.InChannel true (Some rfd) in
  let w = C.create "w" C.PipeChannel C.OutChannel true (Some wfd) in
  line "select [r] [] [] 0, nothing written" "" (guard (fun () -> sel [ r ] [] [] 0.0));
  ignore (Unix.write_substring wfd "x" 0 1);
  line "select [r] [] [] 0, \"x\" in the pipe" "" (guard (fun () -> sel [ r ] [] [] 0.0));
  line "  input_char r" "" (Printf.sprintf "%C" (C.input_char r));
  let s, dt = timed (fun () -> guard (fun () -> sel [ r ] [] [] 0.3)) in
  line "select [r] [] [] 0.3, nothing written" "" (Printf.sprintf "%s, waited >= 0.25 s: %b" s (dt >= 0.25));
  let writer = Thread.create (fun () -> Thread.delay 0.3; ignore (Unix.write_substring wfd "y" 0 1)) () in
  let s, dt = timed (fun () -> guard (fun () -> sel [ r ] [] [] (-1.0))) in
  Thread.join writer;
  line "select [r] [] [] (-1), \"y\" written after 0.3 s" "" (Printf.sprintf "%s, waited >= 0.25 s: %b" s (dt >= 0.25));
  line "  input_char r" "" (Printf.sprintf "%C" (C.input_char r));
  line "select [] [w] [] 0, w's buffer empty" "" (guard (fun () -> sel [] [ w ] [] 0.0));
  C.output_string w "zz";
  line "select [] [w] [] 0, \"zz\" buffered in w" "" (guard (fun () -> sel [] [ w ] [] 0.0));
  C.flush w;
  line "  after flush w, read the pipe" "" (let b = Bytes.create 2 in str (Bytes.sub_string b 0 (Unix.read rfd b 0 2)));
  C.close w; C.close r
