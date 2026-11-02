(* A forked child doing Lm_channel I/O before exec, in both thread
   profiles. The child writes through a pipe channel and exits 0; the
   parent reads the bytes back. The output must be the same in the
   default profile (null threads) and in --profile threads, where the
   main thread holds Lm_thread_pool's lock outside blocking_section and
   OCaml 5's mutexes refuse an unlock from a forked child unless the
   child re-creates the lock. Unix only (fork). *)
open Pin
module C = Lm_channel

let one i =
  let msg = Printf.sprintf "child %d: %s\n" i (String.make (i * 700) 'x') in
  let r, w = Unix.pipe () in
  match Unix.fork () with
  | 0 ->
    Unix.close r;
    let code =
      try
        let ch = C.create "pipe" C.PipeChannel C.OutChannel true (Some w) in
        C.output_string ch msg; C.flush ch;
        (* a read too: blocking_section on the input side *)
        let ch2 = C.of_string "abc" in
        ignore (C.input_char ch2);
        C.close ch; 0
      with _ -> 3 in
    Unix._exit code
  | pid ->
    Unix.close w;
    let ch = C.create "pipe" C.PipeChannel C.InChannel true (Some r) in
    let b = Buffer.create 64 in
    (try while true do Buffer.add_char b (C.input_char ch) done with End_of_file -> ());
    C.close ch;
    let _, st = Unix.waitpid [] pid in
    let st = match st with Unix.WEXITED n -> Printf.sprintf "exit %d" n | Unix.WSIGNALED n -> Printf.sprintf "signal %d" n | Unix.WSTOPPED n -> Printf.sprintf "stopped %d" n in
    line "fork" (string_of_int i) (Printf.sprintf "%s, %d of %d bytes back, same %b" st (Buffer.length b) (String.length msg) (Buffer.contents b = msg))

let () = List.iter one [ 0; 1; 6; 20 ]

(* After a fork, the parent's own I/O still works. *)
let () =
  let ch = C.of_string "after\n" in
  line "parent after the forks" "" (guard (fun () -> str (C.input_line ch)))
