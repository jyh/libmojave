(* Lm_channel.select on socket channels (Unix: a socketpair; on Unix every
   descriptor is selected as a socket). Recorded: what is ready at once,
   input already buffered in the channel, a write channel's buffered
   output (select flushes it), a timeout that expires, and a negative
   timeout — Lm_channel's pipe branch reads that as "no limit" (it polls
   until something is ready), and this records what the socket branch
   does with it, with a peer that writes after 0.3 s. Times are printed as
   coarse comparisons only. Not on Windows (no socketpair; pipes are a
   separate branch there). *)
open Pin
module C = Lm_channel

let pair () =
  let a, b = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  C.create "a" C.SocketChannel C.InOutChannel true (Some a),
  C.create "b" C.SocketChannel C.InOutChannel true (Some b)

let names l = list (fun c -> C.name c) l
let sel r w e t =
  let r, w, e = C.select r w e t in
  Printf.sprintf "%s %s %s" (names r) (names w) (names e)
let timed f =
  let t0 = Unix.gettimeofday () in
  let x = f () in
  x, Unix.gettimeofday () -. t0

let () =
  let a, b = pair () in
  line "select [a] [] [] 0, nothing sent" "" (guard (fun () -> sel [ a ] [] [] 0.0));
  C.output_string b "xy"; C.flush b;
  line "select [a] [] [] 0, b sent \"xy\"" "" (guard (fun () -> sel [ a ] [] [] 0.0));
  line "  input_char a" "" (Printf.sprintf "%C" (C.input_char a));
  (* "y" is in a's buffer now, not in the socket *)
  line "select [a] [] [] 0, \"y\" buffered in a" "" (guard (fun () -> sel [ a ] [] [] 0.0));
  line "  input_char a" "" (Printf.sprintf "%C" (C.input_char a));
  line "select [] [b] [] 0, b's buffer empty" "" (guard (fun () -> sel [] [ b ] [] 0.0));
  C.output_string b "zz";
  line "select [] [b] [] 0, \"zz\" buffered in b" "" (guard (fun () -> sel [] [ b ] [] 0.0));
  line "  select [a] [] [] 0 (did select flush b?)" "" (guard (fun () -> sel [ a ] [] [] 0.0));
  line "  read a" "" (let buf = Bytes.create 2 in let n = C.read a buf 0 2 in Bytes.sub_string buf 0 n);
  let r, dt = timed (fun () -> guard (fun () -> sel [ a ] [] [] 0.3)) in
  line "select [a] [] [] 0.3, nothing sent" "" (Printf.sprintf "%s, waited >= 0.25 s: %b" r (dt >= 0.25));
  (* a negative timeout, the peer writing after 0.3 s *)
  (match Unix.fork () with
   | 0 -> Unix.sleepf 0.3; C.output_string b "late"; C.flush b; Unix._exit 0
   | pid ->
       let r, dt = timed (fun () -> guard (fun () -> sel [ a ] [] [] (-1.0))) in
       line "select [a] [] [] (-1), b writes after 0.3 s" ""
         (Printf.sprintf "%s, returned within 0.1 s: %b" r (dt < 0.1));
       ignore (Unix.waitpid [] pid))
