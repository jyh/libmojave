(* Lm_thread_event, CML-style channels and events. Runs only under the
   "threads" profile. Every line is independent of scheduling: where a
   case needs a thread to be BLOCKED in sync, the main thread retries a
   poll that can only succeed against a queued (that is, blocked) peer. *)
open Pin
module T = Lm_thread
module E = Lm_thread_event

(* Retry a poll until it succeeds; poll performs the communication. *)
let until_poll ev =
  let rec loop n =
    if n = 0 then "TIMEOUT"
    else
      match E.poll ev with
        Some _ -> "ok"
      | None -> Unix.sleepf 0.001; loop (n - 1) in
  loop 10000

(* Run [f] in a thread; return a function that joins it and gives the result. *)
let spawn f =
  let r = ref "unset" in
  let t = T.Thread.create (fun () -> r := guard f) () in
  fun () -> T.Thread.join t; !r

(* Count the lines [f] prints on Lm_printf's stderr formatter. Captured
   through the formatter, not a file descriptor: on the MSVC runner the
   test process has no valid stderr to dup. *)
let stderr_lines f =
  let ppf = Lm_printf.err_formatter in
  let out, flush = Lm_printf.pp_get_formatter_output_functions ppf () in
  let buf = Buffer.create 64 in
  Lm_printf.pp_set_formatter_output_functions ppf (Buffer.add_substring buf) ignore;
  f ();
  Lm_printf.pp_print_flush ppf ();
  Lm_printf.pp_set_formatter_output_functions ppf out flush;
  String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 0 (Buffer.contents buf)

let () =
  let c : int E.channel = E.new_channel () in
  line "poll (receive c), nothing sent" "" (opt string_of_int (E.poll (E.receive c)));
  line "poll (always 3)" "" (opt string_of_int (E.poll (E.always 3)));
  line "sync (always a)" "" (str (E.sync 0 (E.always "a")));
  line "sync (wrap (always 2) (x * 10))" "" (string_of_int (E.sync 0 (E.wrap (E.always 2) (fun x -> x * 10))));
  line "sync (choose [receive c; always 5])" "" (string_of_int (E.sync 0 (E.choose [ E.receive c; E.always 5 ])));
  line "select [receive c; always 6]" "" (string_of_int (E.select 0 [ E.receive c; E.always 6 ]));

  (* one thread sends, the other receives; either may block first *)
  let join = spawn (fun () -> E.sync 0 (E.send c 7); "sent") in
  line "receive from a sending thread" "" (string_of_int (E.sync 0 (E.receive c)));
  line "  the sender's sync" "" (join ());

  (* a blocked receiver, then a poll that hands it a value *)
  let join = spawn (fun () -> string_of_int (E.sync 0 (E.receive c))) in
  line "poll (send c 8) to a blocked receiver" "" (until_poll (E.send c 8));
  line "  the receiver's sync" "" (join ());

  (* sequence and sequence_final *)
  let s = E.sequence [ E.always 1; E.always 2 ] in
  let a = E.sync 0 s in
  let b = E.sync 0 s in
  line "sequence [always 1; always 2]: sync, sync, poll" ""
    (Printf.sprintf "%d %d %s" a b (opt string_of_int (E.poll s)));
  let s = E.sequence_final [ E.always 1; E.always 2 ] in
  let a = E.sync 0 s in
  let b = E.sync 0 s in
  let d = E.sync 0 s in
  line "sequence_final [always 1; always 2]: sync x3" "" (Printf.sprintf "%d %d %d" a b d);

  (* a blocked choose between a send arm and a receive arm *)
  let choose_case arms complete =
    let c1 : unit E.channel = E.new_channel () and c2 : string E.channel = E.new_channel () in
    let send_arm = E.wrap (E.send c1 ()) (fun () -> "sent") in
    let recv_arm = E.wrap (E.receive c2) (fun s -> "received " ^ s) in
    let join = spawn (fun () -> E.sync 0 (E.choose (arms send_arm recv_arm))) in
    let polled = complete c1 c2 in
    polled ^ ", the chooser's sync: " ^ join () in
  let by_receiver c1 _ = until_poll (E.receive c1) in
  let by_sender _ c2 = until_poll (E.send c2 "hello") in
  line "choose [recv; send] blocked, completed by a sender" "" (choose_case (fun s r -> [ r; s ]) by_sender);
  line "choose [send; recv] blocked, completed by a sender" "" (choose_case (fun s r -> [ s; r ]) by_sender);
  line "choose [recv; send] blocked, completed by a receiver" "" (choose_case (fun s r -> [ r; s ]) by_receiver);
  line "choose [send; recv] blocked, completed by a receiver" "" (choose_case (fun s r -> [ s; r ]) by_receiver);

  (* what one sync writes to stderr *)
  line "stderr lines written by sync (always 0)" "" (string_of_int (stderr_lines (fun () -> ignore (E.sync 0 (E.always 0)))));
  line "stderr lines written by poll (receive c)" "" (string_of_int (stderr_lines (fun () -> ignore (E.poll (E.receive c)))))
