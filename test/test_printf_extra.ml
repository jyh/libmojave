(* Lm_printf.extra_formatting: a formatter whose output goes to a channel
   and is appended to a log file, each run of text tagged with the
   writing process ("[|PID: ...|]"), the processes coordinating through a
   locked "<log>.pid" file. Two processes (a fork) write in turn. Printed:
   the channel's bytes and the log's, with the pids replaced by P1 (this
   process) and P2 (the child). Not on Windows (fork; declared). *)
open Pin

let log = "test_printf_extra.log" and out = "test_printf_extra.out"
let read f = let ic = open_in_bin f in let s = really_input_string ic (in_channel_length ic) in close_in ic; s
let replace ~sub ~by s =
  let b = Buffer.create (String.length s) and n = String.length sub in
  let rec go i =
    if i > String.length s - n then Buffer.add_string b (String.sub s i (String.length s - i))
    else if String.sub s i n = sub then (Buffer.add_string b by; go (i + n))
    else (Buffer.add_char b s.[i]; go (i + 1)) in
  go 0; Buffer.contents b

let () =
  List.iter (fun f -> try Sys.remove f with Sys_error _ -> ()) [ log; log ^ ".pid"; out ];
  let chan = open_out_bin out in
  let fmt = Lm_printf.formatter_of_out_channel chan in
  Lm_printf.extra_formatting log chan fmt;
  let p1 = string_of_int (Unix.getpid ()) in
  Lm_printf.fprintf fmt "one ";
  Lm_printf.fprintf fmt "two@?";
  let p2 =
    match Unix.fork () with
    | 0 -> Lm_printf.fprintf fmt "child@?"; Unix._exit 0
    | pid -> ignore (Unix.waitpid [] pid); string_of_int pid in
  Lm_printf.fprintf fmt " back\nnext\r\nlast\rend@?";
  Lm_printf.fprintf fmt "@[<v 2>box@,indented@]@.";
  close_out chan;
  let show s = str (replace ~sub:p2 ~by:"P2" (replace ~sub:p1 ~by:"P1" s)) in
  line "the channel" "" (show (read out));
  line "the log" "" (show (read log));
  line "the pid file" "" (show (read (log ^ ".pid")));
  List.iter Sys.remove [ log; log ^ ".pid"; out ]
