(* Lm_channel.of_string channels (row 304): info's id and set_id, and what
   a write does. Such a channel is for input; its writer is null_writer,
   which takes nothing, so each write runs in a forked child that is
   stopped after 2 s, and a call that never returns prints as such. (omake's
   open-in-string returns one of these channels.) Not on Windows (fork). *)
open Pin
module C = Lm_channel

let in_child what f =
  Printf.printf "%s  = %!" what;
  match Unix.fork () with
  | 0 -> (try f (); print_string "returned" with e -> print_string ("EXN " ^ Printexc.to_string e)); flush stdout; Unix._exit 0
  | pid ->
      let rec wait n =
        match Unix.waitpid [ Unix.WNOHANG ] pid with
        | 0, _ when n > 0 -> Unix.sleepf 0.05; wait (n - 1)
        | 0, _ -> Unix.kill pid Sys.sigkill; ignore (Unix.waitpid [] pid); print_string "did not return within 2 s"
        | _ -> () in
      wait 40;
      print_newline ()

let () =
  let ch = C.of_string "abc\n" in
  let id, _, _, _ = C.info ch in
  line "info id, new" "" (string_of_int id);
  C.set_id ch 42;
  let id, _, _, _ = C.info ch in
  line "info id, after set_id 42" "" (string_of_int id);
  in_child "output_string \"xy\"" (fun () -> C.output_string ch "xy");
  in_child "output_char then flush" (fun () -> C.output_char ch 'x'; C.flush ch);
  in_child "write 2 bytes" (fun () -> print_string (string_of_int (C.write ch (Bytes.of_string "xy") 0 2) ^ " "));
  line "input_line (reading is unaffected)" "" (guard (fun () -> str (C.input_line ch)))
