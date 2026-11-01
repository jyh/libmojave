(* Lm_terminfo reads the terminal database named by TERM (through ncurses
   where it is linked; elsewhere every capability is absent). Lm_terminfo's
   values are computed when the module starts, so each TERM runs in a child
   process. The database differs between systems, so what is recorded are
   properties that hold on every one: a capability is absent or an escape
   sequence. enter_bold_mode and exit_attribute_mode are capability NAMES
   for tgetstr ("bold", "sgr0"), as Lm_command_util and metaprl's dform use
   them. *)
open Pin

let shape = function None -> "absent" | Some s -> if s <> "" && s.[0] = '\027' then "an escape sequence" else "OTHER " ^ String.escaped s

let child () =
  let t = try Sys.getenv "TERM" with Not_found -> "(unset)" in
  let dumb = t = "dumb" in
  let ok s = if dumb then s = "absent" else s = "absent" || s = "an escape sequence" in
  let r = [ shape (Lm_terminfo.tgetstr Lm_terminfo.enter_bold_mode);
            shape (Lm_terminfo.tgetstr Lm_terminfo.exit_attribute_mode);
            shape (Lm_terminfo.tgetstr "no-such-capability") ] in
  print_string (if List.for_all ok r && List.nth r 2 = "absent" then "as expected" else "UNEXPECTED " ^ String.concat ", " r)

let () =
  if Array.length Sys.argv <= 1 then
    line "capability names" "bold, attributes off" (Printf.sprintf "%S, %S" Lm_terminfo.enter_bold_mode Lm_terminfo.exit_attribute_mode);
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "child" then child ()
  else
    List.iter (fun term ->
        let env = Array.append [| "TERM=" ^ term |]
            (Array.of_list (List.filter (fun e -> not (String.length e >= 5 && String.sub e 0 5 = "TERM=")) (Array.to_list (Unix.environment ())))) in
        let r, w = Unix.pipe ~cloexec:true () in
        let pid = Unix.create_process_env Sys.executable_name [| Sys.executable_name; "child" |] env Unix.stdin w Unix.stderr in
        Unix.close w;
        let ic = Unix.in_channel_of_descr r in
        let out = try input_line ic with End_of_file -> "" in
        close_in ic; ignore (Unix.waitpid [] pid);
        line "TERM" term out) ["dumb"; "xterm"; "vt100"]
