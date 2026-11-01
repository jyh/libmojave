(* Lm_unix_util.getlk fd cmd: which process holds a lock on the whole file
   that would block cmd. A child process holds a write lock (F_LOCK), then
   a read lock (F_RLOCK), then nothing; the parent asks with every
   Unix.lock_command. "holder" is the child's pid. Not on Windows (no
   fcntl locks there). *)
open Pin

let cmds = Unix.[F_ULOCK, "F_ULOCK"; F_LOCK, "F_LOCK"; F_TLOCK, "F_TLOCK"; F_TEST, "F_TEST"; F_RLOCK, "F_RLOCK"; F_TRLOCK, "F_TRLOCK"]
let file = "test_getlk.tmp"

let child held =
  let fd = Unix.openfile file [Unix.O_RDWR] 0 in
  Unix.lockf fd (List.assoc held (List.map (fun (a, b) -> b, a) cmds)) 0;
  print_endline "locked";
  ignore (try input_line stdin with End_of_file -> "");
  exit 0

let ask fd who held =
  List.iter (fun (c, n) ->
      line "getlk" (Printf.sprintf "(%s held) %s" held n)
        (guard (fun () -> opt (fun p -> if p = who then "holder" else "another pid") (Lm_unix_util.getlk fd c)))) cmds

let parent () =
  let fd = Unix.openfile file [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
  ignore (Unix.write_substring fd "0123456789" 0 10);
  List.iter (fun held ->
      let r, w = Unix.pipe ~cloexec:true () and r2, w2 = Unix.pipe ~cloexec:true () in
      let pid = Unix.create_process Sys.executable_name [| Sys.executable_name; "child"; held |] r w2 Unix.stderr in
      Unix.close r; Unix.close w2;
      ignore (input_line (Unix.in_channel_of_descr r2));
      ask fd pid held;
      Unix.close w; ignore (Unix.waitpid [] pid)) ["F_LOCK"; "F_RLOCK"];
  ask fd (-1) "nothing";
  Unix.close fd; Sys.remove file

let () = if Array.length Sys.argv > 2 && Sys.argv.(1) = "child" then child Sys.argv.(2) else parent ()
