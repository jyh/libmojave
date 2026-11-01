(* Lm_mmap_pipe: a server and a client in two processes (Unix.fork) share a
   mapped buffer under one directory; the client writes a message, the server
   accepts it, blocks for it and reads it. Recorded: each step's answer, and
   the data as read, which comes back padded to a multiple of 4 bytes (the
   writer stores the size rounded up; callers frame their own payload). It
   could not work on a 64-bit system before libmojave #120 (Lm_mmap.to_bytes).
   The directory is relative: a Unix socket's path must fit 104 bytes. Not on
   Windows (no fork; Lm_mmap's Windows branch is an open question). *)
open Pin

let dir = "test_mmap_pipe.dir"
let message = "hello from the client"

let () =
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let srv = Lm_mmap_pipe.create_server dir in
  match Unix.fork () with
  | 0 ->
      let cl = Lm_mmap_pipe.create_client dir in
      let ok = Lm_mmap_pipe.write cl 7 "name" (fun buf off len ->
          let n = min len (String.length message) in Bytes.blit_string message 0 buf off n; n) in
      Unix.sleepf 0.5;
      exit (if ok then 0 else 1)
  | pid ->
      line "server" "open_client" (string_of_bool (Lm_mmap_pipe.open_client srv));
      line "server" "block" (string_of_bool (Lm_mmap_pipe.block srv));
      line "server" "read"
        (match Lm_mmap_pipe.read srv (fun buf off len -> Bytes.sub_string buf off len) with
         | Some (code, name, data) -> Printf.sprintf "code %d, name %S, data %S" code (Bytes.to_string name) data
         | None -> "None");
      let status = match Unix.waitpid [] pid with _, Unix.WEXITED n -> "exit " ^ string_of_int n | _ -> "killed" in
      line "client" "write, then exit" status
