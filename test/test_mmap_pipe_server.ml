(* Lm_mmap_pipe's server side, beyond test_mmap_pipe's one message: the
   server search (a stale socket is reused, a live server is skipped, 15 live
   servers is "too many"), messages both ways, a second client turned away,
   close_client, the socket accessors, and a second server started beside a
   live one. Each scenario has its own directory. Children leave with
   Unix._exit: a forked child of a server runs the server's at_exit
   otherwise, which unlinks the SERVER's socket and buffer (stated, not
   changed). Every child sets an alarm, so a hang reads as "signal". Not on
   Windows (no fork). *)
open Pin

let mkdir d = try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
let exists f = Sys.file_exists f

let status pid =
  match Unix.waitpid [] pid with
  | _, Unix.WEXITED n -> "exit " ^ string_of_int n
  | _, Unix.WSIGNALED n -> "signal " ^ string_of_int n
  | _, Unix.WSTOPPED _ -> "stopped"

let child f =
  match Unix.fork () with
  | 0 -> ignore (Unix.alarm 5); (try f () with _ -> Unix._exit 99); Unix._exit 98
  | pid -> pid

let writer s = fun buf off len ->
  let n = min len (String.length s) in Bytes.blit_string s 0 buf off n; n

let rec read_one t tries =
  if tries = 0 then None
  else match Lm_mmap_pipe.read t (fun buf off len -> Bytes.sub_string buf off len) with
    | Some (code, name, data) -> Some (code, Bytes.to_string name, data)
    | None -> if Lm_mmap_pipe.block t then read_one t (tries - 1) else None

let show = function
  | Some (code, name, data) -> Printf.sprintf "code %d, name %S, data %S" code name data
  | None -> "None"

(* A bound, then closed, Unix socket: the file stays, nobody listens. *)
let stale_socket path =
  let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind s (Unix.ADDR_UNIX path); Unix.close s

let listening path =
  let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind s (Unix.ADDR_UNIX path); Unix.listen s 1; s

(* 1: a stale 1.sock is reused; messages both ways; close_client. *)
let () =
  let d = "mps1.dir" in
  mkdir d;
  stale_socket (d ^ "/1.sock");
  let srv = Lm_mmap_pipe.create_server d in
  line "stale" "1.buffer created" (string_of_bool (exists (d ^ "/1.buffer")));
  line "stale" "server_socket on the server"
    (guard (fun () -> ignore (Lm_mmap_pipe.server_socket srv); "a descriptor"));
  line "stale" "client_socket before a client"
    (guard (fun () -> ignore (Lm_mmap_pipe.client_socket srv); "a descriptor"));
  let pid = child (fun () ->
      let cl = Lm_mmap_pipe.create_client d in
      let a = (try ignore (Lm_mmap_pipe.server_socket cl); 1 with Invalid_argument _ -> 0) in
      let b = (try ignore (Lm_mmap_pipe.client_socket cl); 1 with Invalid_argument _ -> 0) in
      let c = (try Lm_mmap_pipe.close_client cl; 1 with Invalid_argument _ -> 0) in
      if not (Lm_mmap_pipe.write cl 1 "up" (writer "client to server")) then Unix._exit 2;
      (match read_one cl 200 with
       | Some (2, "down", data) when String.sub data 0 16 = "server to client" -> ()
       | _ -> Unix._exit 3);
      if not (Lm_mmap_pipe.write cl 3 "ack" (writer "got it")) then Unix._exit 4;
      (* After the server's close_client the socket reads end of file; wake-up
         bytes left over from earlier messages are drained first. *)
      let rec drain () = if Lm_mmap_pipe.block cl then drain () in
      drain ();
      Unix._exit (10 + a * 4 + b * 2 + c))
  in
  line "stale" "open_client" (string_of_bool (Lm_mmap_pipe.open_client srv));
  line "stale" "client_socket with a client"
    (guard (fun () -> ignore (Lm_mmap_pipe.client_socket srv); "a descriptor"));
  line "stale" "server reads" (show (read_one srv 200));
  line "stale" "server writes"
    (string_of_bool (Lm_mmap_pipe.write srv 2 "down" (writer "server to client")));
  line "stale" "server reads the ack" (show (read_one srv 200));
  Lm_mmap_pipe.close_client srv;
  line "stale" "client_socket after close_client"
    (guard (fun () -> ignore (Lm_mmap_pipe.client_socket srv); "a descriptor"));
  line "stale" "close_client again"
    (guard (fun () -> Lm_mmap_pipe.close_client srv; "unit"));
  (* 10 + server_socket(0) + client_socket(2) + close_client(0); a hang is the alarm *)
  line "stale" "client: accessors, round trip, eof" (status pid)

(* 2: a second client while the first is connected is turned away (busy
   byte 0), searches the other slots, finds none. *)
let () =
  let d = "mps2.dir" in
  mkdir d;
  let srv = Lm_mmap_pipe.create_server d in
  let first = child (fun () ->
      let cl = Lm_mmap_pipe.create_client d in
      ignore (Lm_mmap_pipe.block cl); Unix._exit 0) in
  line "busy" "open_client, first" (string_of_bool (Lm_mmap_pipe.open_client srv));
  let second = child (fun () ->
      match Lm_mmap_pipe.create_client d with
      | _ -> Unix._exit 1
      | exception Failure m when m = "Lm_mmap_pipe: no servers are online" -> Unix._exit 0
      | exception _ -> Unix._exit 2) in
  line "busy" "open_client, second" (string_of_bool (Lm_mmap_pipe.open_client srv));
  line "busy" "second client: Failure no servers (exit 0)" (status second);
  Lm_mmap_pipe.close_client srv;
  line "busy" "first client: eof after close_client" (status first)

(* 3: slots 1..15 all live: "too many running servers" (slot 0 is never
   used, so there are 15, not max_servers = 16). *)
let () =
  let d = "mps3.dir" in
  mkdir d;
  let socks = List.init 15 (fun i -> listening (Printf.sprintf "%s/%d.sock" d (i + 1))) in
  line "full" "create_server with 15 live"
    (guard (fun () -> ignore (Lm_mmap_pipe.create_server d); "a server"));
  List.iter Unix.close socks

(* 4: a second server starts beside a live, free one. Its search connects to
   the live server to see that it is alive, and hangs up; that connection
   waits in the live server's accept queue. (b) The live server's next
   open_client, with only that probe queued. (c) A client comes; as a caller
   does (metaprl's appl_outboard_server), the parent waits for a server
   socket to be readable and calls that server's open_client, until one
   takes the client; that server then sends it a message. *)
let () =
  let d = "mps4.dir" in
  mkdir d;
  let srv1 = Lm_mmap_pipe.create_server d in
  let srv2 = Lm_mmap_pipe.create_server d in
  line "beside" "second server took slot 2" (string_of_bool (exists (d ^ "/2.buffer")));
  line "beside" "first server, open_client with the probe queued"
    (string_of_bool (Lm_mmap_pipe.open_client srv1));
  line "beside" "first server holds a client"
    (guard (fun () -> ignore (Lm_mmap_pipe.client_socket srv1); "a descriptor"));
  let pid = child (fun () ->
      let cl = Lm_mmap_pipe.create_client d in
      match read_one cl 200 with
      | Some (5, "hi", _) -> Unix._exit 0
      | _ -> Unix._exit 1) in
  let s1 = Lm_mmap_pipe.server_socket srv1 and s2 = Lm_mmap_pipe.server_socket srv2 in
  let rec serve rounds =
    if rounds = 0 then None
    else match Unix.select [s1; s2] [] [] 1.0 with
      | [], _, _ -> serve (rounds - 1)
      | r, _, _ ->
          let take (name, srv, s) = if List.mem s r && Lm_mmap_pipe.open_client srv then Some (name, srv) else None in
          (match take ("first", srv1, s1) with
           | Some x -> Some x
           | None -> (match take ("second", srv2, s2) with Some x -> Some x | None -> serve (rounds - 1)))
  in
  (match serve 10 with
   | Some (name, srv) ->
       line "beside" "the client was taken by" name;
       line "beside" "that server writes" (string_of_bool (Lm_mmap_pipe.write srv 5 "hi" (writer "x")))
   | None -> line "beside" "the client was taken by" "no server in 10 rounds");
  line "beside" "client reads the message (exit 0)" (status pid)
