(* Lm_inet.connect and the socket of a failed connect: open descriptors
   are counted in /dev/fd around 20 failed connects to a closed loopback
   port, 10 by address and 10 by name ("localhost", the gethostbyname
   search). Not on Windows (no /dev/fd; the code path is the same there,
   unmeasured). *)
open Pin

let () =
  let srv = Lm_inet.serve None in
  let _, port = Lm_inet.get_server_host srv in
  Lm_inet.close_server srv;
  let fds () = Array.length (Sys.readdir "/dev/fd") in
  (* The first name lookup may open the resolver's own descriptor, which
     stays: look one up before counting. *)
  ignore (Unix.gethostbyname "localhost");
  let before = fds () in
  let errors = ref 0 in
  for _ = 1 to 10 do
    (try ignore (Lm_inet.connect "127.0.0.1" port) with Unix.Unix_error _ -> incr errors);
    (try ignore (Lm_inet.connect "localhost" port) with Unix.Unix_error _ -> incr errors)
  done;
  line "failed connects" "" (string_of_int !errors);
  line "open descriptors gained over them" "" (string_of_int (fds () - before))
