(* Lm_inet over loopback: serve on an ephemeral port, connect, accept,
   and send bytes both ways. Host names vary by machine, so only ports,
   data and errors are printed. *)
open Pin

let send fd s = ignore (Unix.write_substring fd s 0 (String.length s))
let recv fd n =
  let b = Bytes.create n in
  let rec go k = if k = n then Bytes.to_string b else go (k + Unix.read fd b k (n - k)) in
  go 0

let () =
  let srv = Lm_inet.serve None in
  let _, port = Lm_inet.get_server_host srv in
  line "serve None: port > 0" "" (string_of_bool (port > 0));
  let cl = Lm_inet.connect "127.0.0.1" port in
  let acc = Lm_inet.accept srv in
  let _, peer_port = Lm_inet.get_client_peer cl in
  line "connect 127.0.0.1: the client's peer port is the server's" "" (string_of_bool (peer_port = port));
  let _, local = Lm_inet.get_client_host cl and _, acc_peer = Lm_inet.get_client_peer acc in
  line "accept: the accepted peer port is the client's local port" "" (string_of_bool (local = acc_peer));
  send (Lm_inet.file_descr_of_client cl) "ping";
  line "client -> server" "" (str (recv (Lm_inet.file_descr_of_client acc) 4));
  send (Lm_inet.file_descr_of_client acc) "pong!";
  line "server -> client" "" (str (recv (Lm_inet.file_descr_of_client cl) 5));
  Unix.close (Lm_inet.file_descr_of_client cl);
  Unix.close (Lm_inet.file_descr_of_client acc);
  Lm_inet.close_server srv;
  (* the port is closed now *)
  line "connect to the closed port" "" (guard (fun () -> ignore (Lm_inet.connect "127.0.0.1" port); "connected"))
