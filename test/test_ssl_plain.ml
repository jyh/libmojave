(* Lm_ssl without SSL (every dune build: SSL_ENABLED is never defined), the
   path metaprl's browser server runs. Over loopback in one process: a
   listening socket on 127.0.0.1 port 0, a client that connects, the
   accepted connection; text one way with fprintf/output_* and flush, read
   back with input_line/input_char/really_input; then shutdown and close. *)
open Pin
module S = Lm_ssl

let () =
  line "enabled" "" (string_of_bool S.enabled);
  let srv = S.socket "" in
  S.bind srv Unix.inet_addr_loopback 0;
  S.listen srv "" 5;
  let addr, port = S.getsockname srv in
  line "getsockname" "address, port > 0" (Printf.sprintf "%s, %b" (Unix.string_of_inet_addr addr) (port > 0));
  let cl = S.socket "" in
  S.connect cl Unix.inet_addr_loopback port;
  let conn = S.accept srv in
  let cout = S.out_channel_of_ssl cl and sin = S.in_channel_of_ssl conn in
  S.fprintf cout "hello %d %s\n" 42 "world";
  S.output_string cout "second line\n";
  S.output_char cout 'x';
  let b = Buffer.create 8 in Buffer.add_string b "yz\n";
  S.output_buffer cout b;
  S.output cout "0123456789" 2 5;
  S.flush cout;
  line "input_line" "1" (str (S.input_line sin));
  line "input_line" "2" (str (S.input_line sin));
  line "input_char" "" (Printf.sprintf "%C" (S.input_char sin));
  line "input_line" "3" (str (S.input_line sin));
  let r = Bytes.create 5 in
  S.really_input sin r 0 5;
  line "really_input" "5" (str (Bytes.to_string r));
  (* the other way *)
  let sout = S.out_channel_of_ssl conn and cin = S.in_channel_of_ssl cl in
  S.fprintf sout "reply\n"; S.flush sout;
  line "input_line" "reply" (str (S.input_line cin));
  S.shutdown conn;
  line "after shutdown, input_line" "" (guard (fun () -> str (S.input_line cin)));
  S.close conn; S.close cl; S.close srv
