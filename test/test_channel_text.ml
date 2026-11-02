(* Lm_channel's text mode (binary = false). Only Windows has one: there a
   write turns "\n" into "\r\n" and a read turns "\r\n" back into "\n";
   elsewhere create forces binary and the bytes pass unchanged. So this
   test compares against a per-platform expected file. Cases: a write, a
   read with a lone "\r", and a read whose "\r\n" straddles the channel's
   4096-byte fill (the one extra character read after a final "\r"). *)
open Pin
module C = Lm_channel

let file = "test_channel_text.tmp"
let raw_write s = let oc = open_out_bin file in output_string oc s; close_out oc
let raw_read () =
  let ic = open_in_bin file in
  let s = really_input_string ic (in_channel_length ic) in close_in ic; s
let chan mode flags =
  C.create file C.FileChannel mode false (Some (Unix.openfile file flags 0o644))
let read_all ch =
  let b = Buffer.create 64 and buf = Bytes.create 1000 in
  let rec go () =
    match C.read ch buf 0 1000 with
    | 0 -> ()
    | n -> Buffer.add_subbytes b buf 0 n; go ()
    | exception End_of_file -> () in
  go (); C.close ch; Buffer.contents b
let count c s = String.fold_left (fun n x -> if x = c then n + 1 else n) 0 s

let () =
  let ch = chan C.OutChannel [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] in
  C.output_string ch "a\nb\n\n";
  C.close ch;
  line "write \"a\\nb\\n\\n\", the bytes on disk" "" (str (raw_read ()));
  raw_write "x\r\ny\r\n\r\nz\rw";
  line "read \"x\\r\\ny\\r\\n\\r\\nz\\rw\"" "" (str (read_all (chan C.InChannel [ Unix.O_RDONLY ])));
  raw_write (String.make 4095 'a' ^ "\r\n" ^ "tail\r\n");
  let s = read_all (chan C.InChannel [ Unix.O_RDONLY ]) in
  line "read 4095 a, \"\\r\\n\" at the fill boundary, \"tail\\r\\n\"" ""
    (Printf.sprintf "length %d, \\r %d, \\n %d, ends %s" (String.length s) (count '\r' s) (count '\n' s)
       (str (String.sub s (String.length s - 8) 8)));
  Sys.remove file
