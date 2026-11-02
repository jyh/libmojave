(* Lm_channel's text mode (binary = false). Only Windows has one;
   elsewhere create forces binary and the bytes pass unchanged, so this
   test compares against a per-platform expected file.
   Measured on the Windows CI lanes (mingw and msvc, 2026-11-02): a write
   turns "\n" into "\r\n", but [read] with an empty buffer calls the raw
   reader and returns "\r\n" untouched (as [write] says of itself, "always
   binary"). The "\r\n" -> "\n" squash happens only in fillbuf, i.e. on
   the input_char / input_line path, and [read] then returns whatever
   fillbuf left in the buffer: squashed bytes after an input_char, raw
   bytes otherwise. Cases: a write; read and input_char on a lone "\r";
   both on a "\r\n" that straddles the 4096-byte text fill (the one extra
   character read after a final "\r"); input_char then read. *)
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

let read_chars ch =
  let b = Buffer.create 64 in
  (try while true do Buffer.add_char b (C.input_char ch) done with End_of_file -> ());
  C.close ch; Buffer.contents b
let summary s =
  Printf.sprintf "length %d, \\r %d, \\n %d, ends %s" (String.length s) (count '\r' s) (count '\n' s)
    (str (String.sub s (String.length s - 8) 8))
let input () = chan C.InChannel [ Unix.O_RDONLY ]

let () =
  let ch = chan C.OutChannel [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] in
  C.output_string ch "a\nb\n\n";
  C.close ch;
  line "write \"a\\nb\\n\\n\", the bytes on disk" "" (str (raw_read ()));
  raw_write "x\r\ny\r\n\r\nz\rw";
  line "read \"x\\r\\ny\\r\\n\\r\\nz\\rw\"" "" (str (read_all (input ())));
  line "input_char \"x\\r\\ny\\r\\n\\r\\nz\\rw\"" "" (str (read_chars (input ())));
  raw_write (String.make 4095 'a' ^ "\r\n" ^ "tail\r\n");
  line "read 4095 a, \"\\r\\n\" at the fill boundary, \"tail\\r\\n\"" "" (summary (read_all (input ())));
  line "input_char 4095 a, \"\\r\\n\" at the fill boundary, \"tail\\r\\n\"" "" (summary (read_chars (input ())));
  raw_write "x\r\ny\r\n";
  let ch = input () in
  let c = C.input_char ch in
  line "input_char then read, \"x\\r\\ny\\r\\n\"" "" (Printf.sprintf "%C then %s" c (str (read_all ch)));
  Sys.remove file
