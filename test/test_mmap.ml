(* Lm_mmap maps a file (8 MiB, extended if shorter) and to_bytes views the
   mapping as an OCaml bytes block whose header is the mapping's first word.
   Recorded: the length to_bytes reports, a byte written through it read back
   after close and reopen, and the file's size after close. Until 2026-11 the
   header counted 4-byte words, so on a 64-bit system the block claimed twice
   the mapping and Bytes.length read past its end. *)
open Pin

let name = "test_mmap.tmp"
let mapping = 1 lsl 23

let () =
  (try Sys.remove name with Sys_error _ -> ());
  let m = Lm_mmap.create name [Unix.O_RDWR; Unix.O_CREAT] 0o644 in
  let b = Lm_mmap.to_bytes m in
  line "to_bytes" "length" (string_of_int (Bytes.length b));
  line "to_bytes" "length = mapping - one word - 1" (string_of_bool (Bytes.length b = mapping - Sys.word_size / 8 - 1));
  Bytes.set b 0 'A';
  Bytes.set b 1000 'B';
  Lm_mmap.close m;
  line "file" "size after close" (string_of_int (Unix.stat name).Unix.st_size);
  let m = Lm_mmap.create name [Unix.O_RDWR] 0o644 in
  let b = Lm_mmap.to_bytes m in
  line "reopen" "bytes at 0 and 1000" (Printf.sprintf "%C %C" (Bytes.get b 0) (Bytes.get b 1000));
  Lm_mmap.close m;
  Sys.remove name
