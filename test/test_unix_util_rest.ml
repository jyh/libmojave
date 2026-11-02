(* Lm_unix_util's rest, on Unix, under a scratch directory: copy_file
   (contents, mode, a missing source, an unwritable destination, a
   directory as the source, and the descriptors the failures leave
   open), mkdirhier (relative, absolute, simplified, through a regular
   file: EEXIST, where Lm_filename_util's says ENOTDIR), ftruncate (at the
   current position). The
   HOME-missing branch runs when the module loads, out of a test's reach.
   Not on Windows (its ftruncate and modes differ; declared). *)
open Pin
module U = Lm_unix_util

let root = "test_unix_util_rest.dir"
let write path s =
  let oc = open_out_bin path in output_string oc s; close_out oc
let read path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in close_in ic; s
let rec rm_rf p =
  match (Unix.lstat p).Unix.st_kind with
  | Unix.S_DIR -> Array.iter (fun n -> rm_rf (Filename.concat p n)) (Sys.readdir p); Unix.rmdir p
  | _ -> Unix.unlink p
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
let fds () = Array.length (Sys.readdir "/dev/fd")
let mode p = Printf.sprintf "0o%o" ((Unix.stat p).Unix.st_perm)

let () =
  rm_rf root;
  Unix.mkdir root 0o755;
  let src = root ^ "/src" and dst = root ^ "/dst" in
  let data = String.init 20000 (fun i -> Char.chr (i * 7 land 255)) in
  write src data;
  line "copy_file 20000 bytes, mode 0o640" "" (guard (fun () -> U.copy_file src dst 0o640; "unit"));
  line "  same contents" "" (string_of_bool (read dst = data));
  line "  mode" "" (mode dst);
  write dst "old and longer than the new source";
  write src "new";
  line "copy_file over a longer file" "" (guard (fun () -> U.copy_file src dst 0o600; str (read dst)));
  let before = fds () in
  line "copy_file from a missing source" "" (guard (fun () -> U.copy_file (root ^ "/none") dst 0o600; "unit"));
  line "copy_file into a missing directory" "" (guard (fun () -> U.copy_file src (root ^ "/no/dst") 0o600; "unit"));
  (* A directory opens for reading and fails at the first read: the
     failure inside the copy, with both files open. *)
  line "copy_file from a directory" "" (guard (fun () -> U.copy_file root dst 0o600; "unit"));
  line "  open descriptors gained by the three failures" "" (string_of_int (fds () - before));
  (* mkdirhier *)
  line "mkdirhier a/b/c (relative)" "" (guard (fun () -> U.mkdirhier (root ^ "/a/b/c"); "unit"));
  line "  a/b/c is a directory" "" (string_of_bool (Sys.is_directory (root ^ "/a/b/c")));
  line "mkdirhier a/b/c again" "" (guard (fun () -> U.mkdirhier (root ^ "/a/b/c"); "unit"));
  line "mkdirhier an absolute name" "" (guard (fun () -> U.mkdirhier (Sys.getcwd () ^ "/" ^ root ^ "/abs/x"); "unit"));
  line "  abs/x is a directory" "" (string_of_bool (Sys.is_directory (root ^ "/abs/x")));
  line "mkdirhier a/../d/./e (simplified)" "" (guard (fun () -> U.mkdirhier (root ^ "/a/../d/./e"); string_of_bool (Sys.is_directory (root ^ "/d/e"))));
  line "mkdirhier through a regular file" "" (guard (fun () -> U.mkdirhier (src ^ "/sub"); "unit"));
  (* ftruncate: at the current position *)
  let f = root ^ "/t" in
  write f "0123456789";
  let fd = Unix.openfile f [ Unix.O_RDWR ] 0 in
  ignore (Unix.lseek fd 4 Unix.SEEK_SET);
  U.ftruncate fd;
  Unix.close fd;
  line "ftruncate at offset 4 of 10 bytes" "" (str (read f));
  rm_rf root
