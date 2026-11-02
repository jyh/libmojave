(* Lm_filename_util's search and directory half, on Unix: is_executable
   (a regular file with an execute bit this process holds), which,
   which_dir, where, mkdirhier, lsdir. Everything happens under a scratch
   directory made relative to the test's cwd. Recorded as it stands:
   which and which_dir search the PATH read when the module was loaded
   (only its absolute entries), and share one cache keyed by name, while
   where reads PATH at each call and keeps relative entries; which_dir
   with a bare name does not look in its dir. The tests run as a user
   other than root (root's access ignores the owner rule). Not on Windows (its
   is_executable tries suffixes and ignores permissions). *)
open Pin
module F = Lm_filename_util

let root = "test_filename_exec.dir"
let touch path mode =
  let fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
  Unix.close fd;
  Unix.chmod path mode

(* Paths are printed relative to the cwd, so the expected file does not
   carry the runner's directory. *)
let cwd = Sys.getcwd ()
let rel s =
  let p = cwd ^ "/" in
  let n = String.length p in
  if String.length s >= n && String.sub s 0 n = p then "<cwd>/" ^ String.sub s n (String.length s - n) else s

let res f = guard (fun () -> rel (f ()))
let optp = function None -> "None" | Some s -> "Some " ^ rel s

let rec rm_rf p =
  match (Unix.lstat p).Unix.st_kind with
  | Unix.S_DIR ->
      Array.iter (fun n -> rm_rf (Filename.concat p n)) (Sys.readdir p);
      Unix.rmdir p
  | _ -> Unix.unlink p
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let () =
  rm_rf root;
  (* mkdirhier *)
  line "mkdirhier a/b/c (relative)" "" (guard (fun () -> F.mkdirhier (root ^ "/a/b/c") 0o755; "unit"));
  line "  a/b/c is a directory" "" (string_of_bool (Sys.is_directory (root ^ "/a/b/c")));
  line "mkdirhier a/b/c again" "" (guard (fun () -> F.mkdirhier (root ^ "/a/b/c") 0o755; "unit"));
  line "mkdirhier an absolute path" "" (guard (fun () -> F.mkdirhier (cwd ^ "/" ^ root ^ "/abs/x") 0o755; "unit"));
  line "  abs/x is a directory" "" (string_of_bool (Sys.is_directory (root ^ "/abs/x")));
  touch (root ^ "/a/file") 0o644;
  line "mkdirhier through a regular file" "" (guard (fun () -> F.mkdirhier (root ^ "/a/file/sub") 0o755; "unit"));
  (* lsdir: readdir order is the file system's, so sorted here *)
  line "lsdir a (sorted)" "" (guard (fun () -> list str (List.sort compare (F.lsdir (root ^ "/a")))));
  line "lsdir a missing directory" "" (guard (fun () -> list str (F.lsdir (root ^ "/none"))));
  (* is_executable *)
  let bin = root ^ "/bin" in
  F.mkdirhier bin 0o755;
  List.iter (fun (n, m) -> touch (bin ^ "/" ^ n) m)
    [ "x755", 0o755; "x644", 0o644; "x100", 0o100; "x010", 0o010; "x001", 0o001; "x000", 0o000 ];
  (* The kernel's own answer beside each: access X_OK for this process
     (owner bits for the owner, then group, then other). *)
  let access p = match Unix.access p [ Unix.X_OK ] with () -> "yes" | exception Unix.Unix_error _ -> "no" in
  List.iter (fun n -> let p = bin ^ "/" ^ n in
              line "is_executable" n (optp (F.is_executable p) ^ " (access X_OK: " ^ access p ^ ")"))
    [ "x755"; "x644"; "x100"; "x010"; "x001"; "x000"; "missing" ];
  line "is_executable a directory" "" (optp (F.is_executable bin));
  (* which *)
  line "which ./<bin>/x755 (relative, with a separator)" "" (res (fun () -> F.which ("./" ^ bin ^ "/x755")));
  line "which <bin>/x644 (not executable)" "" (res (fun () -> F.which (bin ^ "/x644")));
  line "which an absolute path" "" (res (fun () -> F.which (cwd ^ "/" ^ bin ^ "/x755")));
  line "which an absolute missing path" "" (res (fun () -> F.which (cwd ^ "/" ^ bin ^ "/missing")));
  line "which sh, found on the load-time PATH" "" (guard (fun () -> let p = F.which "sh" in string_of_bool (Filename.basename p = "sh" && not (Filename.is_relative p))));
  line "which a name on no PATH" "" (res (fun () -> F.which "no-such-command-zq"));
  (* PATH changed now: which keeps the load-time PATH, where reads it *)
  Unix.putenv "PATH" (bin ^ ":" ^ root ^ "/abs/x");
  touch (root ^ "/abs/x/x755") 0o755;
  line "after PATH=<bin>:<abs/x>: which x755" "" (res (fun () -> F.which "x755"));
  line "  where x755" "" (guard (fun () -> list str (F.where "x755")));
  line "  where x644" "" (guard (fun () -> list str (F.where "x644")));
  line "  where a/x755 (a separator)" "" (guard (fun () -> list str (F.where "a/x755")));
  (* which_dir *)
  line "which_dir <bin> x755 (a bare name)" "" (res (fun () -> F.which_dir bin "x755"));
  line "which_dir <bin> ./x755" "" (res (fun () -> F.which_dir bin "./x755"));
  line "which_dir <bin> ./x644" "" (res (fun () -> F.which_dir bin "./x644"));
  line "which_dir <bin> sh (a bare name, the cache)" "" (guard (fun () -> let p = F.which_dir bin "sh" in string_of_bool (Filename.basename p = "sh")));
  rm_rf root
