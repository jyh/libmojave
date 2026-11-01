(* Lm_unix_util.flock between two processes, getpwents and int_of_fd.
   The parent takes LOCK_EX; a child (this program, re-run) tries the
   non-blocking LOCK_TEX and LOCK_TSH on its own descriptor and reports
   through its exit code; then the parent unlocks and the child tries
   again. Not on Windows (no fcntl or flock there; another branch). *)
open Pin
module U = Lm_unix_util

let file = "test_flock.tmp"

let try_lock name cmd =
  let fd = Unix.openfile file [Unix.O_RDWR] 0 in
  let ok = (try U.flock fd cmd; true with Failure _ -> false) in
  ignore name; exit (if ok then 0 else 1)

let child cmd =
  let pid = Unix.create_process Sys.executable_name [| Sys.executable_name; "child"; cmd |] Unix.stdin Unix.stdout Unix.stderr in
  match Unix.waitpid [] pid with
  | _, Unix.WEXITED 0 -> "granted"
  | _, Unix.WEXITED 1 -> "refused"
  | _ -> "child failed"

let () =
  if Array.length Sys.argv > 2 && Sys.argv.(1) = "child" then
    try_lock Sys.argv.(2) (if Sys.argv.(2) = "LOCK_TEX" then U.LOCK_TEX else U.LOCK_TSH);
  let fd = Unix.openfile file [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
  U.flock fd U.LOCK_EX;
  line "flock" "LOCK_EX held, another process asks LOCK_TEX" (child "LOCK_TEX");
  line "flock" "LOCK_EX held, another process asks LOCK_TSH" (child "LOCK_TSH");
  U.flock fd U.LOCK_UN;
  line "flock" "after LOCK_UN, another process asks LOCK_TEX" (child "LOCK_TEX");
  U.flock fd U.LOCK_SH;
  line "flock" "LOCK_SH held, another process asks LOCK_TSH" (child "LOCK_TSH");
  line "flock" "LOCK_SH held, another process asks LOCK_TEX" (child "LOCK_TEX");
  U.flock fd U.LOCK_UN;
  line "int_of_fd" "the descriptor's number" (string_of_bool (U.int_of_fd fd = (Obj.magic fd : int)));
  Unix.close fd; Sys.remove file;
  let me = Unix.getuid () in
  let ents = U.getpwents () in
  line "getpwents" "non-empty, and includes the current uid"
    (string_of_bool (ents <> [] && List.exists (fun e -> e.Unix.pw_uid = me) ents))
