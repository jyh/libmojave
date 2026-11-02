(* Lm_uname against the uname command. Each field must equal what
   `uname -s/-n/-r/-v/-m` prints, read at run time. Only true/false is
   printed: nodename is the host name, and release/version vary by
   machine. On Windows there is no uname command to compare with, so
   each field must only be non-empty and sysname start with "win" (any
   case) -- a weaker check, with the same printed lines. On every
   platform machine must not be "unknown", which x86-64 Windows read
   until the C code learned AMD64 and ARM64; it still goes to stderr
   on Windows. *)
open Pin

let command flag =
  let ic = Unix.open_process_in ("uname " ^ flag) in
  let s = try input_line ic with End_of_file -> "" in
  ignore (Unix.close_process_in ic);
  s

let check name flag value =
  let ok =
    if Sys.win32 then value <> ""
    else value = command flag
  in
  line name ("agrees with uname " ^ flag) (string_of_bool ok)

let () =
  check "sysname" "-s" Lm_uname.sysname;
  check "nodename" "-n" Lm_uname.nodename;
  check "release" "-r" Lm_uname.release;
  check "version" "-v" Lm_uname.version;
  check "machine" "-m" Lm_uname.machine;
  let prefix =
    if Sys.win32 then
      String.length Lm_uname.sysname >= 3
      && String.lowercase_ascii (String.sub Lm_uname.sysname 0 3) = "win"
    else Lm_uname.sysname <> ""
  in
  line "sysname" "is non-empty (Windows: starts with win)" (string_of_bool prefix);
  line "machine" "is not \"unknown\"" (string_of_bool (Lm_uname.machine <> "unknown"));
  if Sys.win32 then Printf.eprintf "Windows machine field: %S\n%!" Lm_uname.machine
