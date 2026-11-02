(* Lm_filename_util values no other test calls, on inputs that mean the
   same on every platform: unescape_string (quote removal), and
   filename_string / filename_path on names without a drive letter
   (a drive letter is Win32-only and test_filename_util covers it per
   platform). *)
open Pin
module U = Lm_filename_util

let path = function
  | U.AbsolutePath (r, p) -> Printf.sprintf "AbsolutePath (%s, %s)" (if r = U.null_root then "null_root" else "a drive") (str p)
  | U.RelativePath p -> "RelativePath " ^ str p

let parts = function
  | U.AbsolutePath (r, l) -> Printf.sprintf "AbsolutePath (%s, %s)" (if r = U.null_root then "null_root" else "a drive") (list str l)
  | U.RelativePath l -> "RelativePath " ^ list str l

let () =
  List.iter (fun s -> line "unescape_string" (str s) (guard (fun () -> str (U.unescape_string s))))
    [ ""; "abc"; "\"ab\""; "\"a b\"c"; "'ab'"; "'a b'c"; "x'y'z"; "'a'\"b\""; "\"a'b\"";
      "\"a\\\"b\""; "'a\\'b'"; "\"a\\nb\""; "\"open"; "'open" ];
  List.iter (fun s -> line "filename_string" (str s) (guard (fun () -> path (U.filename_string s))))
    [ ""; "a"; "a/b"; "/"; "/a/b"; "./a"; "a\\b" ];
  List.iter (fun s -> line "filename_path" (str s) (guard (fun () -> parts (U.filename_path s))))
    [ ""; "a"; "a/b/c"; "/a/b"; "/a//b"; "a\\b/c"; "/" ]
