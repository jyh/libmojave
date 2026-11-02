(* Lm_glob paths no test reached (row 303): glob of an absolute name that
   is not a pattern (a directory, a file, a missing name), and glob_argv of
   a pattern matching both directories and files, whose results are merged
   and sorted. Stated: a match of one kind only comes back in the
   directory's order, so those lines are sorted by the test. A scratch tree in the test's directory; absolute paths print
   as <cwd>. Not on Windows (separators). *)
open Pin
module G = Lm_glob

let top = "glob_mix.tmp"
let cwd = Sys.getcwd ()
let norm s =
  let n = String.length cwd in
  if String.length s >= n && String.sub s 0 n = cwd then "<cwd>" ^ String.sub s n (String.length s - n) else s
let pair (d, f) = Printf.sprintf "dirs %s files %s" (list (fun s -> str (norm s)) d) (list (fun s -> str (norm s)) f)

let () =
  let rec rm_rf p =
    if Sys.is_directory p then (Array.iter (fun n -> rm_rf (Filename.concat p n)) (Sys.readdir p); Unix.rmdir p)
    else Sys.remove p in
  (try rm_rf top with _ -> ());
  Unix.mkdir top 0o755;
  List.iter (fun d -> Unix.mkdir (Filename.concat top d) 0o755) [ "da"; "db" ];
  List.iter (fun f -> close_out (open_out (Filename.concat top f))) [ "fa"; "fb"; "zc" ];
  let abs = Filename.concat cwd top in
  let g names = guard (fun () -> pair (G.glob G.default_glob_options "." names)) in
  line "glob, an absolute directory" "" (g [ Filename.concat abs "da" ]);
  line "glob, an absolute file" "" (g [ Filename.concat abs "fa" ]);
  line "glob, an absolute missing name" "" (g [ Filename.concat abs "nope" ]);
  let a pats = guard (fun () -> list (fun s -> str (norm s)) (G.glob_argv G.default_glob_options top pats)) in
  line "glob_argv, ?a (a directory and a file)" "" (a [ "?a" ]);
  line "glob_argv, * (both kinds)" "" (a [ "*" ]);
  (* One kind only: glob_argv returns them in the directory's own order
     (reversed readdir), which differs between file systems; sorted here. *)
  let sorted pats = guard (fun () -> list (fun s -> str (norm s)) (List.sort compare (G.glob_argv G.default_glob_options top pats))) in
  line "glob_argv, d* (directories only; sorted by the test)" "" (sorted [ "d*" ]);
  line "glob_argv, f* (files only; sorted by the test)" "" (sorted [ "f*" ]);
  line "glob_argv, ?a then fb (order of the arguments kept)" "" (a [ "?a"; "fb" ]);
  rm_rf top
