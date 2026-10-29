(* Lm_file_util: '/'-separated path lists, and opening files. *)
open Pin
open Lm_file_util

let sl = list str
let () =
  List.iter (fun (dir, name) ->
      line "parse_path" (sl dir ^ " " ^ str name) (guard (fun () -> sl (parse_path dir name))))
    [ [], "a/b/c"; [], "a//./b/"; [], "a/../b"; [], "../a"; [], "/abs/x"; [], "";
      [ "usr"; "lib" ], "x"; [ "usr"; "lib" ], "../x"; [ "usr"; "lib" ], "/x" ];
  List.iter (fun l -> line "build_path" (sl l) (str (build_path l))) [ []; [ "a" ]; [ "a"; "b"; "c" ] ];
  List.iter (fun p -> line "path_dir / path_file" (str p) (str (path_dir p) ^ " " ^ str (path_file p)))
    [ "a/b/c"; "c"; ""; "/usr/lib/x"; "a/b/" ];
  (* files: write, read back, find in a path list *)
  let dir = Filename.get_temp_dir_name () in
  let name = Printf.sprintf "test_file_util_%d.txt" (Unix.getpid ()) in
  let file = Filename.concat dir name in
  with_output_file file (fun oc -> output_string oc "hello\n");
  line "with_output_file, with_input_file" "" (str (with_input_file file input_line));
  line "with_input_file, f raises" "" (guard (fun () -> with_input_file file (fun _ -> failwith "boom")));
  line "open_in_path [missing; tmp]" "" (guard (fun () ->
      let ic, full = open_in_path [ Filename.concat dir "no_such_dir_xyz"; dir ] name in
      let s = input_line ic in close_in ic; s ^ (if Filename.basename full = name then " from the file" else " from " ^ full)));
  line "open_in_path [missing]" "" (try ignore (open_in_path [ "no_such_dir_xyz" ] name); "found" with CantFind n -> if n = name then "EXN CantFind (the name)" else "EXN CantFind " ^ n);
  Sys.remove file
