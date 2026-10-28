open Pin
module F = Lm_filename_util

let paths = [ ""; "/"; "a"; "a/b"; "/a/b/"; "a//b"; "./a"; "../a/./b/../c"; "a.b.c"; "/x/y.tar.gz"; ".hidden"; "dir/"; "C:\\x\\y"; "~/z" ]

let () =
  line "separator_string" "" (str F.separator_string);
  List.iter (fun p -> line "is_absolute" (str p) (guard (fun () -> string_of_bool (F.is_absolute p)))) paths;
  List.iter (fun p -> line "basename" (str p) (guard (fun () -> str (F.basename p)))) paths;
  List.iter (fun p -> line "root" (str p) (guard (fun () -> str (F.root p)))) paths;
  List.iter (fun p -> line "suffix" (str p) (guard (fun () -> str (F.suffix p)))) paths;
  List.iter (fun p -> line "strip_suffixes" (str p) (guard (fun () -> str (F.strip_suffixes p)))) paths;
  List.iter (fun p -> line "split" (str p) (guard (fun () -> let a, b = F.split p in str a ^ ", " ^ str b))) paths;
  List.iter (fun p -> line "normalize_string" (str p) (guard (fun () -> str (F.normalize_string p)))) paths;
  List.iter (fun p -> line "drive_skip" (str p) (guard (fun () -> string_of_int (F.drive_skip p)))) paths;
  List.iter (fun p -> line "concat∘simplify∘split_path" (str p)
               (guard (fun () -> str (F.concat_path (F.simplify_path (F.split_path p)))))) paths;
  List.iter (fun p -> line "normalize_path" (list str p) (guard (fun () -> list str (F.normalize_path p))))
    [ []; [ "a"; "."; "b" ]; [ "a"; ".."; "b" ]; [ ".."; "a" ]; [ "a"; "b"; ".."; ".." ] ];
  List.iter (fun (p, b) -> line "replace_basename" (str p ^ " " ^ str b) (guard (fun () -> str (F.replace_basename p b))))
    [ ("a/b", "c"); ("b", "c"); ("/b", "c") ]
