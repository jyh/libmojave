(* Lm_index: an index from key lists to data, each entry with its own
   sub-index. *)
open Pin
module I = Lm_index.LmMake (struct type t = string let compare = compare end)

let () =
  let t = I.add_list I.empty [ "a"; "b"; "c" ] [ 1; 2; 3 ] in
  let t = I.add t "z" 26 in
  let t = I.add_entry t [ "a"; "x" ] 24 in
  line "add_list [a;b;c] [1;2;3]; add z 26; add_entry [a;x] 24" "" "";
  line "  find_data a, z" "" (guard (fun () -> Printf.sprintf "%d %d" (I.find_data t "a") (I.find_data t "z")));
  line "  find_list_data [a;b;c], [a;x]" "" (guard (fun () -> Printf.sprintf "%d %d" (I.find_list_data t [ "a"; "b"; "c" ]) (I.find_list_data t [ "a"; "x" ])));
  line "  mem a, b (b is not top level)" "" (Printf.sprintf "%b %b" (I.mem t "a") (I.mem t "b"));
  line "  mem_list [a;b], [a;q], []" "" (guard (fun () -> Printf.sprintf "%b %b %b" (I.mem_list t [ "a"; "b" ]) (I.mem_list t [ "a"; "q" ]) (I.mem_list t [])));
  line "  mem_list [q;r] (q missing)" "" (guard (fun () -> string_of_bool (I.mem_list t [ "q"; "r" ])));
  line "  mem_list [a;q;r] (q missing)" "" (guard (fun () -> string_of_bool (I.mem_list t [ "a"; "q"; "r" ])));
  line "  find_data_opt q, find_index_opt q" "" (Printf.sprintf "%s %b" (opt string_of_int (I.find_data_opt t "q")) (I.find_index_opt t "q" = None));
  line "  find_list_data [q]" "" (guard (fun () -> string_of_int (I.find_list_data t [ "q" ])));
  let t2 = I.remove_entry t [ "a"; "b" ] in
  line "remove_entry [a;b]: mem_list [a;b], [a;x], mem a" "" (guard (fun () -> Printf.sprintf "%b %b %b" (I.mem_list t2 [ "a"; "b" ]) (I.mem_list t2 [ "a"; "x" ]) (I.mem t2 "a")));
  let t3 = I.remove t "a" in
  line "remove a: mem a, mem z, is_empty" "" (Printf.sprintf "%b %b %b" (I.mem t3 "a") (I.mem t3 "z") (I.is_empty t3));
  line "is_empty (remove z of that)" "" (string_of_bool (I.is_empty (I.remove t3 "z")));
  let t4 = I.filter_add t "z" (function Some v -> v + 1 | None -> 0) in
  let t4 = I.filter_add t4 "n" (function Some v -> v + 1 | None -> 0) in
  line "filter_add z (+1), n (new 0)" "" (guard (fun () -> Printf.sprintf "%d %d" (I.find_data t4 "z") (I.find_data t4 "n")));
  line "original after filter_add (persistent)" "" (guard (fun () -> Printf.sprintf "%d %b" (I.find_data t "z") (I.mem t "n")))
