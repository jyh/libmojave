(* Lm_int_set: int sets and maps. The order is the module's own compare,
   checked at the extremes, where a subtraction-based compare overflows. *)
open Pin
open Lm_int_set

let ints = list string_of_int
let xs = [ 3; max_int; -1; min_int; 0; 3; -max_int ]

let () =
  line "IntSet.elements" "with min_int and max_int" (ints (IntSet.elements (IntSet.of_list xs)));
  line "IntSet.mem min_int, max_int, 7" "" (Printf.sprintf "%b %b %b" (IntSet.mem (IntSet.of_list xs) min_int) (IntSet.mem (IntSet.of_list xs) max_int) (IntSet.mem (IntSet.of_list xs) 7));
  let t = List.fold_left (fun t x -> IntTable.add t x (x = 3)) IntTable.empty xs in
  line "IntTable.keys" "" (ints (IntTable.keys t));
  line "IntTable.find min_int" "" (guard (fun () -> string_of_bool (IntTable.find t min_int)));
  let m = List.fold_left (fun m (k, v) -> IntMTable.add m k v) IntMTable.empty [ max_int, "a"; min_int, "b"; max_int, "c" ] in
  line "IntMTable keys, find_all max_int" "" (ints (IntMTable.keys m) ^ " " ^ list str (IntMTable.find_all m max_int))
