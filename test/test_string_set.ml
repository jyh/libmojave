(* Lm_string_set: string sets and maps under two orders (StringSet etc.
   order by length first, LexStringSet etc. by Stdlib.compare), and the
   multi-value maps (LmMakeList), which no other test exercises. *)
open Pin
open Lm_string_set

let words = [ "pear"; "fig"; "apple"; "kiwi"; "a"; "banana"; "fig" ]
let ints = list string_of_int

let () =
  line "StringSet.elements" "words" (list str (StringSet.elements (StringSet.of_list words)));
  line "LexStringSet.elements" "words" (list str (LexStringSet.elements (LexStringSet.of_list words)));
  let t = List.fold_left (fun t w -> StringTable.add t w (String.length w)) StringTable.empty words in
  line "StringTable keys" "words -> length" (list str (StringTable.keys t));
  line "StringTable.find fig" "" (guard (fun () -> string_of_int (StringTable.find t "fig")));
  line "StringTable.find plum" "" (guard (fun () -> string_of_int (StringTable.find t "plum")));
  let lt = List.fold_left (fun t w -> LexStringTable.add t w (String.length w)) LexStringTable.empty words in
  line "LexStringTable keys" "words -> length" (list str (LexStringTable.keys lt));

  (* multi-value tables *)
  let m = List.fold_left (fun m (k, v) -> StringMTable.add m k v) StringMTable.empty
      [ "x", 1; "y", 2; "x", 3; "zz", 4; "x", 5 ] in
  line "MTable add x1 y2 x3 zz4 x5; cardinal" "" (string_of_int (StringMTable.cardinal m));
  line "  find x" "" (guard (fun () -> string_of_int (StringMTable.find m "x")));
  line "  find_all x" "" (ints (StringMTable.find_all m "x"));
  line "  find_all w (absent)" "" (guard (fun () -> ints (StringMTable.find_all m "w")));
  line "  find_all_partial w (absent)" "" (guard (fun () -> ints (StringMTable.find_all_partial m "w")));
  line "  keys" "" (list str (StringMTable.keys m));
  line "  data" "" (ints (StringMTable.data m));
  line "  data_all" "" (list ints (StringMTable.data_all m));
  line "  choose_all" "" (let k, l = StringMTable.choose_all m in k ^ " " ^ ints l);
  let r = StringMTable.remove m "x" in
  line "  remove x; find_all x" "" (guard (fun () -> ints (StringMTable.find_all r "x")));
  line "  remove x; cardinal" "" (string_of_int (StringMTable.cardinal r));
  let f = StringMTable.filter m "x" (List.filter (fun v -> v > 1)) in
  line "  filter x (> 1); find_all x" "" (ints (StringMTable.find_all f "x"));
  let f = StringMTable.filter m "x" (fun _ -> []) in
  line "  filter x to []; mem x" "" (string_of_bool (StringMTable.mem f "x"));
  let acc = ref [] in
  StringMTable.iter_all (fun k l -> acc := (k ^ ":" ^ ints l) :: !acc) m;
  line "  iter_all" "" (list (fun s -> s) (List.rev !acc));
  let acc = ref [] in
  StringMTable.iter (fun k v -> acc := (k ^ "=" ^ string_of_int v) :: !acc) m;
  line "  iter" "" (list (fun s -> s) (List.rev !acc));
  line "  fold_all (count values)" "" (string_of_int (StringMTable.fold_all (fun n _ l -> n + List.length l) 0 m));
  line "  mapi_all (double, drop y)" "" (list ints (StringMTable.data_all (StringMTable.mapi_all (fun k l -> if k = "y" then [] else List.map (( * ) 2) l) m)));
  let e = StringMTable.mapi_all (fun k l -> if k = "y" then [] else l) m in
  line "  mapi_all leaving y with []: mem y, cardinal" "" (Printf.sprintf "%b %d" (StringMTable.mem e "y") (StringMTable.cardinal e));
  line "  ... find y" "" (guard (fun () -> string_of_int (StringMTable.find e "y")));
  line "  ... find_all y" "" (guard (fun () -> ints (StringMTable.find_all e "y")));
  line "  ... data" "" (guard (fun () -> ints (StringMTable.data e)));
  let m2 = StringMTable.add (StringMTable.add StringMTable.empty "x" 10) "q" 11 in
  let u = StringMTable.union (fun _ a b -> a @ b) m m2 in
  line "  union (a @ b) with x10 q11; elements via find_all" ""
    (list (fun k -> k ^ ":" ^ ints (StringMTable.find_all u k)) (StringMTable.keys u));
  let lm = List.fold_left (fun m (k, v) -> LexStringMTable.add m k v) LexStringMTable.empty
      [ "x", 1; "y", 2; "x", 3; "zz", 4; "x", 5 ] in
  line "LexStringMTable keys, find_all x" "" (list str (LexStringMTable.keys lm) ^ " " ^ ints (LexStringMTable.find_all lm "x"))
