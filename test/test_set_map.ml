open Pin
module SS = Lm_set.LmMake (struct type t = string let compare = compare end)
module SM = Lm_map.LmMake (struct type t = string let compare = compare end)
module IS = Lm_set.LmMake (struct type t = int let compare = compare end)

let ss s = list str (SS.elements s)
let is s = list string_of_int (IS.elements s)

let () =
  let a = SS.of_list [ "b"; "a"; "c"; "a" ] and b = SS.of_list [ "c"; "d" ] in
  line "SS.of_list" "[b;a;c;a]" (ss a);
  line "SS.cardinal" "a" (string_of_int (SS.cardinal a));
  line "SS.union" "a b" (ss (SS.union a b));
  line "SS.inter" "a b" (ss (SS.inter a b));
  line "SS.diff" "a b" (ss (SS.diff a b));
  line "SS.subset" "{c} a" (string_of_bool (SS.subset (SS.of_list [ "c" ]) a));
  line "SS.mem" "a \"b\"" (string_of_bool (SS.mem a "b"));
  line "SS.remove" "a \"b\"" (ss (SS.remove a "b"));
  line "SS.remove absent" "a \"z\"" (ss (SS.remove a "z"));
  line "SS.min/max" "a" (SS.min_elt a ^ " " ^ SS.max_elt a);
  line "SS.min_elt" "empty" (guard (fun () -> SS.min_elt SS.empty));
  line "SS.compare sign" "a b" (string_of_int (compare (SS.compare a b) 0));
  line "SS.equal" "a (of_list [c;b;a])" (string_of_bool (SS.equal a (SS.of_list [ "c"; "b"; "a" ])));
  line "SS.fold concat" "a" (SS.fold (fun acc x -> acc ^ x) "" a);
  let evens, odds = IS.partition (fun x -> x mod 2 = 0) (IS.of_list [ 5; 1; 4; 2; 3 ]) in
  line "IS.partition even" "[5;1;4;2;3]" (is evens ^ " " ^ is odds);
  line "IS.filter >2" "[5;1;4;2;3]" (is (IS.filter (fun x -> x > 2) (IS.of_list [ 5; 1; 4; 2; 3 ])));
  let big = List.fold_left IS.add IS.empty (List.init 1000 (fun i -> (i * 7919) mod 1000)) in
  line "IS 1000 permuted adds" "cardinal min max" (Printf.sprintf "%d %d %d" (IS.cardinal big) (IS.min_elt big) (IS.max_elt big));
  let m = List.fold_left (fun m (k, v) -> SM.add m k v) SM.empty [ ("b", 2); ("a", 1); ("c", 3); ("a", 10) ] in
  let sm m = list (fun (k, v) -> k ^ "=" ^ string_of_int v) (SM.fold (fun acc k v -> (k, v) :: acc) [] m |> List.rev) in
  line "SM.add (a twice)" "" (sm m);
  line "SM.find" "\"a\"" (string_of_int (SM.find m "a"));
  line "SM.find absent" "\"z\"" (guard (fun () -> string_of_int (SM.find m "z")));
  line "SM.keys" "" (list str (SM.keys m));
  line "SM.data" "" (list string_of_int (SM.data m));
  line "SM.remove" "\"b\"" (sm (SM.remove m "b"));
  line "SM.filter_add" "\"a\" +1, \"z\" new" (sm (SM.filter_add (SM.filter_add m "a" (function Some v -> v + 1 | None -> 0)) "z" (function Some v -> v | None -> 99)));
  line "SM.union sum" "" (sm (SM.union (fun _ x y -> x + y) m (SM.add SM.empty "a" 5)));
  line "SM.cardinal" "" (string_of_int (SM.cardinal m))
