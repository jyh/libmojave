open Pin
module L = Lm_list_util

let il = list string_of_int
let ill = list il
let pair f g (a, b) = "(" ^ f a ^ ", " ^ g b ^ ")"
let ip = pair string_of_int string_of_int
let ipl = list ip
let b = string_of_bool
let i = string_of_int

let lists = [ []; [ 1 ]; [ 1; 2; 3 ]; [ 3; 1; 2; 1 ]; [ 5; 4; 3; 2; 1 ] ]
let idx = [ -1; 0; 1; 2; 5 ]

let each name f = List.iter (fun l -> line name (il l) (guard (fun () -> f l))) lists
let each_n name f =
  List.iter (fun l -> List.iter (fun n -> line name (i n ^ " " ^ il l) (guard (fun () -> f n l))) idx) lists
let each2 name f =
  List.iter (fun l1 -> List.iter (fun l2 -> line name (il l1 ^ " " ^ il l2) (guard (fun () -> f l1 l2))) lists) lists

let even x = x mod 2 = 0

let () =
  (* slicing and positions *)
  List.iter (fun l -> List.iter (fun (a, n) -> line "sub" (il l ^ " " ^ i a ^ " " ^ i n) (guard (fun () -> il (L.sub l a n))))
    [ (0, 0); (0, 2); (1, 2); (2, 5); (-1, 1) ]) lists;
  each_n "split_list" (fun n l -> pair il il (L.split_list n l));
  each_n "split" (fun n l -> pair il il (L.split n l));
  each_n "splitup" (fun n l -> ill (L.splitup n l));
  each_n "firstn" (fun n l -> il (L.firstn n l));
  each_n "nth_tl" (fun n l -> il (L.nth_tl n l));
  each_n "replace_nth 9" (fun n l -> il (L.replace_nth n 9 l));
  each_n "insert_nth 9" (fun n l -> il (L.insert_nth n 9 l));
  each_n "remove_nth" (fun n l -> il (L.remove_nth n l));
  each_n "replacef_nth (+100)" (fun n l -> il (L.replacef_nth n (fun x -> x + 100) l));
  each_n "replacef_arg_nth" (fun n l -> pair il i (L.replacef_arg_nth n (fun x -> (x * 10, x)) l));
  each_n "nth" (fun n l -> i (L.nth l n));
  each "split_last" (fun l -> pair il i (L.split_last l));
  each "last" (fun l -> i (L.last l));
  (* searching *)
  each "find even" (fun l -> i (L.find even l));
  each "find_item even" (fun l -> i (L.find_item even l));
  each "find_index 1" (fun l -> i (L.find_index 1 l));
  each "find_rindex 1" (fun l -> i (L.find_rindex 1 l));
  each "find_indexq 1" (fun l -> i (L.find_indexq 1 l));
  each "find_index_eq (=) 2" (fun l -> i (L.find_index_eq ( = ) 2 l));
  (* maps and folds *)
  each "flat_map (dup)" (fun l -> il (L.flat_map (fun x -> [ x; x ]) l));
  each "fail_map (100/x-1)" (fun l -> il (L.fail_map (fun x -> 100 / (x - 1)) l));
  each "some_map (even->x)" (fun l -> il (L.some_map (fun x -> if even x then Some x else None) l));
  each "some_map_safe (even->x*10)" (fun l -> il (L.some_map_safe (fun x -> if even x then Some (x * 10) else None) l));
  each "fold_left (sum, x*2)" (fun l -> pair i il (L.fold_left (fun a x -> (a + x, x * 2)) 0 l));
  each "fold_map (sum, a)" (fun l -> pair i il (L.fold_map (fun a x -> (a + x, a)) 0 l));
  each "fold_map1to2" (fun l -> let a, x, y = L.fold_map1to2 (fun a x -> (a + 1, x, -x)) 0 l in i a ^ " " ^ il x ^ " " ^ il y);
  each "fold_filter (count, even)" (fun l -> pair i il (L.fold_filter (fun a x -> (a + 1, even x)) 0 l));
  each "rev_map (+1)" (fun l -> il (L.rev_map (fun x -> x + 1) l));
  each "smap (id)" (fun l -> b (L.smap (fun x -> x) l == l));
  each "smap (+0 on evens: identity values)" (fun l -> il (L.smap (fun x -> if even x then x + 0 else x) l));
  each "filter even" (fun l -> il (L.filter even l));
  each "rev_iter" (fun l -> let r = ref [] in L.rev_iter (fun x -> r := x :: !r) l; il !r);
  (* two- and three-list operations *)
  each2 "map2 (+)" (fun a c -> il (L.map2 ( + ) a c));
  each2 "map2to1 (+)" (fun a c -> il (L.map2to1 ( + ) a c));
  each2 "fold_left2 (a+x*y)" (fun a c -> i (L.fold_left2 (fun s x y -> s + (x * y)) 0 a c));
  each2 "for_all2 (<=)" (fun a c -> b (L.for_all2 ( <= ) a c));
  each2 "exists2 (=)" (fun a c -> b (L.exists2 ( = ) a c));
  each2 "short_iter2" (fun a c -> let r = ref [] in L.short_iter2 (fun x y -> r := (x, y) :: !r) a c; ipl (List.rev !r));
  each2 "iter2" (fun a c -> let r = ref [] in L.iter2 (fun x y -> r := (x, y) :: !r) a c; ipl (List.rev !r));
  each2 "rev_iter2" (fun a c -> let r = ref [] in L.rev_iter2 (fun x y -> r := (x, y) :: !r) a c; ipl (List.rev !r));
  each2 "zip" (fun a c -> ipl (L.zip a c));
  each2 "fold_map2to1" (fun a c -> pair i il (L.fold_map2to1 (fun s x y -> (s + 1, x - y)) 0 a c));
  each2 "length_eq" (fun a c -> b (L.length_eq a c));
  each2 "compare_lists" (fun a c -> i (compare (L.compare_lists compare a c) 0));
  each2 "compare_eq" (fun a c -> b (L.compare_eq a c));
  each2 "compare_cmp (=)" (fun a c -> b (L.compare_cmp ( = ) a c));
  List.iter (fun (x, y, z) -> line "iter3" (il x ^ " " ^ il y ^ " " ^ il z)
    (guard (fun () -> let r = ref [] in L.iter3 (fun a c d -> r := (a + c + d) :: !r) x y z; il (List.rev !r))))
    [ ([], [], []); ([ 1 ], [ 2 ], [ 3 ]); ([ 1; 2 ], [ 3; 4 ], [ 5 ]) ];
  List.iter (fun (x, y, z) -> line "fold_left3" (il x ^ " " ^ il y ^ " " ^ il z)
    (guard (fun () -> i (L.fold_left3 (fun s a c d -> s + (a * c * d)) 0 x y z))))
    [ ([], [], []); ([ 1; 2 ], [ 3; 4 ], [ 5; 6 ]); ([ 1 ], [ 2; 3 ], [ 4 ]) ];
  (* set-like operations *)
  each2 "intersect" (fun a c -> il (L.intersect a c));
  each2 "intersectq" (fun a c -> il (L.intersectq a c));
  each2 "intersects" (fun a c -> b (L.intersects a c));
  each2 "subtract" (fun a c -> il (L.subtract a c));
  each2 "subtractq" (fun a c -> il (L.subtractq a c));
  each2 "subtract_list" (fun a c -> il (L.subtract_list a c));
  each2 "subtract_multiset" (fun a c -> il (L.subtract_multiset a c));
  each2 "union" (fun a c -> il (L.union a c));
  each2 "unionq" (fun a c -> il (L.unionq a c));
  each2 "subset" (fun a c -> b (L.subset a c));
  each2 "remove_suffix" (fun a c -> il (L.remove_suffix a c));
  (* Pinned as they stand, both against their own documentation (CHANGES):
     remove_suffix returns [] where the prefix is expected, and splitup's
     chunks hold n+1 elements, last chunk first. *)
  List.iter (fun (l, sfx) -> line "remove_suffix (explicit)" (il l ^ " " ^ il sfx) (guard (fun () -> il (L.remove_suffix l sfx))))
    [ ([ 1; 2; 3 ], [ 3 ]); ([ 1; 2; 3 ], [ 2; 3 ]); ([ 1; 2; 3 ], []); ([ 1; 2; 3 ], [ 1 ]) ];
  List.iter (fun n -> line "splitup (explicit)" (i n ^ " [1; 2; 3; 4; 5; 6; 7]")
    (guard (fun () -> ill (L.splitup n [ 1; 2; 3; 4; 5; 6; 7 ])))) [ 1; 2; 3 ];
  each "addq 1" (fun l -> il (L.addq 1 l));
  each "addq 9" (fun l -> il (L.addq 9 l));
  each "remove 1" (fun l -> il (L.remove 1 l));
  each "tryremove 1" (fun l -> il (L.tryremove 1 l));
  each "removeq 1" (fun l -> il (L.removeq 1 l));
  each "replaceq 1 7" (fun l -> il (L.replaceq 1 7 l));
  each "replace_first even 0" (fun l -> il (L.replace_first even 0 l));
  each "replace_all even 0" (fun l -> il (L.replace_all even 0 l));
  each "remove_elements [t;f;t]" (fun l -> il (L.remove_elements [ true; false; true ] l));
  (* association lists *)
  let al = [ (1, 10); (2, 20); (1, 11) ] in
  List.iter (fun k -> line "assoc_index" (ipl al ^ " " ^ i k) (guard (fun () -> i (L.assoc_index al k)))) [ 1; 2; 3 ];
  List.iter (fun k -> line "assoc_replace 99" (ipl al ^ " " ^ i k) (guard (fun () -> ipl (L.assoc_replace al k 99)))) [ 1; 2; 3 ];
  List.iter (fun p -> line "add_assoc" (ip p ^ " " ^ ipl al) (guard (fun () -> ipl (L.add_assoc p al)))) [ (1, 10); (1, 12); (3, 30) ];
  List.iter (fun k -> line "assoc_in_dom (=)" (i k) (b (L.assoc_in_dom ( = ) k al))) [ 1; 3 ];
  List.iter (fun v -> line "assoc_in_range (=)" (i v) (b (L.assoc_in_range ( = ) v al))) [ 20; 30 ];
  List.iter (fun k -> line "apply_assoc (+1)" (i k) (guard (fun () -> ipl (L.apply_assoc k al (fun v -> v + 1))))) [ 1; 2; 3 ];
  List.iter (fun k -> line "assoc_eq (=)" (i k) (guard (fun () -> i (L.assoc_eq ( = ) k al)))) [ 1; 2; 3 ];
  line "fst_split" (ipl al) (il (L.fst_split al));
  line "zip_list" "[(0, 0)] [1; 2] [3; 4]" (guard (fun () -> ipl (L.zip_list [ (0, 0) ] [ 1; 2 ] [ 3; 4 ])));
  line "zip_list" "[] [1] [3; 4]" (guard (fun () -> ipl (L.zip_list [] [ 1 ] [ 3; 4 ])));
  line "assoc_append_replace_snd" "[(1, 10); (2, 20)] 0 [(3, 'x')]"
    (guard (fun () -> ipl (L.assoc_append_replace_snd [ (1, 10); (2, 20) ] 0 [ (3, 'x') ])));
  let sl = [ (1, 1); (2, 3); (4, 4) ] in
  List.iter (fun (x, y) -> line "check_assoc" (i x ^ " " ^ i y) (guard (fun () -> b (L.check_assoc x y sl)))) [ (1, 1); (2, 3); (2, 2); (3, 3); (3, 4) ];
  List.iter (fun (x, y) -> line "try_check_assoc" (i x ^ " " ^ i y) (guard (fun () -> b (L.try_check_assoc x y sl)))) [ (1, 1); (2, 3); (2, 2); (3, 3) ];
  List.iter (fun x -> line "try_assoc" (i x) (guard (fun () -> i (L.try_assoc x sl)))) [ 1; 2; 3 ]
