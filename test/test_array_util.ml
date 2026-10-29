open Pin
module A = Lm_array_util

let i = string_of_int
let b = string_of_bool
let ia a = list i (Array.to_list a)
let arrays = [ [||]; [| 1 |]; [| 1; 2; 3 |]; [| 3; 1; 2; 1 |] ]
let each name f = List.iter (fun a -> line name (ia a) (guard (fun () -> f a))) arrays
let each2 name f =
  List.iter (fun x -> List.iter (fun y -> line name (ia x ^ " " ^ ia y) (guard (fun () -> f x y))) arrays) arrays
let even x = x mod 2 = 0

let () =
  each "mem 1" (fun a -> b (A.mem 1 a));
  each "index 1" (fun a -> i (A.index 1 a));
  each "find_index even" (fun a -> i (A.find_index even a));
  each "for_all even" (fun a -> b (A.for_all even a));
  each "exists even" (fun a -> b (A.exists even a));
  List.iter (fun a -> line "all_true" (list b (Array.to_list a)) (b (A.all_true a)))
    [ [||]; [| true |]; [| true; false |]; [| false; false |] ];
  List.iter (fun a -> line "exists_true" (list b (Array.to_list a)) (b (A.exists_true a)))
    [ [||]; [| true |]; [| true; false |]; [| false; false |] ];
  each2 "for_all2 (<=)" (fun x y -> b (A.for_all2 ( <= ) x y));
  each2 "iter2" (fun x y -> let r = ref [] in A.iter2 (fun p q -> r := (p + q) :: !r) x y; list i (List.rev !r));
  (* replace a i n l: elements i .. i+n-1 replaced by the list l *)
  List.iter (fun a -> List.iter (fun (s, n, l) ->
      line "replace" (ia a ^ " " ^ i s ^ " " ^ i n ^ " " ^ list i l) (guard (fun () -> ia (A.replace a s n l))))
      [ (0, 0, [ 9 ]); (0, 1, [ 9 ]); (1, 2, []); (1, 1, [ 8; 9 ]); (2, 5, [ 9 ]); (-1, 1, [ 9 ]) ]) arrays;
  each "append_list [7; 8]" (fun a -> ia (A.append_list a [ 7; 8 ]));
  each "append_list []" (fun a -> ia (A.append_list a []));
  each "append_list_array [7] [|5; 6|]" (fun a -> ia (A.append_list_array a [ 7 ] [| 5; 6 |]));
  List.iter (fun a -> List.iter (fun (s, n) ->
      line "sub_map (*10)" (ia a ^ " " ^ i s ^ " " ^ i n) (guard (fun () -> ia (A.sub_map (fun x -> x * 10) a s n))))
      [ (0, 0); (0, 2); (1, 2); (2, 5); (-1, 1) ]) arrays;
  let src = [| 10; 20; 30; 40 |] in
  List.iter (fun (name, parts) -> line "collect" name (guard (fun () -> ia (A.collect parts))))
    [ ("[]", []);
      ("[E 1]", [ A.ArrayElement 1 ]);
      ("[A src 1 2]", [ A.ArrayArray (src, 1, 2) ]);
      ("[E 1; A src 0 4; E 2]", [ A.ArrayElement 1; A.ArrayArray (src, 0, 4); A.ArrayElement 2 ]);
      ("[A src 3 0; E 5]", [ A.ArrayArray (src, 3, 0); A.ArrayElement 5 ]);
      ("[A src 3 2]", [ A.ArrayArray (src, 3, 2) ]) ];
  (* distinct sorts the array in place and moves the distinct elements to the front *)
  List.iter (fun a ->
      let c = Array.copy a in
      line "distinct compare" (ia a) (guard (fun () -> let n = A.distinct compare c in i n ^ " " ^ ia c)))
    (arrays @ [ [| 2; 2; 2 |]; [| 5; 1; 5; 3; 1 |] ])
