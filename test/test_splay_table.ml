open Pin

module Base = struct
  type elt = int
  type data = string
  let print out k l = Lm_printf.fprintf out "%d:%s;" k (String.concat "," l)
  let compare = compare
  let append = ( @ )
end

module T = Lm_splay_table.MakeTable (Base)

let i = string_of_int
let ks l = list i l
let entries t = list (fun (k, ds) -> i k ^ "->" ^ list str ds) (T.list_of t)
let build l = List.fold_left (fun t k -> T.add t k ("v" ^ i k)) T.empty l

(* Insertion orders, including a repeated key. *)
let orders = [ []; [ 1 ]; [ 3; 1; 2 ]; [ 1; 2; 3; 4; 5 ]; [ 5; 4; 3; 2; 1 ]; [ 2; 1; 2; 3; 2 ] ]

let each name f = List.iter (fun l -> line name (ks l) (guard (fun () -> f (build l)))) orders

let () =
  each "list_of" entries;
  each "length" (fun t -> i (T.length t));
  each "is_empty" (fun t -> string_of_bool (T.is_empty t));
  each "mem 2" (fun t -> string_of_bool (T.mem t 2));
  each "find 2" (fun t -> str (T.find t 2));
  each "find_all 2" (fun t -> list str (T.find_all t 2));
  each "remove 2" (fun t -> entries (T.remove t 2));
  each "remove 9" (fun t -> entries (T.remove t 9));
  each "replace 2 [x; y]" (fun t -> entries (T.replace t 2 [ "x"; "y" ]));
  each "add 2 w" (fun t -> entries (T.add t 2 "w"));
  each "map (k^d)" (fun t -> entries (T.map (fun k d -> i k ^ d) t));
  each "iter" (fun t -> let r = ref [] in T.iter (fun k d -> r := (i k ^ "=" ^ d) :: !r) t; list str (List.rev !r));
  each "deletemax" (fun t -> let k, ds, t' = T.deletemax t in i k ^ " " ^ list str ds ^ " rest " ^ entries t');
  each "fold_map (keep even keys)" (fun t ->
      entries (T.fold_map (fun k d acc -> if k mod 2 = 0 then T.add acc k d else acc) T.empty t));
  each "print" (fun t ->
      let b = Buffer.create 16 in
      let fmt = Lm_printf.formatter_of_buffer b in
      T.print fmt t; Lm_printf.pp_print_flush fmt (); str (Buffer.contents b));
  List.iter (fun (a, c) -> line "union" (ks a ^ " " ^ ks c) (guard (fun () -> entries (T.union (build a) (build c)))))
    [ ([], []); ([ 1; 2 ], []); ([], [ 1; 2 ]); ([ 1; 2 ], [ 2; 3 ]); ([ 3; 1 ], [ 1; 3; 5 ]) ];
  (* A larger table: every key found after many insertions and removals. *)
  let big = build (List.init 200 (fun k -> (k * 37) mod 200)) in
  let big = List.fold_left (fun t k -> T.remove t k) big (List.init 50 (fun k -> k * 4)) in
  line "big: length after 200 adds and 50 removes" "" (i (T.length big));
  line "big: all remaining keys found" ""
    (string_of_bool (List.for_all (fun k -> (k mod 4 = 0 && k < 200) <> T.mem big k) (List.init 200 (fun k -> k))));
  line "big: list_of sorted" "" (string_of_bool (let l = List.map fst (T.list_of big) in l = List.sort compare l))
