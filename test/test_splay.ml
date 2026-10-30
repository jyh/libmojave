(* Lm_splay (top-down splaying on any ordered tree) against its stated
   semantics: if splay c l x r = l', x', r' then l' x' r' hold the same
   elements in the same order as l x r, every element of l' is below the
   cut and every element of r' above it (an element at the cut, if there
   is one, is x'), and l', r' are still search trees. splay_max returns
   the largest element and the rest; splay_min the smallest and the rest.
   Trees are random search trees over distinct ints (insertion of a fixed
   LCG's keys); cuts are an element (compare y), a point between elements
   (2y + 1 against doubled keys), or all-below / all-above. Per check:
   how many of N cases fail, and the first.
   Then Lm_rformat_raw's channel printer against its buffer printer, and
   Lm_thread_util's printer lock taken and released twice. *)
open Pin
module S = Lm_splay

let seed = ref 67
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 14) mod n

let rec insert k = function
  | S.Leaf -> S.Node (S.Leaf, k, S.Leaf)
  | S.Node (l, x, r) as t ->
      if k < x then S.Node (insert k l, x, r) else if k > x then S.Node (l, x, insert k r) else t
let rec elems acc = function S.Leaf -> acc | S.Node (l, x, r) -> elems (x :: elems acc r) l
let to_list t = elems [] t
let rec bst lo hi = function
  | S.Leaf -> true
  | S.Node (l, x, r) -> lo < x && x < hi && bst lo x l && bst x hi r
let rec depth = function S.Leaf -> 0 | S.Node (l, _, r) -> 1 + max (depth l) (depth r)

(* a non-empty tree of keys 2, 4, ..., so 2y+1 cuts fall between them *)
let tree () =
  let n = 1 + rnd 40 in
  let rec go t i = if i = 0 then t else go (insert (2 * (1 + rnd 200)) t) (i - 1) in
  go S.Leaf n

let show_list l = "[" ^ String.concat " " (List.map string_of_int l) ^ "]"

let check name n case =
  seed := 67;
  let bad = ref 0 and first = ref "" in
  for _ = 1 to n do
    match case () with
    | true, _ -> ()
    | false, desc -> incr bad; if !first = "" then first := desc ()
  done;
  line (name ^ ": cases that fail") (string_of_int n ^ " trees")
    (Printf.sprintf "%d%s" !bad (if !bad = 0 then "" else " (first: " ^ !first ^ ")"))

let () =
  let n = 2000 in
  check "splay keeps elements, order, and the cut" n (fun () ->
      match tree () with
      | S.Leaf -> true, (fun () -> "")
      | S.Node (l, x, r) as t ->
          let all = to_list t in
          let kind = rnd 4 in
          let a = List.nth all (rnd (List.length all)) in
          (* cut c y: <0 when the cut is below y (y lies to the right) *)
          let c, cname = match kind with
            | 0 -> (fun y -> compare a y), Printf.sprintf "at %d" a
            | 1 -> (fun y -> compare (a + 1) y), Printf.sprintf "between %d and above" a
            | 2 -> (fun _ -> -1), "below all"
            | _ -> (fun _ -> 1), "above all" in
          let l', x', r' = S.splay c l x r in
          let same = to_list (S.Node (l', x', r')) = all in
          let left_ok = List.for_all (fun y -> c y > 0) (to_list l') in
          let right_ok = List.for_all (fun y -> c y < 0) (to_list r') in
          let hit_ok = not (List.exists (fun y -> c y = 0) all) || c x' = 0 in
          let trees_ok = bst min_int x' l' && bst x' max_int r' in
          same && left_ok && right_ok && hit_ok && trees_ok,
          (fun () -> Printf.sprintf "%s, cut %s: left %s, top %d, right %s" (show_list all) cname
                       (show_list (to_list l')) x' (show_list (to_list r'))));
  check "splay_max: the largest, and the rest in order" n (fun () ->
      match tree () with
      | S.Leaf -> true, (fun () -> "")
      | S.Node (l, x, r) as t ->
          let all = to_list t in
          let l', m = S.splay_max l x r in
          to_list l' @ [ m ] = all && bst min_int m l',
          (fun () -> Printf.sprintf "%s: rest %s, max %d" (show_list all) (show_list (to_list l')) m));
  check "splay_min: the smallest, and the rest in order" n (fun () ->
      match tree () with
      | S.Leaf -> true, (fun () -> "")
      | S.Node (l, x, r) as t ->
          let all = to_list t in
          let m, r' = S.splay_min l x r in
          m :: to_list r' = all && bst m max_int r',
          (fun () -> Printf.sprintf "%s: min %d, rest %s" (show_list all) m (show_list (to_list r'))));
  (* splaying a degenerate (list-shaped) tree at its far end roughly halves its depth *)
  let chain = List.fold_left (fun t k -> insert k t) S.Leaf (List.init 64 (fun i -> 2 * (i + 1))) in
  (match chain with
   | S.Node (l, x, r) ->
       let l', x', r' = S.splay (fun y -> compare 128 y) l x r in
       line "splay a 64-node chain at its deepest key: depth before, top, depth after" ""
         (Printf.sprintf "%d %d %d" (depth chain) x' (depth (S.Node (l', x', r'))))
   | S.Leaf -> ());
  (* Lm_rformat_raw: the channel printer writes what the buffer printer does *)
  let drive (p : Lm_rformat_raw.raw_printer) =
    p.raw_print_string "xhello worldx" 1 11; p.raw_print_spaces 3; p.raw_print_string "!" 0 1;
    p.raw_print_newline (); p.raw_print_spaces 0; p.raw_print_string "" 0 0; p.raw_print_flush () in
  let b = Buffer.create 16 in
  drive (Lm_rformat_raw.raw_buffer_printer b);
  let file = Filename.temp_file "test_splay" ".txt" in
  let oc = open_out_bin file in
  drive (Lm_rformat_raw.raw_channel_printer oc);
  close_out oc;
  let ic = open_in_bin file in
  let from_file = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Sys.remove file;
  line "raw_buffer_printer output" "" (str (Buffer.contents b));
  line "raw_channel_printer writes the same" "" (string_of_bool (from_file = Buffer.contents b));
  (* Lm_thread_util: the printer lock can be taken and released, twice *)
  line "lock_printer; unlock_printer, twice" "" (guard (fun () ->
      Lm_thread_util.lock_printer (); Lm_thread_util.unlock_printer ();
      Lm_thread_util.lock_printer (); Lm_thread_util.unlock_printer (); "ok"))
