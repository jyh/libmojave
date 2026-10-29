(* The MinimalSetSig implementations against a model (a sorted list of
   distinct ints), on the same generated cases (a fixed LCG): Lm_set (the
   control; pinned elsewhere), Lm_splay_set, Lm_hash_set, and Lm_small_set
   over each. Element order is compared after sorting, except in
   "to_list order", which the tree sets keep sorted. (MinimalSetSig does
   not promise an order, and Lm_small_set keeps a small set in insertion
   order, so that line is not checked for it.) *)
open Lm_set_sig

let seed = ref 777
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n
let gen () = let n = if rnd 4 = 0 then 20 + rnd 60 else rnd 12 in List.init n (fun _ -> rnd (if n > 20 then 200 else 30))
let norm l = List.sort_uniq compare l
let ints l = "[" ^ String.concat ";" (List.map string_of_int l) ^ "]"

module Ord = struct type t = int let compare = compare end

module Check (S : MinimalSetSig with type elt = int) (N : sig val name : string val ordered : bool end) = struct
  let fails = Hashtbl.create 16 and first = Hashtbl.create 16
  let check op ok detail =
    let ok = try ok () with _ -> false in
    if not ok then begin
      Hashtbl.replace fails op (1 + try Hashtbl.find fails op with Not_found -> 0);
      if not (Hashtbl.mem first op) then Hashtbl.replace first op (detail ())
    end else if not (Hashtbl.mem fails op) then Hashtbl.replace fails op 0
  let elems s = List.sort compare (S.to_list s)
  let build l = List.fold_left S.add S.empty l
  let run cases =
    for _ = 1 to cases do
      let a = gen () and b = gen () in
      let ma = norm a and mb = norm b in
      let sa = build a and sb = build b in
      let d () = ints a ^ " " ^ ints b in
      check "add/to_list" (fun () -> elems sa = ma) d;
      check "to_list order" (fun () -> (not N.ordered) || S.to_list sa = ma) d;
      check "elements" (fun () -> List.sort compare (S.elements sa) = ma) d;
      check "of_list" (fun () -> elems (S.of_list a) = ma) d;
      check "of_sorted_list" (fun () -> elems (S.of_sorted_list ma) = ma) d;
      check "cardinal" (fun () -> S.cardinal sa = List.length ma) d;
      check "is_empty" (fun () -> S.is_empty sa = (ma = [])) d;
      check "mem" (fun () -> List.for_all (fun x -> S.mem sa x = List.mem x ma) (List.init 32 Fun.id)) d;
      check "union" (fun () -> elems (S.union sa sb) = norm (a @ b)) d;
      check "intersectp" (fun () -> S.intersectp sa sb = List.exists (fun x -> List.mem x mb) ma) d;
      let x = rnd 32 in
      check "remove" (fun () -> elems (S.remove sa x) = List.filter (( <> ) x) ma) (fun () -> d () ^ " x=" ^ string_of_int x);
      check "remove then mem" (fun () -> not (S.mem (S.remove sa x) x)) (fun () -> d () ^ " x=" ^ string_of_int x);
      check "remove all" (fun () -> S.is_empty (List.fold_left S.remove sa a) && S.cardinal (List.fold_left S.remove sa a) = 0) d;
      check "singleton" (fun () -> elems (S.singleton x) = [ x ]) d;
      let acc = ref [] in
      check "iter" (fun () -> S.iter (fun e -> acc := e :: !acc) sa; List.sort compare !acc = ma) d;
      check "mem_filt" (fun () -> S.mem_filt sa b = List.filter (fun y -> List.mem y ma) b) d;
      check "not_mem_filt" (fun () -> S.not_mem_filt sa b = List.filter (fun y -> not (List.mem y ma)) b) d;
      check "fst_mem_filt" (fun () -> let l = List.map (fun y -> y, y * 2) b in S.fst_mem_filt sa l = List.filter (fun (y, _) -> List.mem y ma) l) d;
      check "persistence (add does not change the original)" (fun () -> let _ = S.add sa 31 in elems sa = ma) d
    done;
    List.iter (fun op ->
        let k = Hashtbl.find fails op in
        Printf.printf "%s %s: %d of %d disagree%s\n" N.name op k cases (if k = 0 then "" else " (first: " ^ Hashtbl.find first op ^ ")"))
      (List.sort compare (Hashtbl.fold (fun k _ acc -> k :: acc) fails []))
end

module LmSet = Lm_set.LmMake (Ord)
module Splay = Lm_splay_set.Make (Ord)
module Hash = Lm_hash_set.Make (Ord)
module C1 = Check (LmSet) (struct let name = "Lm_set" let ordered = true end)
module C2 = Check (Splay) (struct let name = "Lm_splay_set" let ordered = true end)
module C3 = Check (Hash) (struct let name = "Lm_hash_set" let ordered = false end)
module C4 = Check (Lm_small_set.Make (LmSet)) (struct let name = "Lm_small_set(Lm_set)" let ordered = false end)
module C5 = Check (Lm_small_set.Make (Splay)) (struct let name = "Lm_small_set(Lm_splay_set)" let ordered = false end)
(* an order other than Stdlib.compare, to catch a polymorphic compare *)
module RevOrd = struct type t = int let compare a b = compare b a end
module SplayRev = Lm_splay_set.Make (RevOrd)
module C6 = Check (SplayRev) (struct let name = "Lm_splay_set (reversed order)" let ordered = false end)

let () =
  let s0 = !seed in
  List.iter (fun run -> seed := s0; run 1500) [ C1.run; C2.run; C3.run; C4.run; C5.run; C6.run ]
