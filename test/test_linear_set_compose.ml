(* Both linear sets on sets built by random histories of operations (as
   metaprl's refiner composes them), against a list model, from a fixed
   LCG. Each set is checked by to_list, get at every index, fold indices,
   length, and split at every index with get on both parts. *)
open Lm_array_util

let seed = ref 2026
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n
let gen () = List.init (rnd 7) (fun _ -> rnd 100)
let sub l i n = List.filteri (fun k _ -> k >= i && k < i + n) l
let ints l = "[" ^ String.concat ";" (List.map string_of_int l) ^ "]"

module Check (L : Lm_linear_set_sig.LinearSetSig with type elt = int) (N : sig val name : string end) = struct
  let rec build depth : L.t * int list * string =
    let leaf () = let l = gen () in L.of_list l, l, "of_list" ^ ints l in
    if depth = 0 then leaf ()
    else
      match rnd 10 with
      | 0 -> leaf ()
      | 1 -> let a, ma, da = build (depth - 1) and b, mb, db = build (depth - 1) in L.concat a b, ma @ mb, "concat(" ^ da ^ "," ^ db ^ ")"
      | 2 -> let a, ma, da = build (depth - 1) and b, mb, db = build (depth - 1) in let x = rnd 100 in L.append a x b, ma @ [ x ] @ mb, "append(" ^ da ^ "," ^ db ^ ")"
      | 3 -> let a, ma, da = build (depth - 1) and b, mb, db = build (depth - 1) in let l = gen () in L.append_list a l b, ma @ l @ mb, "append_list(" ^ da ^ "," ^ db ^ ")"
      | 4 -> let a, ma, da = build (depth - 1) in L.lazy_apply (fun x -> x + 1) a, List.map (fun x -> x + 1) ma, "lazy_apply(" ^ da ^ ")"
      | 5 -> let a, ma, da = build (depth - 1) in let n = List.length ma in
        if n = 0 then a, ma, da else
          let i = rnd n in let len = rnd (n - i + 1) in
          L.lazy_sub_map (fun x -> x * 2) a i len, List.map (fun x -> x * 2) (sub ma i len), Printf.sprintf "lazy_sub_map(%s,%d,%d)" da i len
      | 6 | 7 -> let a, ma, da = build (depth - 1) in let n = List.length ma in
        if n = 0 then a, ma, da else
          let i = rnd n in let l, _, r = L.split a i in
          if rnd 2 = 0 then l, sub ma 0 i, Printf.sprintf "left(%s,%d)" da i else r, sub ma (i + 1) n, Printf.sprintf "right(%s,%d)" da i
      | 8 -> let a, ma, da = build (depth - 1) in L.mapi (fun i x -> x + i) a, List.mapi (fun i x -> x + i) ma, "mapi(" ^ da ^ ")"
      | _ -> let a, ma, da = build (depth - 1) in let n = List.length ma in
        let i = if n = 0 then 0 else rnd n in let len = rnd (n - i + 1) in
        L.collect [ ArrayElement 7; ArrayArray (a, i, len) ], 7 :: sub ma i len, Printf.sprintf "collect(%s,%d,%d)" da i len

  let fails = Hashtbl.create 8 and first = Hashtbl.create 8
  let check op ok d =
    let ok = try ok () with _ -> false in
    if not ok then begin
      Hashtbl.replace fails op (1 + try Hashtbl.find fails op with Not_found -> 0);
      if not (Hashtbl.mem first op) then Hashtbl.replace first op (d ())
    end else if not (Hashtbl.mem fails op) then Hashtbl.replace fails op 0

  let run cases =
    for _ = 1 to cases do
      match build 4 with
      | exception e -> check "build raises" (fun () -> false) (fun () -> Printexc.to_string e)
      | t, m, d ->
        check "build raises" (fun () -> true) (fun () -> "");
        let n = List.length m in
        let dd () = d in
        check "to_list" (fun () -> L.to_list t = m) dd;
        check "length" (fun () -> L.length t = n) dd;
        check "get" (fun () -> List.init n (L.get t) = m) dd;
        check "fold" (fun () -> List.rev (L.fold (fun acc k x -> (k, x) :: acc) [] t) = List.mapi (fun k x -> k, x) m) dd;
        check "split, then get both parts" (fun () ->
            List.for_all (fun i ->
                let l, x, r = L.split t i in
                x = List.nth m i && List.init (L.length l) (L.get l) = sub m 0 i && List.init (L.length r) (L.get r) = sub m (i + 1) n)
              (List.init n Fun.id)) dd
    done;
    (* collect with whole parts placed after other elements (a branch the
       random histories rarely reach) *)
    let whole = ref 0 and whole_first = ref "" in
    List.iter (fun (parts, m) ->
        let ok = try let t = L.collect (parts ()) in L.to_list t = m && List.init (L.length t) (L.get t) = m with _ -> false in
        if not ok then begin incr whole; if !whole_first = "" then whole_first := ints m end)
      [ (fun () -> [ ArrayElement 7; ArrayArray (L.of_list [ 1; 2; 3 ], 0, 3) ]), [ 7; 1; 2; 3 ];
        (fun () -> [ ArrayElement 7; ArrayElement 8; ArrayArray (L.of_list [ 1; 2; 3; 4; 5 ], 0, 5) ]), [ 7; 8; 1; 2; 3; 4; 5 ];
        (fun () -> [ ArrayElement 7; ArrayArray (L.of_list [ 1; 2; 3; 4; 5 ], 0, 4) ]), [ 7; 1; 2; 3; 4 ];
        (fun () -> [ ArrayArray (L.of_list [ 1; 2 ], 0, 2); ArrayArray (L.of_list [ 3; 4; 5 ], 0, 3) ]), [ 1; 2; 3; 4; 5 ] ];
    Printf.printf "%s collect, whole parts: %d of 4 disagree%s\n" N.name !whole (if !whole = 0 then "" else " (first: " ^ !whole_first ^ ")");
    List.iter (fun op ->
        let k = Hashtbl.find fails op in
        Printf.printf "%s %s: %d of %d disagree%s\n" N.name op k cases (if k = 0 then "" else " (first: " ^ Hashtbl.find first op ^ ")"))
      (List.sort compare (Hashtbl.fold (fun k _ acc -> k :: acc) fails []))
end

module T = struct type t = int end
module A = Check (Lm_array_linear_set.Make (T)) (struct let name = "array" end)
module S = Check (Lm_splay_linear_set.Make (T)) (struct let name = "splay" end)

let () =
  let s0 = !seed in
  A.run 1500;
  seed := s0;
  S.run 1500
