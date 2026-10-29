(* Lm_array_linear_set and Lm_splay_linear_set against a list model, on
   the same generated cases (a fixed LCG). Each line counts the cases
   where an implementation disagrees with the model, with the first. *)
open Lm_array_util

let seed = ref 4242
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n
let gen_list () = List.init (rnd 9) (fun _ -> rnd 50)
let sub l i n = List.filteri (fun k _ -> k >= i && k < i + n) l
let ints l = "[" ^ String.concat ";" (List.map string_of_int l) ^ "]"

module Check (L : Lm_linear_set_sig.LinearSetSig with type elt = int) (N : sig val name : string end) = struct
  let fails = Hashtbl.create 16 and first = Hashtbl.create 16
  let check op ok detail =
    let ok = try ok () with _ -> false in
    if not ok then begin
      Hashtbl.replace fails op (1 + try Hashtbl.find fails op with Not_found -> 0);
      if not (Hashtbl.mem first op) then Hashtbl.replace first op (detail ())
    end else if not (Hashtbl.mem fails op) then Hashtbl.replace fails op 0
  let lst t = L.to_list t
  let run cases =
    for _ = 1 to cases do
      let a = gen_list () and b = gen_list () in
      let ta = L.of_list a and tb = L.of_list b in
      let d () = ints a ^ " " ^ ints b in
      check "of_list/to_list" (fun () -> lst ta = a) d;
      check "length" (fun () -> L.length ta = List.length a) d;
      check "get" (fun () -> List.for_all (fun i -> L.get ta i = List.nth a i) (List.init (List.length a) Fun.id)) d;
      check "map" (fun () -> lst (L.map (fun x -> x + 1) ta) = List.map (fun x -> x + 1) a) d;
      check "mapi" (fun () -> lst (L.mapi (fun i x -> x * 100 + i) ta) = List.mapi (fun i x -> x * 100 + i) a) d;
      check "fold" (fun () -> L.fold (fun acc i x -> (i, x) :: acc) [] ta = List.rev (List.mapi (fun i x -> i, x) a)) d;
      check "concat" (fun () -> lst (L.concat ta tb) = a @ b) d;
      check "append" (fun () -> lst (L.append ta 7 tb) = a @ [ 7 ] @ b) d;
      check "append_list" (fun () -> lst (L.append_list ta [ 1; 2 ] tb) = a @ [ 1; 2 ] @ b) d;
      let tl = L.append_list ta [ 1; 2; 3 ] tb and ml = a @ [ 1; 2; 3 ] @ b in
      check "get of append_list" (fun () -> List.for_all (fun i -> L.get tl i = List.nth ml i) (List.init (List.length ml) Fun.id)) d;
      check "split of append_list" (fun () ->
          List.for_all (fun i -> let tl = L.append_list ta [ 1; 2; 3 ] tb in
                         let l, x, r = L.split tl i in lst l @ [ x ] @ lst r = ml && x = List.nth ml i) (List.init (List.length ml) Fun.id)) d;
      check "lazy_apply" (fun () -> lst (L.lazy_apply (fun x -> x * 2) ta) = List.map (fun x -> x * 2) a) d;
      check "lazy_apply twice" (fun () -> lst (L.lazy_apply (fun x -> x + 3) (L.lazy_apply (fun x -> x * 2) ta)) = List.map (fun x -> x * 2 + 3) a) d;
      check "for_all/exists" (fun () -> L.for_all (fun x -> x < 25) ta = List.for_all (fun x -> x < 25) a && L.exists (fun x -> x < 5) ta = List.exists (fun x -> x < 5) a) d;
      let n = List.length a in
      if n > 0 then begin
        let i = rnd n in
        check "split" (fun () -> let l, x, r = L.split ta i in lst l = sub a 0 i && x = List.nth a i && lst r = sub a (i + 1) n) (fun () -> d () ^ " i=" ^ string_of_int i);
        check "get/fold of split parts" (fun () ->
            let l, _, r = L.split (L.of_list a) i in
            let gets t = List.init (L.length t) (L.get t) in
            let folds t = List.rev (L.fold (fun acc k x -> (k, x) :: acc) [] t) in
            gets l = sub a 0 i && gets r = sub a (i + 1) n
            && folds l = List.mapi (fun k x -> k, x) (sub a 0 i) && folds r = List.mapi (fun k x -> k, x) (sub a (i + 1) n))
          (fun () -> d () ^ " i=" ^ string_of_int i);
        let len = rnd (n - i + 1) in
        check "lazy_sub_map" (fun () -> lst (L.lazy_sub_map (fun x -> -x) ta i len) = List.map (fun x -> -x) (sub a i len)) (fun () -> d () ^ Printf.sprintf " i=%d len=%d" i len);
        check "get of lazy_sub_map" (fun () -> let t = L.lazy_sub_map (fun x -> -x) ta i len in List.for_all (fun k -> L.get t k = - List.nth a (i + k)) (List.init len Fun.id)) (fun () -> d () ^ Printf.sprintf " i=%d len=%d" i len);
        check "collect" (fun () -> lst (L.collect [ ArrayElement 9; ArrayArray (ta, i, len); ArrayElement 8 ]) = [ 9 ] @ sub a i len @ [ 8 ]) (fun () -> d () ^ Printf.sprintf " i=%d len=%d" i len);
        (* operations on the result of an operation *)
        let tc = L.concat ta tb in
        let j = rnd (n + List.length b) in
        check "split of concat" (fun () -> let l, x, r = L.split tc j in lst l @ [ x ] @ lst r = a @ b) (fun () -> d () ^ " j=" ^ string_of_int j);
        check "get of lazy_apply of concat" (fun () -> L.get (L.lazy_apply (fun x -> x + 1) tc) j = List.nth (a @ b) j + 1) (fun () -> d () ^ " j=" ^ string_of_int j)
      end;
      let k = rnd 6 in
      check "init/make" (fun () -> lst (L.init k (fun i -> i * i)) = List.init k (fun i -> i * i) && lst (L.make k 5) = List.init k (fun _ -> 5)) (fun () -> string_of_int k);
      check "empty/singleton" (fun () -> lst L.empty = [] && lst (L.singleton 3) = [ 3 ]) (fun () -> "")
    done;
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
  A.run 2000;
  seed := s0;
  S.run 2000
