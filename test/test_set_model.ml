(* Lm_set against Stdlib.Set on random histories, from a fixed LCG: the
   operations the 2026-11-02 coverage run never reached (add_list,
   subtract_list, range_fold, for_all, exists, and the backwards-compatible
   Make's mem, add, remove, fold and partition).

   300 histories of up to 30 steps over small ints, applied to LmMake and
   to Make side by side with a Stdlib.Set; after each step the elements
   are compared, then each query: range_fold over a random interval
   [lo, hi] (range k is 1 below lo, -1 above hi, 0 inside: the fold visits
   the elements inside, in order), for_all and exists of a random
   predicate, Make.fold (the element first), Make.partition (S declares it
   to return the elements that satisfy the predicate). One line per
   operation: how many steps disagree, and the first. *)
open Pin
module L = Lm_set.LmMake (Int)
module M = Lm_set.Make (Int)
module R = Set.Make (Int)

let seed = ref 280
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n

let bad = Hashtbl.create 8 and first = Hashtbl.create 8 and order = ref [] and steps = Hashtbl.create 8
let check op ok d =
  if not (List.mem op !order) then order := op :: !order;
  Hashtbl.replace steps op (1 + try Hashtbl.find steps op with Not_found -> 0);
  if not ok then begin
    Hashtbl.replace bad op (1 + try Hashtbl.find bad op with Not_found -> 0);
    if not (Hashtbl.mem first op) then Hashtbl.replace first op (d ())
  end

let il l = list string_of_int l
let keys () = List.init (rnd 8) (fun _ -> rnd 40)

let () =
  for _ = 1 to 300 do
    let l = ref L.empty and m = ref M.empty and r = ref R.empty and hist = ref [] in
    for _ = 1 to 1 + rnd 30 do
      let ks = keys () and k = rnd 40 in
      let step =
        match rnd 6 with
        | 0 -> l := L.add_list !l ks; m := List.fold_left (fun s x -> M.add x s) !m ks;
          r := List.fold_left (fun s x -> R.add x s) !r ks; "add_list " ^ il ks
        | 1 -> l := L.subtract_list !l ks; m := List.fold_left (fun s x -> M.remove x s) !m ks;
          r := List.fold_left (fun s x -> R.remove x s) !r ks; "subtract_list " ^ il ks
        | 2 | 3 -> l := L.add !l k; m := M.add k !m; r := R.add k !r; "add " ^ string_of_int k
        | _ -> l := L.remove !l k; m := M.remove k !m; r := R.remove k !r; "remove " ^ string_of_int k in
      hist := step :: !hist;
      let h () = String.concat ", " (List.rev !hist) in
      let want = R.elements !r in
      check "LmMake add_list / subtract_list / add / remove: elements" (L.elements !l = want)
        (fun () -> h () ^ ": " ^ il (L.elements !l) ^ " vs " ^ il want);
      check "Make add / remove: elements" (M.elements !m = want)
        (fun () -> h () ^ ": " ^ il (M.elements !m) ^ " vs " ^ il want);
      let lo = rnd 40 in
      let hi = lo + rnd 15 in
      let range x = if x < lo then 1 else if x > hi then -1 else 0 in
      let got = List.rev (L.range_fold range (fun acc x -> x :: acc) [] !l) in
      let rw = List.filter (fun x -> x >= lo && x <= hi) want in
      check "range_fold" (got = rw) (fun () -> Printf.sprintf "%s, [%d, %d]: %s vs %s" (h ()) lo hi (il got) (il rw));
      let d = 1 + rnd 5 in
      let p x = x mod d = 0 in
      check "for_all" (L.for_all p !l = R.for_all p !r && M.for_all p !m = R.for_all p !r) (fun () -> h ());
      check "exists" (L.exists p !l = R.exists p !r && M.exists p !m = R.exists p !r) (fun () -> h ());
      check "Make mem" (M.mem k !m = R.mem k !r) (fun () -> h () ^ " mem " ^ string_of_int k);
      check "Make fold (element first)" (List.rev (M.fold (fun x acc -> x :: acc) !m []) = want) (fun () -> h ());
      check "Make partition = the elements that satisfy" (M.elements (M.partition p !m) = List.filter p want) (fun () -> h ())
    done
  done;
  List.iter (fun op ->
      let k = try Hashtbl.find bad op with Not_found -> 0 in
      line op (string_of_int (Hashtbl.find steps op) ^ " steps")
        (if k = 0 then "0 disagree" else Printf.sprintf "%d disagree (first: %s)" k (Hashtbl.find first op)))
    (List.rev !order)
