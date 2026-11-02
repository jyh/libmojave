(* Lm_map's functions that no test reached (2026-11-02 bisect_ppx run:
   lm_map.ml at 54 %), against Stdlib.Map as the model, on 300 random maps
   of int keys built by random add / remove sequences:
     - LmMake (IntTable): of_list (from any list, the last binding of a key
       winning, as add would), to_list (elements), equal, find_key,
       forall, exists, find_iter, isect_mem, add_list, union, forall2,
       fold_map, filter_add, filter_remove, replace;
     - LmMakeList (IntMTable, a key to the list of its values, newest
       first): add, find (the newest), find_all, find_all_partial, remove
       (the key), filter_add, filter_remove, replace (newest only), add_list,
       data_all, and fold_map, which must keep each key's values in order.
   Each line prints how many maps agree with the model, and the first
   difference.  The generator is a fixed linear congruential sequence. *)
open Lm_int_set
module M = Map.Make (Int)

let state = ref 1357
let next () = state := (!state * 1103515245 + 12345) land 0x7fffffff; !state lsr 8
let pick n = next () mod n

let tried = Hashtbl.create 32 and ok = Hashtbl.create 32 and first = Hashtbl.create 32 and order = ref []
let note name b what =
  if not (Hashtbl.mem tried name) then order := name :: !order;
  Hashtbl.replace tried name (1 + try Hashtbl.find tried name with Not_found -> 0);
  if b then Hashtbl.replace ok name (1 + try Hashtbl.find ok name with Not_found -> 0)
  else if not (Hashtbl.mem first name) then Hashtbl.replace first name (what ())

let pairs m = M.bindings m
let show l = "[" ^ String.concat "; " (List.map (fun (k, v) -> Printf.sprintf "%d:%d" k v) l) ^ "]"

(* A random map and its model, and the operations that built it. *)
let gen () =
  let n = pick 25 in
  let rec go i t m = if i = 0 then t, m else
      let k = pick 20 and v = pick 100 in
      if pick 4 = 0 then go (i - 1) (IntTable.remove t k) (M.remove k m)
      else go (i - 1) (IntTable.add t k v) (M.add k v m) in
  go n IntTable.empty M.empty

let () =
  for _ = 1 to 300 do
    let t, m = gen () in
    let what () = show (pairs m) in
    note "elements = the model's bindings" (IntTable.elements t = pairs m) what;
    let l = List.init (pick 15) (fun _ -> pick 20, pick 100) in
    let ml = List.fold_left (fun m (k, v) -> M.add k v m) M.empty l in
    note "of_list = the model of the list (last wins)" (IntTable.elements (IntTable.of_list l) = pairs ml)
      (fun () -> "of_list " ^ show l ^ " gave " ^ show (IntTable.elements (IntTable.of_list l)));
    let t2, m2 = gen () in
    note "equal iff the models are equal" (IntTable.equal ( = ) t t2 = M.equal ( = ) m m2) what;
    note "equal to itself rebuilt by of_list" (IntTable.equal ( = ) t (IntTable.of_list (IntTable.elements t))) what;
    let k = pick 20 in
    note "find_key" (IntTable.find_key t k = (if M.mem k m then Some k else None)) what;
    note "forall (v < 90)" (IntTable.forall (fun _ v -> v < 90) t = M.for_all (fun _ v -> v < 90) m) what;
    note "exists (v > 90)" (IntTable.exists (fun _ v -> v > 90) t = M.exists (fun _ v -> v > 90) m) what;
    (* find_iter searches root first, so it returns some match, not the
       first in key order *)
    note "find_iter (an even value, iff there is one)"
      (match IntTable.find_iter (fun _ v -> if v mod 2 = 0 then Some v else None) t with
       | None -> not (M.exists (fun _ v -> v mod 2 = 0) m)
       | Some v -> v mod 2 = 0 && M.exists (fun _ w -> w = v) m) what;
    note "isect_mem (even keys)" (IntTable.elements (IntTable.isect_mem t (fun k -> k mod 2 = 0)) = pairs (M.filter (fun k _ -> k mod 2 = 0) m)) what;
    note "add_list" (IntTable.elements (IntTable.add_list t l) = pairs (List.fold_left (fun m (k, v) -> M.add k v m) m l)) what;
    note "union (sum on a shared key)"
      (IntTable.elements (IntTable.union (fun _ a b -> a + b) t t2) = pairs (M.union (fun _ a b -> Some (a + b)) m m2)) what;
    note "forall2 on itself" (IntTable.forall2 ( = ) t t) what;
    let acc, t' = IntTable.fold_map (fun acc k v -> acc + k, v * 2) 0 t in
    note "fold_map" (acc = M.fold (fun k _ a -> a + k) m 0 && IntTable.elements t' = pairs (M.map (fun v -> v * 2) m)) what;
    note "filter_add" (IntTable.elements (IntTable.filter_add t k (function None -> 0 | Some v -> v + 1))
                       = pairs (M.update k (function None -> Some 0 | Some v -> Some (v + 1)) m)) what;
    note "filter_remove (drop odd values)"
      (IntTable.elements (try IntTable.filter_remove t k (fun v -> if v mod 2 = 1 then None else Some v) with Not_found -> t)
       = pairs (M.update k (function Some v when v mod 2 = 1 -> None | x -> x) m)) what;
    note "replace (raises iff absent)"
      ((try Some (IntTable.elements (IntTable.replace t k (fun v -> v + 7))) with Not_found -> None)
       = (if M.mem k m then Some (pairs (M.update k (Option.map (fun v -> v + 7)) m)) else None)) what
  done;
  (* LmMakeList: a key to its values, newest first. *)
  for _ = 1 to 300 do
    let ops = List.init (pick 30) (fun _ -> pick 5, pick 10, pick 100) in
    let t, m =
      List.fold_left (fun (t, m) (o, k, v) ->
          let cur = try M.find k m with Not_found -> [] in
          match o with
          | 0 | 1 -> IntMTable.add t k v, M.add k (v :: cur) m
          | 2 -> IntMTable.remove t k, M.remove k m
          | 3 -> (try IntMTable.filter_remove t k (fun x -> if x mod 2 = 0 then None else Some (x + 1)) with Not_found -> t),
                 (match cur with
                  | [] -> m
                  | h :: rest -> if h mod 2 = 0 then (if rest = [] then M.remove k m else M.remove k m) else M.add k ((h + 1) :: rest) m)
          | _ -> IntMTable.filter_add t k (function None -> v | Some h -> h + v),
                 M.add k (match cur with [] -> [ v ] | h :: rest -> (h + v) :: rest) m)
        (IntMTable.empty, M.empty) ops in
    let what () = String.concat " " (List.map (fun (o, k, v) -> Printf.sprintf "%d/%d/%d" o k v) ops) in
    let lists t = List.sort compare (IntMTable.fold_all (fun acc k l -> (k, l) :: acc) [] t) in
    note "MTable: every key's values, newest first" (lists t = M.bindings m) what;
    let k = pick 10 in
    note "MTable.find = the newest" ((try Some (IntMTable.find t k) with Not_found -> None) = (match M.find_opt k m with Some (h :: _) -> Some h | _ -> None)) what;
    note "MTable.find_all" (IntMTable.find_all t k = (try M.find k m with Not_found -> [])) what;
    note "MTable.find_all_partial raises iff absent" ((try Some (IntMTable.find_all_partial t k) with Not_found -> None) = M.find_opt k m) what;
    note "MTable.replace (the newest; raises iff absent)"
      ((try Some (lists (IntMTable.replace t k (fun h -> h * 3))) with Not_found -> None)
       = (match M.find_opt k m with Some (h :: rest) -> Some (M.bindings (M.add k ((h * 3) :: rest) m)) | _ -> None)) what;
    let l = List.init (pick 6) (fun _ -> pick 10, pick 100) in
    note "MTable.add_list" (lists (IntMTable.add_list t l) = M.bindings (List.fold_left (fun m (k, v) -> M.add k (v :: (try M.find k m with Not_found -> [])) m) m l)) what;
    note "MTable.data_all" (List.sort compare (IntMTable.data_all t) = List.sort compare (List.map snd (M.bindings m))) what;
    let _, t' = IntMTable.fold_map (fun acc _ v -> acc, v) () t in
    note "MTable.fold_map (identity) keeps each key's values in order" (lists t' = lists t) what
  done;
  List.iter (fun name ->
      let t = Hashtbl.find tried name and o = try Hashtbl.find ok name with Not_found -> 0 in
      Printf.printf "%-62s %3d of %3d\n" name o t;
      match Hashtbl.find_opt first name with None -> () | Some w -> Printf.printf "  first difference: %s\n" w)
    (List.rev !order)
