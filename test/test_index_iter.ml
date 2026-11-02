(* Lm_index's iterators against fold_all (which test_index_model checks
   against a model trie), on random tries from a fixed LCG: the family the
   2026-11-02 coverage run never reached.

   - iter_all visits the (path, datum) pairs fold_all visits, in the same
     order; map_all f and mapi_all f give a trie whose pairs are fold_all's
     with f applied; fold_map_all threads an accumulator through the same
     pairs and builds the mapped trie.
   - one level: iter and fold visit each top key once with its sub-index
     and datum (keys increasing); map and mapi map that pair; fold_map
     threads an accumulator and maps it. Checked against find on each key.
   One line per check: how many of 400 tries disagree, and the first. *)
open Pin
module I = Lm_index.LmMake (struct type t = string let compare = compare end)

let seed = ref 281
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n

let bad = Hashtbl.create 8 and first = Hashtbl.create 8 and order = ref [] and steps = Hashtbl.create 8
let check op ok d =
  if not (List.mem op !order) then order := op :: !order;
  Hashtbl.replace steps op (1 + try Hashtbl.find steps op with Not_found -> 0);
  if not ok then begin
    Hashtbl.replace bad op (1 + try Hashtbl.find bad op with Not_found -> 0);
    if not (Hashtbl.mem first op) then Hashtbl.replace first op (d ())
  end

let keys = [| "a"; "b"; "c"; "d" |]
let path () = List.init (1 + rnd 3) (fun _ -> keys.(rnd 4))
let pairs t = List.rev (I.fold_all (fun acc p d -> (p, d) :: acc) [] t)
let show l = list (fun (p, d) -> String.concat "." p ^ "=" ^ string_of_int d) l

let gen () =
  let t = ref I.empty in
  for _ = 1 to rnd 10 do
    let p = path () and d = rnd 100 in
    (* add_list makes the whole path (add_entry refuses a missing prefix) *)
    t := I.add_list !t p (List.map (fun _ -> d + rnd 5) p)
  done;
  !t

let total = ref 0 and big = ref 0
let () =
  for _ = 1 to 400 do
    let t = gen () in
    let ps = pairs t in
    total := !total + List.length ps; if List.length ps >= 3 then incr big;
    let h () = show ps in
    let seen = ref [] in
    I.iter_all (fun p d -> seen := (p, d) :: !seen) t;
    check "iter_all = fold_all's pairs, in order" (List.rev !seen = ps) (fun () -> h () ^ " vs " ^ show (List.rev !seen));
    check "map_all" (pairs (I.map_all (fun d -> d * 2) t) = List.map (fun (p, d) -> p, d * 2) ps) (fun () -> h ());
    check "mapi_all" (pairs (I.mapi_all (fun p d -> d + List.length p) t) = List.map (fun (p, d) -> p, d + List.length p) ps)
      (fun () -> h ());
    let n, t2 = I.fold_map_all (fun n p d -> n + 1, d + List.length p) 0 t in
    check "fold_map_all" (n = List.length ps && pairs t2 = List.map (fun (p, d) -> p, d + List.length p) ps)
      (fun () -> h () ^ Printf.sprintf " count %d" n);
    (* one level *)
    let top = List.sort_uniq compare (List.map (fun (p, _) -> List.hd p) ps) in
    let lv = ref [] in
    I.iter (fun k (sub, d) -> lv := (k, pairs sub, d) :: !lv) t;
    let want = List.map (fun k -> let sub, d = I.find t k in k, pairs sub, d) top in
    check "iter (one level, keys increasing)" (List.rev !lv = want) (fun () -> h ());
    check "fold (one level)" (List.rev (I.fold (fun acc k (sub, d) -> (k, pairs sub, d) :: acc) [] t) = want) (fun () -> h ());
    let m = I.map (fun (sub, d) -> I.map_all (fun x -> x + 1) sub, d * 3) t in
    check "map (one level)" (List.map (fun k -> let sub, d = I.find m k in k, pairs sub, d) top
                             = List.map (fun (k, sp, d) -> k, List.map (fun (p, x) -> p, x + 1) sp, d * 3) want)
      (fun () -> h ());
    let mi = I.mapi (fun k (sub, d) -> sub, d + String.length k + Char.code k.[0]) t in
    check "mapi (one level)" (List.map (fun k -> snd (I.find mi k)) top
                              = List.map (fun (k, _, d) -> d + String.length k + Char.code k.[0]) want)
      (fun () -> h ());
    let c, fm = I.fold_map (fun c k (sub, d) -> c ^ k, (sub, d + 7)) "" t in
    check "fold_map (one level)" (c = String.concat "" top && List.map (fun k -> snd (I.find fm k)) top = List.map (fun (_, _, d) -> d + 7) want)
      (fun () -> h () ^ " acc " ^ c)
  done;
  line "haystack" "400 tries" (Printf.sprintf "%d (path, datum) pairs; %d tries hold 3 or more" !total !big);
  List.iter (fun op ->
      let k = try Hashtbl.find bad op with Not_found -> 0 in
      line op (string_of_int (Hashtbl.find steps op) ^ " tries")
        (if k = 0 then "0 disagree" else Printf.sprintf "%d disagree (first: %s)" k (Hashtbl.find first op)))
    (List.rev !order)
