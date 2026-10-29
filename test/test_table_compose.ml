(* Lm_splay_table.MakeTable (built by Lm_table_util.MakeTable) against a
   model: a sorted association list from key to data list, last added
   first. Random histories from a fixed LCG over a POOL of tables: each
   step takes tables from the pool (old versions included), applies add,
   remove, replace, union, map, fold_map or deletemax, and puts the result
   back. Lookups splay a table in place, so after each step both the result
   and a random older table are checked: list_of, length, is_empty, mem,
   find and find_all on every key, iter's entries, and deletemax. *)
open Pin

module Base = struct
  type elt = int
  type data = string
  let print out k l = Lm_printf.fprintf out "%d:%s;" k (String.concat "," l)
  let compare = compare
  let append = ( @ )
end

module T = Lm_splay_table.MakeTable (Base)

let seed = ref 41
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n
let keys = List.init 12 (fun k -> k)
let fresh = let c = ref 0 in fun k -> incr c; Printf.sprintf "%d.%d" k !c

(* the model *)
let m_find m k = try List.assoc k m with Not_found -> []
let m_set m k l =
  let m = List.remove_assoc k m in
  if l = [] then m else List.sort (fun (a, _) (b, _) -> compare a b) ((k, l) :: m)
let m_union a b = List.fold_left (fun m k -> m_set m k (m_find a k @ m_find b k)) [] keys

let ops = [ "list_of"; "length/is_empty"; "mem"; "find"; "find_all"; "iter"; "deletemax"; "old version changed" ]
let bad = Hashtbl.create 8
let firsts = Hashtbl.create 8
let steps = ref 0
let report op ok desc =
  if not ok then begin
    Hashtbl.replace bad op (1 + try Hashtbl.find bad op with Not_found -> 0);
    if not (Hashtbl.mem firsts op) then Hashtbl.replace firsts op desc
  end

(* returns whether every check passed; an older table is reported only if
   it passed when it was made, so a wrong result is not counted twice *)
let check ?(old = false) desc t m =
  let all = ref true in
  let r op ok = if not ok then all := false; if not old then report op ok desc in
  r "list_of" (T.list_of t = m);
  r "length/is_empty" (T.length t = List.length m && T.is_empty t = (m = []));
  List.iter (fun k ->
      let d = m_find m k in
      r "mem" (T.mem t k = (d <> []));
      r "find" ((try Some (T.find t k) with Not_found -> None) = (match d with [] -> None | x :: _ -> Some x));
      r "find_all" ((try T.find_all t k with Not_found -> []) = d)) keys;
  let seen = ref [] in
  T.iter (fun k d -> seen := (k, d) :: !seen) t;
  r "iter" (List.sort compare !seen = List.sort compare (List.concat_map (fun (k, l) -> List.map (fun d -> (k, d)) l) m));
  if m <> [] then begin
    let k, l, rest = T.deletemax t in
    let mk, ml = List.nth m (List.length m - 1) in
    r "deletemax" (k = mk && l = ml && T.list_of rest = List.remove_assoc mk m)
  end;
  !all

let () =
  for _ = 1 to 300 do
    let pool = ref [ (T.empty, [], "empty", true) ] in
    for _ = 1 to 20 do
      incr steps;
      let pick () = List.nth !pool (rnd (List.length !pool)) in
      let t, m, h, _ = pick () in
      let k = rnd 12 in
      let t', m', d =
        match rnd 9 with
        | 0 | 1 | 2 -> let d = fresh k in (T.add t k d, m_set m k (d :: m_find m k), Printf.sprintf "add %d" k)
        | 3 -> (T.remove t k, m_set m k (match m_find m k with [] -> [] | _ :: r -> r), Printf.sprintf "remove %d" k)
        | 4 -> let l = List.init (1 + rnd 2) (fun _ -> fresh k) in (T.replace t k l, m_set m k l, Printf.sprintf "replace %d" k)
        | 5 -> let t2, m2, h2, _ = pick () in (T.union t t2, m_union m m2, "union (" ^ h2 ^ ")")
        | 6 -> (T.map (fun k d -> d ^ "m" ^ string_of_int k) t, List.map (fun (k, l) -> (k, List.map (fun d -> d ^ "m" ^ string_of_int k) l)) m, "map")
        | 7 -> (T.fold_map (fun k d acc -> if k mod 2 = 0 then T.add acc k d else acc) T.empty t,
                List.filter_map (fun (k, l) -> if k mod 2 = 0 then Some (k, List.rev l) else None) m, "fold_map even")
        | _ -> if m = [] then (t, m, "deletemax(skip)") else
                 let _, _, rest = T.deletemax t in (rest, List.filteri (fun i _ -> i < List.length m - 1) m, "deletemax")
      in
      let h' = h ^ "; " ^ d in
      let ok = try check h' t' m' with e -> report "list_of" false (h' ^ " EXN " ^ Printexc.to_string e); false in
      let o, om, oh, ook = pick () in
      if ook then begin
        let still = try check ~old:true "" o om with _ -> false in
        report "old version changed" still (oh ^ " [after: " ^ h' ^ "]")
      end;
      pool := (t', m', h', ok) :: !pool
    done
  done;
  List.iter (fun op ->
      let k = try Hashtbl.find bad op with Not_found -> 0 in
      line ("steps where " ^ op ^ " disagrees with the model") "300 histories x 20 steps"
        (Printf.sprintf "%d of %d%s" k !steps (if k = 0 then "" else " (first: " ^ Hashtbl.find firsts op ^ ")"))) ops;
  (* fixed cases *)
  let t = T.replace (T.add T.empty 1 "a") 1 [] in
  line "replace k [] on a one-key table: length, list_of" "" (guard (fun () -> Printf.sprintf "%d %s" (T.length t) (list (fun (k, l) -> string_of_int k ^ "->" ^ list str l) (T.list_of t))));
  line "  then mem" "" (guard (fun () -> string_of_bool (T.mem t 1)));
  line "  then add 2" "" (guard (fun () -> string_of_int (T.length (T.add t 2 "b"))));
  let t = T.replace (T.add (T.add T.empty 1 "a") 2 "b") 1 [] in
  line "replace k [] on a two-key table, then mem k" "" (guard (fun () -> string_of_bool (T.mem t 1)));
  let one = T.add T.empty 1 "s2" and two = T.add (T.add T.empty 1 "s1") 0 "s1" in
  line "union: shared key, second table one node" "" (guard (fun () -> list str (T.find_all (T.union two one) 1)));
  line "union: shared key, first table one node" "" (guard (fun () -> list str (T.find_all (T.union (T.add T.empty 1 "s1") (T.add (T.add T.empty 1 "s2") 0 "s2")) 1)));
  line "deletemax empty" "" (guard (fun () -> let k, _, _ = T.deletemax T.empty in string_of_int k));
  line "find absent" "" (guard (fun () -> T.find (T.add T.empty 1 "a") 2));
  line "find_all absent" "" (guard (fun () -> list str (T.find_all (T.add T.empty 1 "a") 2)))
;;
(* A table ordered by descending key: union must use the table's own order,
   not polymorphic compare. *)
module RBase = struct
  include Base
  let compare a b = compare b a
end
module R = Lm_splay_table.MakeTable (RBase)

let () =
  let build l = List.fold_left (fun t k -> R.add t k (string_of_int k)) R.empty l in
  let bad = ref 0 and n = ref 0 and first = ref "" in
  for _ = 1 to 500 do
    let a = List.init (rnd 6) (fun _ -> rnd 10) and b = List.init (rnd 6) (fun _ -> rnd 10) in
    incr n;
    let u = R.union (build a) (build b) in
    let want = List.sort_uniq (fun x y -> compare y x) (a @ b) in
    let ok = (try List.map fst (R.list_of u) = want && List.for_all (fun k -> R.mem u k) want with _ -> false) in
    if not ok then begin incr bad; if !first = "" then first := list string_of_int a ^ " " ^ list string_of_int b end
  done;
  line "descending order: unions whose keys are wrong or unordered" "500 random pairs"
    (Printf.sprintf "%d of %d%s" !bad !n (if !bad = 0 then "" else " (first: " ^ !first ^ ")"))
