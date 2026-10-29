(* Lm_field_table against models. Random histories from a fixed LCG.
   FieldTable: a list of (name, value, index), a re-added name taking a new
   index; checked: cardinal, mem, find, find_index, to_list in index order,
   iter / fold / fold_index entries, and map / mapi / fold_map.
   FieldMTable: a list of (external, internal, value, index), internal
   names unique (each add makes a fresh one); checked: find_ext (newest
   first, Not_found when absent), find_int,
   to_list in index order, fold, after add, replace_ext and map. *)
open Pin
module F = Lm_field_table.FieldTable
module M = Lm_field_table.FieldMTable

let seed = ref 61
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n
let names = Array.init 8 (fun i -> Lm_symbol.add (Printf.sprintf "f%d" i))
let name i = names.(i)
let sym = Lm_symbol.string_of_symbol
let sort l = List.sort compare l

let bad = Hashtbl.create 8
let firsts = Hashtbl.create 8
let order = ref []
let check op ok desc =
  if not (List.mem op !order) then order := op :: !order;
  if not ok then begin
    Hashtbl.replace bad op (1 + try Hashtbl.find bad op with Not_found -> 0);
    if not (Hashtbl.mem firsts op) then Hashtbl.replace firsts op desc
  end

let report what steps =
  List.iter (fun op ->
      let k = try Hashtbl.find bad op with Not_found -> 0 in
      line (what ^ ": steps where " ^ op ^ " disagrees with the model") (string_of_int steps ^ " steps")
        (Printf.sprintf "%d of %d%s" k steps (if k = 0 then "" else " (first: " ^ Hashtbl.find firsts op ^ ")")))
    (List.rev !order);
  Hashtbl.reset bad; Hashtbl.reset firsts; order := []

(* FieldTable *)
let () =
  let steps = ref 0 in
  for _ = 1 to 300 do
    let t = ref F.empty and m = ref [] and next = ref 0 and h = Buffer.create 32 in
    for _ = 1 to 15 do
      incr steps;
      let k = rnd 8 and x = rnd 100 in
      (match rnd 6 with
       | 0 | 1 | 2 ->
           t := F.add !t (name k) x;
           m := List.filter (fun (k', _, _) -> k' <> k) !m @ [ (k, x, !next) ]; incr next;
           Buffer.add_string h (Printf.sprintf "add f%d %d; " k x)
       | 3 -> t := F.map (fun x -> x + 1) !t; m := List.map (fun (k, x, i) -> (k, x + 1, i)) !m; Buffer.add_string h "map; "
       | 4 -> t := F.mapi (fun v x -> x + String.length (sym v)) !t; m := List.map (fun (k, x, i) -> (k, x + String.length (sym (name k)), i)) !m; Buffer.add_string h "mapi; "
       | _ ->
           let s, t' = F.fold_map (fun s _ x -> (s + x, x * 2)) 0 !t in
           check "fold_map" (s = List.fold_left (fun s (_, x, _) -> s + x) 0 !m) (Buffer.contents h);
           t := t'; m := List.map (fun (k, x, i) -> (k, x * 2, i)) !m; Buffer.add_string h "fold_map; ");
      let d = Buffer.contents h in
      check "cardinal" (F.cardinal !t = List.length !m) d;
      let ks = List.init 8 (fun k -> k) in
      let e k = List.find_opt (fun (k', _, _) -> k' = k) !m in
      check "mem" (List.for_all (fun k -> F.mem !t (name k) = (e k <> None)) ks) d;
      check "find" (List.for_all (fun k ->
          let want = Option.map (fun (_, x, i) -> (x, i)) (e k) in
          (try Some (F.find_with_index !t (name k)) with Not_found -> None) = want
          && (try Some (F.find !t (name k), F.find_index !t (name k)) with Not_found -> None) = want) ks) d;
      check "to_list" (List.map (fun (v, x) -> (sym v, x)) (F.to_list !t) = List.map (fun (k, x, _) -> (sym (name k), x)) !m) d;
      let seen = ref [] in
      F.iter (fun v x -> seen := (sym v, x) :: !seen) !t;
      let want = sort (List.map (fun (k, x, _) -> (sym (name k), x)) !m) in
      check "iter/fold" (sort !seen = want && sort (F.fold (fun l v x -> (sym v, x) :: l) [] !t) = want) d;
      check "fold_index" (sort (F.fold_index (fun l v x i -> (sym v, x, i) :: l) [] !t) = sort (List.map (fun (k, x, i) -> (sym (name k), x, i)) !m)) d
    done
  done;
  report "FieldTable" !steps;
  (* add_index: a table rebuilt from another through fold_index *)
  let t = List.fold_left (fun t (k, x) -> F.add t (name k) x) F.empty [ (3, 30); (1, 10); (3, 31); (5, 50) ] in
  let t2 = F.fold_index (fun t v x i -> F.add_index t v x i) F.empty t in
  line "FieldTable: rebuilt with add_index, to_list" "" (list (fun (v, x) -> sym v ^ "=" ^ string_of_int x) (F.to_list t2));
  let t3 = F.add t2 (name 6) 60 in
  line "  then add, find_index of the new field" "" (string_of_int (F.find_index t3 (name 6)));
  line "find absent" "" (guard (fun () -> string_of_int (F.find F.empty (name 0))))

(* FieldMTable *)
let () =
  let steps = ref 0 and fresh = ref 0 in
  for _ = 1 to 300 do
    let t = ref (M.create ()) and m = ref [] and next = ref 0 and h = Buffer.create 32 in
    for _ = 1 to 15 do
      incr steps;
      let k = rnd 4 and x = rnd 100 in
      (match rnd 5 with
       | 0 | 1 | 2 ->
           incr fresh;
           let vi = Lm_symbol.add (Printf.sprintf "i%d" !fresh) in
           t := M.add !t (name k) vi x;
           m := !m @ [ (k, vi, x, !next) ]; incr next;
           Buffer.add_string h (Printf.sprintf "add f%d %d; " k x)
       | 3 ->
           (* replace the first (newest) entry whose value is even *)
           let entries = List.rev (List.filter (fun (k', _, _, _) -> k' = k) !m) in
           let target = List.find_opt (fun (_, _, x, _) -> x mod 2 = 0) entries in
           (match M.replace_ext !t (name k) (fun vi x -> if x mod 2 = 0 then (vi, x + 1) else raise Not_found) with
            | t' ->
                check "replace_ext" (target <> None) (Buffer.contents h);
                t := t';
                (match target with
                 | Some (_, vi, _, _) -> m := List.map (fun (k', vi', x, i) -> if vi' == vi then (k', vi', x + 1, i) else (k', vi', x, i)) !m
                 | None -> ())
            | exception Not_found -> check "replace_ext" (target = None) (Buffer.contents h));
           Buffer.add_string h (Printf.sprintf "replace_ext f%d; " k)
       | _ ->
           t := M.map (fun _ vi x -> (vi, x * 3)) !t; m := List.map (fun (k, vi, x, i) -> (k, vi, x * 3, i)) !m;
           Buffer.add_string h "map; ");
      let d = Buffer.contents h in
      (* newest first; an absent external name raises Not_found, as
         Lm_map's find_all_partial documents *)
      check "find_ext (newest first)" (List.for_all (fun k ->
          let want = match List.rev_map (fun (_, vi, x, _) -> (sym vi, x)) (List.filter (fun (k', _, _, _) -> k' = k) !m) with [] -> [ ("Not_found", 0) ] | l -> l in
          (try List.map (fun (vi, x) -> (sym vi, x)) (M.find_ext !t (name k)) with Not_found -> [ ("Not_found", 0) ]) = want) [ 0; 1; 2; 3 ]) d;
      check "find_int" (List.for_all (fun (k, vi, x, _) -> match M.find_int !t vi with (v, y) -> v == name k && y = x | exception Not_found -> false) !m) d;
      check "to_list" ((try List.map (fun (v, vi, x) -> (sym v, sym vi, x)) (M.to_list !t) with Failure _ -> [ ("FAILURE", "", 0) ])
                       = List.map (fun (k, vi, x, _) -> (sym (name k), sym vi, x)) !m) d;
      check "fold" (sort (M.fold (fun l v vi x -> (sym v, sym vi, x) :: l) [] !t) = sort (List.map (fun (k, vi, x, _) -> (sym (name k), sym vi, x)) !m)) d
    done
  done;
  report "FieldMTable" !steps;
  (* fixed cases *)
  let a = Lm_symbol.add "a" in
  let t = M.add (M.add (M.create ()) (name 0) a 1) (name 1) a 2 in
  line "FieldMTable: two entries with one internal name, to_list" "" (guard (fun () -> string_of_int (List.length (M.to_list t))));
  line "  find_int" "" (guard (fun () -> let v, x = M.find_int t a in sym v ^ " " ^ string_of_int x));
  line "replace_ext on an absent external name" "" (guard (fun () -> ignore (M.replace_ext (M.create ()) (name 0) (fun v x -> (v, x))); "returned"));
  line "find_ext absent" "" (guard (fun () -> list string_of_int (List.map snd (M.find_ext (M.create ()) (name 0)))))
