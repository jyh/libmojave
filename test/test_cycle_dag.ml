(* Lm_cycle_dag against a model, from a fixed LCG: classes of equated
   names, each class a leaf or a node with an edge label and children.
   Random insert and equate steps over 8 names (an equate of two
   different names; a name equated with itself is a fixed case below); the model raises Cycle
   when an insert would make a class reach itself, an insert on a class
   that already has an edge, or an equate of two names already known.
   After each step: find on every name, and sort (every known class once,
   and each class before its children). sort's order among unrelated
   classes follows Hashtbl, so only that property is checked. *)
open Pin
module C = Lm_cycle_dag

let seed = ref 101
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n

let () =
  let steps = ref 0 in
  let bad = Hashtbl.create 8 and first = Hashtbl.create 8 and order = ref [] in
  let check op ok d =
    if not (List.mem op !order) then order := op :: !order;
    if not ok then begin
      Hashtbl.replace bad op (1 + try Hashtbl.find bad op with Not_found -> 0);
      if not (Hashtbl.mem first op) then Hashtbl.replace first op d
    end in
  for _ = 1 to 400 do
    let dag = C.create () in
    (* model: class id per known name; per class: Some (edge, child names) or None *)
    let cls = Hashtbl.create 8 and data = Hashtbl.create 8 and next = ref 0 and h = Buffer.create 32 in
    let class_of x = Hashtbl.find_opt cls x in
    let new_class names d = let c = !next in incr next; List.iter (fun x -> Hashtbl.replace cls x c) names; Hashtbl.replace data c d; c in
    let children c = match Hashtbl.find data c with None -> [] | Some (_, ns) -> List.filter_map class_of ns in
    let rec reaches seen c target = c = target || (not (List.mem c seen) && List.exists (fun k -> reaches (c :: seen) k target) (children c)) in
    for _ = 1 to 12 do
      incr steps;
      let a = rnd 8 in
      let desc =
        if rnd 3 = 0 then begin
          let b = (a + 1 + rnd 7) mod 8 in
          Buffer.add_string h (Printf.sprintf "eq %d %d; " a b);
          let want_cycle = class_of a <> None && class_of b <> None in
          (match C.equate dag a b with
           | () ->
               check "equate raises Cycle exactly when both names are known" (not want_cycle) (Buffer.contents h);
               (match class_of a, class_of b with
                | Some c, None -> Hashtbl.replace cls b c
                | None, Some c -> Hashtbl.replace cls a c
                | None, None -> ignore (new_class [ a; b ] None)
                | Some _, Some _ -> ())
           | exception C.Cycle -> check "equate raises Cycle exactly when both names are known" want_cycle (Buffer.contents h));
          ""
        end else begin
          let kids = List.init (rnd 3) (fun _ -> rnd 8) and e = rnd 100 in
          Buffer.add_string h (Printf.sprintf "ins %d %d [%s]; " a e (String.concat "," (List.map string_of_int kids)));
          let want_cycle =
            List.mem a kids
            || (match class_of a with
                | Some c -> Hashtbl.find data c <> None || List.exists (fun k -> match class_of k with Some ck -> reaches [] ck c | None -> false) kids
                | None -> false) in
          (match C.insert dag a e kids with
           | () ->
               check "insert raises Cycle exactly when the model does" (not want_cycle) (Buffer.contents h);
               List.iter (fun k -> if class_of k = None then ignore (new_class [ k ] None)) kids;
               (match class_of a with
                | Some c -> Hashtbl.replace data c (Some (e, kids))
                | None -> ignore (new_class [ a ] (Some (e, kids))))
           | exception C.Cycle -> check "insert raises Cycle exactly when the model does" want_cycle (Buffer.contents h));
          ""
        end in
      Buffer.add_string h desc;
      let d = Buffer.contents h in
      check "find" (List.for_all (fun x ->
          let want = match class_of x with Some c -> (match Hashtbl.find data c with Some (e, _) -> Some e | None -> None) | None -> None in
          (try Some (C.find dag x) with Not_found -> None) = want) (List.init 8 (fun x -> x))) d;
      let sorted = C.sort dag in
      let pos x = let rec go i = function [] -> -1 | (ns, _) :: r -> if List.mem x ns then i else go (i + 1) r in go 0 sorted in
      let classes = List.sort_uniq compare (Hashtbl.fold (fun _ c l -> c :: l) cls []) in
      check "sort: every class once, with all its names" (List.length sorted = List.length classes
        && List.for_all (fun c -> let names = Hashtbl.fold (fun x c' l -> if c' = c then x :: l else l) cls [] in
                           List.exists (fun (ns, _) -> List.sort compare ns = List.sort compare names) sorted) classes) d;
      check "sort: each class before its children" (Hashtbl.fold (fun x c ok -> ok && List.for_all (fun k ->
          match class_of k with Some ck when ck <> c -> pos x < pos k | _ -> true) (match Hashtbl.find data c with Some (_, ns) -> ns | None -> [])) cls true) d
    done
  done;
  List.iter (fun op ->
      let k = try Hashtbl.find bad op with Not_found -> 0 in
      line ("steps where " ^ op ^ " disagrees with the model") (string_of_int !steps ^ " steps")
        (Printf.sprintf "%d%s" k (if k = 0 then "" else " (first: " ^ Hashtbl.find first op ^ ")"))) (List.rev !order);
  (* fixed cases *)
  let dag = C.create () in
  C.insert dag 1 10 [ 2 ];
  C.equate dag 1 3;
  line "insert 1 -> [2], equate 1 3: find 3" "" (guard (fun () -> string_of_int (C.find dag 3)));
  line "  then insert 2 -> [3]" "" (guard (fun () -> C.insert dag 2 20 [ 3 ]; "returned"));
  let dag = C.create () in
  line "insert 0 -> [6] (6 a leaf), then insert 6 -> [3; 0] (a cycle)" "" (guard (fun () -> C.insert dag 0 2 [ 6 ]; C.insert dag 6 3 [ 3; 0 ]; "returned"));
  line "  then the names sort lists" "" (list (fun (ns, _) -> list string_of_int ns) (List.sort compare (C.sort dag)));
  let dag = C.create () in
  C.equate dag 0 0;
  line "equate 0 0 on a new name: sort" "" (list (fun (ns, _) -> list string_of_int ns) (C.sort dag));
  line "make, then find" "" (guard (fun () -> let d = C.make [ ([ 1; 2 ], Some (5, [ 3 ])); ([ 3 ], None) ] in Printf.sprintf "%d %d %s" (C.find d 1) (C.find d 2) (guard (fun () -> string_of_int (C.find d 3)))))
