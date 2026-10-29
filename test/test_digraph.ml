(* Lm_digraph against a model: live nodes and a multiset of directed edges.
   Random histories of add_node / add_edge / add_multi_edge / delete_edge /
   delete_node / coalesce_nodes from a fixed LCG (self-loops allowed).
   After each step, per operation: degrees, succ and pred (as multisets),
   query, list, is_path, and the depth-first orders. *)
open Pin
module D = Lm_digraph

let seed = ref 23
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n
let sort l = List.sort compare l
let uniq l = List.sort_uniq compare l

let rec remove_one x = function
  | [] -> []
  | y :: r -> if x = y then r else y :: remove_one x r

let ops = [ "degrees"; "succ/pred"; "query"; "list"; "is_path"; "rev_postorder"; "postorder"; "full_rev_postorder"; "untouched" ]
let bad = Hashtbl.create 8
let firsts = Hashtbl.create 8
let check op ok desc =
  if not ok then begin
    Hashtbl.replace bad op (1 + try Hashtbl.find bad op with Not_found -> 0);
    if not (Hashtbl.mem firsts op) then Hashtbl.replace firsts op desc
  end

let () =
  let steps = ref 0 in
  for _ = 1 to 400 do
    let g = D.create () in
    let nodes = Hashtbl.create 8 and alive = ref [] and edges = ref [] and next = ref 0 and hist = Buffer.create 64 in
    for _ = 1 to 20 do
      incr steps;
      let pick () = List.nth !alive (rnd (List.length !alive)) in
      let node a = Hashtbl.find nodes a in
      let what = if List.length !alive < 2 then 0 else rnd 8 in
      let desc =
        match what with
        | 0 | 1 ->
            let id = !next in
            incr next; Hashtbl.replace nodes id (D.add_node g id); alive := id :: !alive; Printf.sprintf "n%d" id
        | 2 | 3 ->
            let a = pick () and b = pick () in
            D.add_edge (node a) (node b);
            if not (List.mem (a, b) !edges) then edges := (a, b) :: !edges;
            Printf.sprintf "e%d>%d" a b
        | 4 ->
            let a = pick () and b = pick () in
            D.add_multi_edge (node a) (node b); edges := (a, b) :: !edges; Printf.sprintf "m%d>%d" a b
        | 5 ->
            let a = pick () and b = pick () in
            D.delete_edge (node a) (node b); edges := remove_one (a, b) !edges; Printf.sprintf "x%d>%d" a b
        | 6 ->
            let a = pick () in
            D.delete_node (node a); alive := List.filter (( <> ) a) !alive;
            edges := List.filter (fun (x, y) -> x <> a && y <> a) !edges; Printf.sprintf "d%d" a
        | _ ->
            (* m merged into n: n keeps its edges, gains m's (no duplicates),
               and m is gone; an edge between them becomes a self-loop on n *)
            let a = pick () and b = pick () in
            if a = b then Printf.sprintf "c%d=%d(skip)" a b
            else begin
              ignore (D.coalesce_nodes (node a) (node b) a);
              let mo = List.filter_map (fun (x, y) -> if x = b then Some y else None) !edges
              and mi = List.filter_map (fun (x, y) -> if y = b then Some x else None) !edges in
              edges := List.filter (fun (x, y) -> x <> b && y <> b) !edges;
              alive := List.filter (( <> ) b) !alive;
              let add (x, y) = if not (List.mem (x, y) !edges) then edges := (x, y) :: !edges in
              List.iter (fun y -> add (a, if y = b then a else y)) mo;
              List.iter (fun x -> add ((if x = b then a else x), a)) mi;
              Printf.sprintf "c%d=%d" a b
            end
      in
      Buffer.add_string hist (desc ^ " ");
      let h = Buffer.contents hist in
      let succ a = sort (List.filter_map (fun (x, y) -> if x = a then Some y else None) !edges) in
      let pred a = sort (List.filter_map (fun (x, y) -> if y = a then Some x else None) !edges) in
      let rec reach seen = function [] -> seen | a :: r -> if List.mem a seen then reach seen r else reach (a :: seen) (succ a @ r) in
      let ids l = List.map D.get l in
      List.iter (fun a ->
          let n = node a in
          check "degrees" (D.out_degree n = List.length (succ a) && D.in_degree n = List.length (pred a)) h;
          check "succ/pred" (sort (ids (D.succ n)) = succ a && sort (ids (D.pred n)) = pred a) h;
          check "query" (List.for_all (fun b -> D.query n (node b) = List.mem b (succ a)) !alive) h;
          check "is_path" (List.for_all (fun b -> D.is_path g n (node b) = (a = b || List.mem b (reach [] (succ a)))) !alive) h;
          let rp = ids (D.rev_postorder_list g n) in
          let r = reach [] [ a ] in
          (* every reachable node once, a first, and an edge x>y that is not
             a back edge (y cannot reach x) puts x before y *)
          let rec index x i = function [] -> -1 | y :: l -> if x = y then i else index x (i + 1) l in
          check "rev_postorder"
            (sort rp = uniq r && List.hd rp = a
             && List.for_all (fun (x, y) -> not (List.mem x r) || x = y || List.mem x (reach [] [ y ]) || index x 0 rp < index y 0 rp) !edges) h;
          check "untouched" (uniq (ids (D.untouched g)) = uniq (List.filter (fun b -> not (List.mem b r)) !alive)) h;
          check "postorder" (ids (D.postorder_list g n) = List.rev rp) h) !alive;
      check "list" (sort (ids (D.list g)) = sort !alive) h;
      let full = ids (D.full_rev_postorder_list g) in
      check "full_rev_postorder" (sort full = sort !alive) h
    done
  done;
  List.iter (fun op ->
      let k = try Hashtbl.find bad op with Not_found -> 0 in
      line ("steps where " ^ op ^ " disagrees with the model") "400 graphs x 20 steps"
        (Printf.sprintf "%d of %d%s" k !steps (if k = 0 then "" else " (first: after " ^ Hashtbl.find firsts op ^ ")"))) ops;
  (* fixed cases *)
  let g = D.create () in
  let a = D.add_node g 0 and b = D.add_node g 1 in
  D.add_edge a b; D.add_edge a b;
  line "add_edge twice: out_degree" "" (string_of_int (D.out_degree a));
  D.add_multi_edge a b;
  line "then add_multi_edge: out_degree, in_degree" "" (Printf.sprintf "%d %d" (D.out_degree a) (D.in_degree b));
  D.delete_edge a b;
  line "then delete_edge once: out_degree, query" "" (Printf.sprintf "%d %b" (D.out_degree a) (D.query a b));
  D.delete_edge b a;
  line "delete_edge of an absent edge: out_degree b" "" (string_of_int (D.out_degree b));
  line "is_path n n with no edges" "" (string_of_bool (let g = D.create () in let n = D.add_node g 0 in D.is_path g n n));
  line "choose on an empty graph" "" (guard (fun () -> string_of_int (D.get (D.choose (D.create ())))));
  line "find with no match" "" (guard (fun () -> string_of_int (D.get (D.find (fun _ -> false) g))));
  line "fold (sum of infos)" "" (string_of_int (D.fold (fun s n -> s + D.get n) 0 g));
  line "filter (info > 0)" "" (list string_of_int (List.map D.get (D.filter (fun n -> D.get n > 0) g)));
  let g = D.create () in
  let a = D.add_node g 0 and b = D.add_node g 1 in
  D.add_edge a b;
  line "coalesce a b where a>b: self-loop on a" "" (let n = D.coalesce_nodes a b 9 in Printf.sprintf "info %d query %b out %d" (D.get n) (D.query n n) (D.out_degree n));
  let g = D.create () in
  let a = D.add_node g 0 and b = D.add_node g 1 in
  D.add_edge a b;
  D.delete_node b;
  line "delete_node twice" "" (guard (fun () -> D.delete_node b; "returned"));
  line "  then list" "" (list string_of_int (List.map D.get (D.list g)))
;;
let () =
  let g = D.create () in
  let a = D.add_node g 0 and b = D.add_node g 1 in
  D.add_edge b b;
  let n = D.coalesce_nodes a b 0 in
  line "coalesce a b where b>b: a's succ, pred" "" (Printf.sprintf "%s %s" (list string_of_int (List.map D.get (D.succ n))) (list string_of_int (List.map D.get (D.pred n))));
  line "  pred holds the deleted node" "" (string_of_bool (List.memq b (D.pred n)))
