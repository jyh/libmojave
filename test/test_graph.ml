(* Lm_graph (undirected) against a model: live nodes and a set of
   unordered edges. Random add_node / add_edge / delete_edge / delete_node
   steps from a fixed LCG (no self-loops); after each step: list, degree,
   neighbors, query both ways, and the component found by dfs_list. *)
open Pin
module G = Lm_graph

let seed = ref 17
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n
let sort l = List.sort_uniq compare l

let () =
  let bad = ref 0 and steps = ref 0 and first = ref "" in
  for _ = 1 to 400 do
    let g = G.create () in
    let nodes = Hashtbl.create 8 and alive = ref [] and edges = ref [] and next = ref 0 in
    let edge a b = (min a b, max a b) in
    for _ = 1 to 25 do
      incr steps;
      let pick () = List.nth !alive (rnd (List.length !alive)) in
      let what = if List.length !alive < 2 then 0 else rnd 6 in
      let desc =
        match what with
        | 0 | 1 -> let id = !next in incr next; Hashtbl.replace nodes id (G.add_node g id); alive := id :: !alive; Printf.sprintf "n%d" id
        | 2 | 3 ->
            let a = pick () and b = pick () in
            if a <> b then begin G.add_edge g (Hashtbl.find nodes a) (Hashtbl.find nodes b); edges := sort (edge a b :: !edges) end;
            Printf.sprintf "e%d-%d" a b
        | 4 ->
            let a = pick () and b = pick () in
            G.delete_edge (Hashtbl.find nodes a) (Hashtbl.find nodes b); edges := List.filter (( <> ) (edge a b)) !edges;
            Printf.sprintf "x%d-%d" a b
        | _ ->
            let a = pick () in
            G.delete_node (Hashtbl.find nodes a); alive := List.filter (( <> ) a) !alive;
            edges := List.filter (fun (x, y) -> x <> a && y <> a) !edges; Printf.sprintf "d%d" a
      in
      let nb a = sort (List.filter_map (fun (x, y) -> if x = a then Some y else if y = a then Some x else None) !edges) in
      let rec reach seen = function [] -> seen | a :: r -> if List.mem a seen then reach seen r else reach (a :: seen) (nb a @ r) in
      let ok =
        sort (List.map G.get (G.list g)) = sort !alive
        && List.for_all (fun a ->
            let n = Hashtbl.find nodes a in
            G.degree n = List.length (nb a)
            && sort (List.map G.get (G.neighbors n)) = nb a
            && List.for_all (fun b -> G.query n (Hashtbl.find nodes b) = List.mem b (nb a)) !alive
            && (G.dfs_init g; sort (List.map G.get (G.dfs_list n)) = sort (reach [] [ a ]))) !alive in
      if not ok then begin
        incr bad;
        if !first = "" then first := "after " ^ desc
      end
    done
  done;
  line "steps that disagree with the model" "400 graphs x 25 steps" (Printf.sprintf "%d of %d%s" !bad !steps (if !bad = 0 then "" else " (first: " ^ !first ^ ")"));
  (* a self-loop *)
  let g = G.create () in
  let n = G.add_node g 0 in
  G.add_edge g n n;
  line "self-loop: degree, neighbors, query" "" (Printf.sprintf "%d %d %b" (G.degree n) (List.length (G.neighbors n)) (G.query n n));
  G.delete_edge n n;
  line "  after delete_edge: degree, neighbors" "" (Printf.sprintf "%d %d" (G.degree n) (List.length (G.neighbors n)));
  line "choose on an empty graph" "" (guard (fun () -> string_of_int (G.get (G.choose (G.create ())))))
