(* Lm_graph's traversals against its own dfs_list (which test_graph checks
   against a model) and list: on 300 random undirected graphs (10 nodes,
   random edges, from a fixed LCG), from a random start node,
   - dfs_list is a preorder: each node after the first is adjacent to an
     earlier one (dfs_list is defined as dfs_list_rev reversed, so this
     checks both);
   - dfs_iter visits the nodes of dfs_list, in that order;
   - dfs_fold folds over them in that order;
   - iter and fold visit exactly the nodes of list, once each.
   dfs_init is called before each traversal, as the interface asks.
   Per check: how many graphs differ, and the first. *)
open Pin
module G = Lm_graph

let seed = ref 113
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 12) mod n

let () =
  let bad = Array.make 4 0 and first = Array.make 4 "" in
  let note k ok d = if not ok then begin bad.(k) <- bad.(k) + 1; if first.(k) = "" then first.(k) <- d () end in
  for _ = 1 to 300 do
    let g = G.create () in
    let nodes = Array.init 10 (fun i -> G.add_node g i) in
    for _ = 1 to rnd 20 do
      let a = rnd 10 and b = rnd 10 in
      if a <> b && not (G.query nodes.(a) nodes.(b)) then G.add_edge g nodes.(a) nodes.(b)
    done;
    let s = nodes.(rnd 10) in
    let ids l = List.map G.get l in
    let show l = String.concat "," (List.map string_of_int (ids l)) in
    G.dfs_init g; let l = G.dfs_list s in
    G.dfs_init g; let r = G.dfs_list_rev s in
    let rec pre earlier = function
      | [] -> true
      | n :: rest -> List.exists (fun e -> G.query e n) earlier && pre (n :: earlier) rest in
    let ok0 = (match l with [] -> false | h :: t -> G.get h = G.get s && pre [ h ] t) && ids r = List.rev (ids l) in
    note 0 ok0 (fun () -> Printf.sprintf "dfs_list %s, dfs_list_rev %s" (show l) (show r));
    G.dfs_init g; let seen = ref [] in G.dfs_iter (fun n -> seen := G.get n :: !seen) s;
    note 1 (List.rev !seen = ids l) (fun () -> Printf.sprintf "dfs_list %s, dfs_iter %s" (show l)
                                       (String.concat "," (List.map string_of_int (List.rev !seen))));
    G.dfs_init g; let f = G.dfs_fold (fun acc n -> G.get n :: acc) [] s in
    note 2 (List.rev f = ids l) (fun () -> Printf.sprintf "dfs_list %s, dfs_fold %s" (show l)
                                   (String.concat "," (List.map string_of_int (List.rev f))));
    let all = List.sort compare (ids (G.list g)) in
    let it = ref [] in G.iter (fun n -> it := G.get n :: !it) g;
    let fo = G.fold (fun acc n -> G.get n :: acc) [] g in
    note 3 (List.sort compare !it = all && List.sort compare fo = all && List.length all = 10)
      (fun () -> Printf.sprintf "list %d nodes, iter %d, fold %d" (List.length all) (List.length !it) (List.length fo))
  done;
  List.iteri (fun k name ->
      line (name ^ ": graphs that differ") "300 graphs"
        (Printf.sprintf "%d%s" bad.(k) (if bad.(k) = 0 then "" else " (first: " ^ first.(k) ^ ")")))
    [ "dfs_list from the start, each node adjacent to an earlier one"; "dfs_iter in dfs_list order"; "dfs_fold in dfs_list order";
      "iter and fold: each node of list once" ]
