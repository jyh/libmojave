(* Lm_imp_dag: the imperative DAG behind metaprl's precedences
   (mllib/precedence.ml, auto_tactic.ml use insert, add_edge, equate and
   node_rel). Nodes are ints in insertion order. *)
open Pin
open Lm_dag_sig
open Lm_imp_dag

let rel = function NoRelation -> "none" | LessThan -> "<" | Equal -> "=" | GreaterThan -> ">"

(* a -> b -> c, a -> d, e alone *)
let build () =
  let g = ImpDag.create () in
  let n = List.map (ImpDag.insert g) [ "a"; "b"; "c"; "d"; "e" ] in
  (match n with
     [ a; b; c; d; _ ] -> ImpDag.add_edge g a b; ImpDag.add_edge g b c; ImpDag.add_edge g a d
   | _ -> assert false);
  g, Array.of_list n

let names g l = list (fun x -> ImpDag.node_value g x) l
let edges g n = Printf.sprintf "out=%s in=%s" (names g (ImpDag.node_out_edges g n)) (names g (ImpDag.node_in_edges g n))

let () =
  let g, n = build () in
  line "values" "" (names g (Array.to_list n));
  line "roots" "" (names g (ImpDag.roots g));
  line "sort" "" (names g (ImpDag.sort g));
  line "edges of a, b, c" "" (String.concat " | " (List.map (fun i -> edges g n.(i)) [ 0; 1; 2 ]));
  let show f = list (fun s -> s) (f g (fun v kids -> v ^ "(" ^ String.concat "," kids ^ ")")) in
  line "sweep_up" "" (show ImpDag.sweep_up);
  line "sweep_down" "" (show ImpDag.sweep_down);
  line "sweep_up_all" "" (show ImpDag.sweep_up_all);
  line "sweep_down_all" "" (show ImpDag.sweep_down_all);
  let r i j = rel (ImpDag.node_rel g n.(i) n.(j)) in
  line "node_rel a b, b a, a c, c a, a e, b d" "" (String.concat " " [ r 0 1; r 1 0; r 0 2; r 2 0; r 0 4; r 1 3 ]);
  (* node_rel's cache of derived relations must not change the edges or sweeps *)
  line "  edges of a, c after those queries" "" (edges g n.(0) ^ " | " ^ edges g n.(2));
  line "  node_rel c a again" "" (r 2 0);
  line "  sweep_up after those queries" "" (show ImpDag.sweep_up);
  let g2, m = build () in
  ImpDag.equate g2 m.(2) m.(4);
  line "equate c e: node_rel c e, e c, a e" "" (String.concat " " (List.map (fun (i, j) -> rel (ImpDag.node_rel g2 m.(i) m.(j))) [ 2, 4; 4, 2; 0, 4 ]));
  line "  roots, sort" "" (names g2 (ImpDag.roots g2) ^ " " ^ names g2 (ImpDag.sort g2));
  (* delete b (node 1) *)
  let g3, k = build () in
  ImpDag.delete g3 k.(1);
  line "delete b: roots" "" (guard (fun () -> names g3 (ImpDag.roots g3)));
  line "  sort" "" (guard (fun () -> names g3 (ImpDag.sort g3)));
  line "  edges of each node in sort order" "" (guard (fun () -> String.concat " | " (List.map (fun x -> ImpDag.node_value g3 x ^ ": " ^ edges g3 x) (ImpDag.sort g3))))

(* A relation found by a query through b must not outlive b: after
   delete b, a and c are unrelated. *)
let () =
  let g, n = build () in
  let before = rel (ImpDag.node_rel g n.(0) n.(2)) in
  ImpDag.delete g n.(1);
  (* delete renumbers: c now has b's old index, n.(1) *)
  line "node_rel a c, then delete b, then node_rel a c" "" (before ^ " " ^ ImpDag.node_value g n.(1) ^ " " ^ rel (ImpDag.node_rel g n.(0) n.(1)));
  line "  roots, sort" "" (names g (ImpDag.roots g) ^ " " ^ names g (ImpDag.sort g))
