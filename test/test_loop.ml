(* Lm_loop's dominators against a brute-force model, from a fixed LCG:
   random digraphs on up to 9 nodes, node 0 the root. d strictly dominates
   a reachable n iff removing d makes n unreachable from the root; the
   immediate dominator is the strict dominator that every other strict
   dominator dominates. Then fixed cases, and the loop nest of one fixed
   graph, printed. *)
open Pin
module L = Lm_loop
module S = Lm_symbol

let seed = ref 107
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n
let syms = Array.init 9 (fun i -> S.add (Printf.sprintf "n%d" i))
let sym i = syms.(i)
let make succ nodes = L.create "test" sym (fun i -> List.map sym succ.(i)) 0 nodes
let idoms succ nodes =
  let dom = L.dominators (make succ nodes) sym in
  String.concat " " (List.map (fun i -> S.string_of_symbol (S.SymbolTable.find dom (sym i))) nodes)

(* [restricted]: no edge into the root, and only reachable nodes listed *)
let model name restricted =
  let graphs = 1500 and bad = ref 0 and checked = ref 0 and first = ref "" in
  seed := 107;
  for _ = 1 to graphs do
    let k = 2 + rnd 8 in
    let succ = Array.init k (fun _ -> List.sort_uniq compare (List.init (rnd 3) (fun _ -> if restricted then 1 + rnd (k - 1) else rnd k))) in
    let reach ?(without = -1) () =
      let seen = Array.make k false in
      let rec go n = if n <> without && not seen.(n) then begin seen.(n) <- true; List.iter go succ.(n) end in
      go 0; seen in
    let r = reach () in
    let dominates d n = d = n || not (reach ~without:d ()).(n) in
    let nodes = List.filter (fun i -> r.(i) || not restricted) (List.init (k - 1) (fun i -> i + 1)) in
    let dom = L.dominators (make succ nodes) sym in
    List.iter (fun n ->
        if r.(n) then begin
          incr checked;
          let strict = List.filter (fun d -> d <> n && dominates d n) (List.init k (fun d -> d)) in
          let idom = List.find (fun d -> List.for_all (fun d' -> dominates d' d) strict) strict in
          let got = S.SymbolTable.find dom (sym n) in
          if not (S.eq got (sym idom)) then begin
            incr bad;
            if !first = "" then first := Printf.sprintf "%s: n%d got %s, model n%d"
                (String.concat " " (Array.to_list (Array.mapi (fun i l -> Printf.sprintf "%d>%s" i (String.concat "," (List.map string_of_int l))) succ))) n (S.string_of_symbol got) idom
          end
        end) nodes
  done;
  line ("reachable nodes whose idom differs from the model, " ^ name) (string_of_int graphs ^ " graphs")
    (Printf.sprintf "%d of %d%s" !bad !checked (if !bad = 0 then "" else " (first: " ^ !first ^ ")"))

let () =
  model "no edge into the root, reachable nodes only" true;
  model "any edges, every node listed" false;
  (* an edge into the root *)
  line "0 > 1 > 0: idom of n1" "" (guard (fun () -> idoms [| [ 1 ]; [ 0 ] |] [ 1 ]));
  (* an unreachable node listed: 2 > 1 *)
  line "0 > 1, 1 > 3, 2 > 3 (2 unreachable): idom of n1 n3 n2" "" (guard (fun () -> idoms [| [ 1 ]; [ 3 ]; [ 3 ]; [] |] [ 1; 3; 2 ]));
  line "0 > 1, 1 > 2 > 3, 0 > 3, 4 > 2 (4 unreachable): idom of n1 n2 n3 n4" "" (guard (fun () -> idoms [| [ 1; 3 ]; [ 2 ]; [ 3 ]; []; [ 2 ] |] [ 1; 2; 3; 4 ]));
  (* a fixed graph with a nested loop: 0 -> 1 -> 2 -> 3 -> 2, 3 -> 1, 3 -> 4 *)
  let succ = [| [ 1 ]; [ 2 ]; [ 3 ]; [ 2; 1; 4 ]; [] |] in
  let t = make succ [ 1; 2; 3; 4 ] in
  let b = Buffer.create 64 in
  let fmt = Lm_printf.formatter_of_buffer b in
  L.pp_print_trace fmt sym (L.loop_nest t sym);
  Lm_printf.pp_print_flush fmt ();
  line "fixed graph: idom of n1..n4" "" (idoms succ [ 1; 2; 3; 4 ]);
  line "fixed graph: loop nest" "" (str (Buffer.contents b));
  line "0>2 1>7 2>4,6 3> 4> 5> 6>8 7>4 8>1,3 (it overflowed the stack): idom of n1..n8" ""
    (guard (fun () -> idoms [| [ 2 ]; [ 7 ]; [ 4; 6 ]; []; []; []; [ 8 ]; [ 4 ]; [ 1; 3 ] |] [ 1; 2; 3; 4; 5; 6; 7; 8 ]))
;;
(* loop_nest against a model of natural loops, from the same LCG: a back
   edge is t > h with h dominating t (h = t included); the body of h is h
   and every node that reaches one of h's latches without passing h. Each
   node other than the root sits in the smallest body that contains it
   other than its own; a header heads its loop. Traces are compared with
   every list sorted, since their order follows internal tables. *)
type canon = E of int | T of int * canon list

let rec canon_of_trace l =
  List.sort compare (List.map (function
      | Lm_trace.Elem x -> E x
      | Lm_trace.Lm_trace (x, l) -> T (x, canon_of_trace l)) l)

let () =
  let graphs = 1500 and bad = ref 0 and first = ref "" and raised = ref 0 in
  seed := 211;
  for _ = 1 to graphs do
    let k = 2 + rnd 8 in
    let succ = Array.init k (fun _ -> List.sort_uniq compare (List.init (rnd 3) (fun _ -> rnd k))) in
    let reach ?(without = -1) () =
      let seen = Array.make k false in
      let rec go n = if n <> without && not seen.(n) then begin seen.(n) <- true; List.iter go succ.(n) end in
      go 0; seen in
    let r = reach () in
    let dominates d n = d = n || not (reach ~without:d ()).(n) in
    let nodes = List.init k (fun i -> i) |> List.filter (fun i -> r.(i)) in
    let preds n = List.filter (fun p -> r.(p) && List.mem n succ.(p)) nodes in
    let headers = List.filter (fun h -> h <> 0 && List.exists (fun t -> r.(t) && List.mem h succ.(t) && dominates h t) nodes) nodes in
    let body h =
      let seen = Hashtbl.create 8 in
      Hashtbl.replace seen h ();
      let rec go x = if not (Hashtbl.mem seen x) then begin Hashtbl.replace seen x (); List.iter go (preds x) end in
      List.iter (fun t -> if List.mem h succ.(t) && dominates h t then go t) nodes;
      Hashtbl.fold (fun x () l -> x :: l) seen [] in
    let bodies = List.map (fun h -> (h, body h)) headers in
    let parent n =
      match List.sort (fun (_, b1) (_, b2) -> compare (List.length b1) (List.length b2))
              (List.filter (fun (h, b) -> h <> n && List.mem n b) bodies) with
      | (h, _) :: _ -> h | [] -> 0 in
    let rec items p = List.filter_map (fun n -> if n <> 0 && parent n = p then Some (if List.mem n headers then T (n, items n) else E n) else None) nodes in
    let want = List.sort compare (E 0 :: items 0) in
    let want = let rec fix = function E x -> E x | T (x, l) -> T (x, List.sort compare (List.map fix l)) in List.map fix want in
    let got = try canon_of_trace (L.loop_nest (make succ (List.init (k - 1) (fun i -> i + 1))) sym) with e -> [ E (-1) ] |> fun l -> (raised := !raised + 1; ignore e; l) in
    if got <> want then begin
      incr bad;
      if !first = "" then first := String.concat " " (Array.to_list (Array.mapi (fun i l -> Printf.sprintf "%d>%s" i (String.concat "," (List.map string_of_int l))) succ))
    end
  done;
  line "loop_nest: graphs whose nest differs from natural loops" (string_of_int graphs ^ " graphs")
    (Printf.sprintf "%d%s" !bad (if !bad = 0 then "" else " (first: " ^ !first ^ ")"));
  line "loop_nest: graphs where it raised" "" (string_of_int !raised);
  line "loop_nest: 0 alone (1 unreachable)" "" (guard (fun () -> string_of_int (List.length (L.loop_nest (make [| []; [] |] [ 1 ]) sym))));
  let show succ nodes =
    let rec pr l = String.concat " " (List.map (function E x -> Printf.sprintf "n%d" x | T (x, l) -> Printf.sprintf "[n%d: %s]" x (pr l)) l) in
    pr (canon_of_trace (L.loop_nest (make succ nodes) sym)) in
  line "loop_nest: 0>1 1>2,3 2>4 3>4 4>1,5 (a loop with a branch)" "" (show [| [ 1 ]; [ 2; 3 ]; [ 4 ]; [ 4 ]; [ 1; 5 ]; [] |] [ 1; 2; 3; 4; 5 ]);
  line "loop_nest: 0>1 1>2 2>3 3>2,1,4 (3 closes two loops)" "" (show [| [ 1 ]; [ 2 ]; [ 3 ]; [ 2; 1; 4 ]; [] |] [ 1; 2; 3; 4 ]);
  line "loop_nest: 0>1 1>1,2 (a self-loop)" "" (show [| [ 1 ]; [ 1; 2 ]; [] |] [ 1; 2 ])
