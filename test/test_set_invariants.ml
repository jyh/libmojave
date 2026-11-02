(* Lm_set's red-black invariants, checked by the library's own check
   (LmMakeDebug), which no test called: no red node under a red node, the
   same number of black nodes on every path, keys in order, sizes right.
   300 random sequences of add / remove / union / inter / diff / filter /
   partition / of_list / add_list / subtract_list; every tree built is
   checked (a broken invariant raises), and its elements are compared with
   Stdlib.Set. A fixed linear congruential generator. *)
open Pin
module Ord = struct type t = int let compare = compare let print out i = Lm_printf.fprintf out "%d" i end
module S = Lm_set.LmMakeDebug (Ord)
module M = Set.Make (Int)

let state = ref 4242
let rnd n = state := (!state * 1103515245 + 12345) land 0x7fffffff; (!state lsr 8) mod n

let trees = ref 0 and broken = ref 0 and first_broken = ref "" and wrong = ref 0 and first_wrong = ref ""
let seen what t m =
  incr trees;
  (match S.check t with
   | _ -> ()
   | exception e -> incr broken; if !first_broken = "" then first_broken := what () ^ ": " ^ Printexc.to_string e);
  if S.elements t <> M.elements m then (incr wrong; if !first_wrong = "" then first_wrong := what ())

let gen () =
  let n = rnd 40 in
  let l = List.init n (fun _ -> rnd 60) in
  S.of_list l, M.of_list l, l

let () =
  for round = 1 to 300 do
    let t, m, l0 = gen () in
    let log = ref [ Printf.sprintf "round %d of_list %d" round (List.length l0) ] in
    let what () = String.concat ", " (List.rev !log) in
    seen what t m;
    let t = ref t and m = ref m in
    for _ = 1 to 25 do
      let x = rnd 60 in
      let t', m', op =
        match rnd 10 with
        | 0 | 1 -> S.add !t x, M.add x !m, Printf.sprintf "add %d" x
        | 2 | 3 -> S.remove !t x, M.remove x !m, Printf.sprintf "remove %d" x
        | 4 -> let u, v, _ = gen () in S.union !t u, M.union !m v, "union"
        | 5 -> let u, v, _ = gen () in S.inter !t u, M.inter !m v, "inter"
        | 6 -> let u, v, _ = gen () in S.diff !t u, M.diff !m v, "diff"
        | 7 -> S.filter (fun y -> y mod 3 <> x mod 3) !t, M.filter (fun y -> y mod 3 <> x mod 3) !m, "filter"
        | 8 -> let l = List.init (rnd 8) (fun _ -> rnd 60) in S.add_list !t l, List.fold_left (fun m y -> M.add y m) !m l, "add_list"
        | _ -> let l = List.init (rnd 8) (fun _ -> rnd 60) in S.subtract_list !t l, List.fold_left (fun m y -> M.remove y m) !m l, "subtract_list" in
      log := op :: !log;
      seen what t' m';
      t := t'; m := m'
    done;
    let p = fun y -> y < 30 in
    let a, b = S.partition p !t in
    let a', b' = M.partition p !m in
    seen (fun () -> what () ^ ", partition (in)") a a';
    seen (fun () -> what () ^ ", partition (out)") b b'
  done;
  line "Lm_set trees checked by check (red-black invariants)" (string_of_int !trees ^ " trees")
    (if !broken = 0 then "0 broken" else Printf.sprintf "%d broken (first: %s)" !broken !first_broken);
  line "Lm_set elements vs Stdlib.Set" (string_of_int !trees ^ " trees")
    (if !wrong = 0 then "0 differ" else Printf.sprintf "%d differ (first: %s)" !wrong !first_wrong)
