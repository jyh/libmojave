(* Lm_interval_set against a brute-force model.
   Countable: ints 0..20, a set is a bool array.
   Dense: ints used as a dense order; generated endpoints are even, so an
   odd point stands for "strictly between" and tells Open from Closed.
   Cases come from a fixed LCG (Random differs between OCaml versions).
   Each line counts the cases where the library and the model disagree,
   with the first such case. *)
open Lm_interval_set

let seed = ref 12345
let nonempty_only = ref false
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n

module type M = sig
  module S : IntervalSetSig with type elt = int
  val lo : int val hi : int          (* the domain sampled *)
  val gen_point : unit -> int        (* an endpoint *)
  val name : string
end

module Check (X : M) = struct
  open X
  let n = hi - lo + 1
  let model_of lb ub =
    Array.init n (fun i ->
        let p = lo + i in
        (match lb with Infinity -> true | Open a -> p > a | Closed a -> p >= a)
        && (match ub with Infinity -> true | Open b -> p < b | Closed b -> p <= b))
  let gen_bound () = match rnd 5 with 0 -> Infinity | 1 | 2 -> Open (gen_point ()) | _ -> Closed (gen_point ())
  let show_bound = function Infinity -> "inf" | Open a -> "(" ^ string_of_int a | Closed a -> "[" ^ string_of_int a
  let show_ubound = function Infinity -> "inf" | Open b -> string_of_int b ^ ")" | Closed b -> string_of_int b ^ "]"
  let show s = String.concat " " (S.fold (fun acc l u -> acc @ [ show_bound l ^ "," ^ (match u with Infinity -> "inf" | Open b -> string_of_int b ^ ")" | Closed b -> string_of_int b ^ "]") ]) [] s)
  let showm m = String.concat "" (Array.to_list (Array.map (fun b -> if b then "1" else "0") m))
  let fails = Hashtbl.create 16 and first = Hashtbl.create 16
  let check op ok detail =
    if not ok then begin
      Hashtbl.replace fails op (1 + try Hashtbl.find fails op with Not_found -> 0);
      if not (Hashtbl.mem first op) then Hashtbl.replace first op (detail ())
    end else if not (Hashtbl.mem fails op) then Hashtbl.replace fails op 0
  let to_model s = Array.init n (fun i -> S.mem_point (lo + i) s)
  (* a set built as a union of 0..3 intervals, with its model *)
  let gen () =
    let k = rnd 4 in
    let rec go k s m =
      if k = 0 then s, m
      else
        let rec pick () =
          let lb = gen_bound () and ub = gen_bound () in
          let m' = model_of lb ub in
          if !nonempty_only && not (Array.exists (fun x -> x) m') then pick () else lb, ub, m' in
        let lb, ub, m' = pick () in
        let iv =
          try S.of_interval lb ub with e ->
            check "of_interval raises" false (fun () -> Printf.sprintf "of_interval %s %s: %s" (show_bound lb) (show_ubound ub) (Printexc.to_string e));
            raise Exit
        in
        check "of_interval raises" true (fun () -> "");
        go (k - 1) (S.union s iv) (Array.map2 ( || ) m m')
    in
    go k S.empty (Array.make n false)
  let run cases =
    for _ = 1 to cases do
      match (let a = gen () in let b = gen () in a, b) with
      | exception Exit -> ()
      | (a, ma), (b, mb) ->
      let d2 () = Printf.sprintf "a={%s} b={%s}" (show a) (show b) in
      check "of_interval/union (mem_point)" (to_model a = ma) (fun () -> Printf.sprintf "a={%s} got %s want %s" (show a) (showm (to_model a)) (showm ma));
      check "union" (to_model (S.union a b) = Array.map2 ( || ) ma mb) d2;
      check "isect" (to_model (S.isect a b) = Array.map2 ( && ) ma mb) d2;
      check "subtract" (to_model (S.subtract a b) = Array.map2 (fun x y -> x && not y) ma mb) d2;
      check "negate" (to_model (S.negate a) = Array.map not ma) d2;
      check "subset" (S.subset a b = Array.for_all2 (fun x y -> (not x) || y) ma mb) d2;
      check "equal" (S.equal a b = (ma = mb)) d2;
      check "is_empty" (S.is_empty a = not (Array.exists (fun x -> x) ma)) d2;
      let p = lo + rnd n in
      check "add_point" (to_model (S.add_point a p) = Array.mapi (fun i x -> x || lo + i = p) ma) (fun () -> d2 () ^ " p=" ^ string_of_int p);
      check "subtract_point" (to_model (S.subtract_point a p) = Array.mapi (fun i x -> x && lo + i <> p) ma) (fun () -> d2 () ^ " p=" ^ string_of_int p);
      check "of_point" (to_model (S.of_point p) = Array.init n (fun i -> lo + i = p)) (fun () -> "p=" ^ string_of_int p)
    done;
    let ops = List.sort compare (Hashtbl.fold (fun k _ acc -> k :: acc) fails []) in
    List.iter (fun op ->
        let k = Hashtbl.find fails op in
        Printf.printf "%s%s %s: %d of %d disagree%s\n" name (if !nonempty_only then " (non-empty intervals)" else "") op k cases
          (if k = 0 then "" else " (first: " ^ Hashtbl.find first op ^ ")")) ops
end

module CountableArg = struct
  module S = CountableIntervalSet (struct type t = int let compare = compare let succ = succ let pred = pred let min = 0 let max = 20 end)
  let lo = 0 and hi = 20
  let gen_point () = rnd 21
  let name = "countable"
end

module DenseArg = struct
  module S = DenseIntervalSet (struct type t = int let compare = compare end)
  let lo = -2 and hi = 22
  let gen_point () = 2 * rnd 11
  let name = "dense"
end

module Countable = Check (CountableArg)
module Dense = Check (DenseArg)

let () =
  Countable.run 3000;
  Dense.run 3000;
  nonempty_only := true;
  Hashtbl.reset Countable.fails; Hashtbl.reset Countable.first;
  Hashtbl.reset Dense.fails; Hashtbl.reset Dense.first;
  Countable.run 3000;
  Dense.run 3000
