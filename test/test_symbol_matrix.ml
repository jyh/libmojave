(* Lm_symbol_matrix: hash sets and tables keyed by a pair of symbols.
   SymbolMatrix and SymSymbolMatrix ignore the pair's order; AsymSymbolMatrix
   keeps it. iter and fold run in hash order, so their output is sorted. *)
open Pin
open Lm_symbol_matrix

let s = Lm_symbol.add
let a = s "a" and b = s "b" and c = s "c"
let n = Lm_symbol.to_string
let sorted l = list (fun x -> x) (List.sort compare l)

let () =
  let m = SymbolMatrix.create 4 in
  SymbolMatrix.add m a b; SymbolMatrix.add m b a; SymbolMatrix.add m c c;
  line "SymbolMatrix add (a,b) (b,a) (c,c): query (b,a) (a,c) (c,c)" ""
    (Printf.sprintf "%b %b %b" (SymbolMatrix.query m b a) (SymbolMatrix.query m a c) (SymbolMatrix.query m c c));
  let acc = ref [] in
  SymbolMatrix.iter (fun x y -> acc := (n x ^ "," ^ n y) :: !acc) m;
  line "  iter" "" (sorted !acc);
  line "SymbolMatrix.create 0; add" "" (guard (fun () -> SymbolMatrix.add (SymbolMatrix.create 0) a b; "ok"));
  let t = SymSymbolMatrix.create 2 in
  SymSymbolMatrix.add t a b 1; SymSymbolMatrix.add t b a 2; SymSymbolMatrix.add t a c 3;
  line "Sym add (a,b)=1 (b,a)=2 (a,c)=3: find (a,b) (c,a)" "" (guard (fun () -> Printf.sprintf "%d %d" (SymSymbolMatrix.find t a b) (SymSymbolMatrix.find t c a)));
  line "  find (b,c)" "" (guard (fun () -> string_of_int (SymSymbolMatrix.find t b c)));
  let u = AsymSymbolMatrix.create 2 in
  AsymSymbolMatrix.add u a b 1; AsymSymbolMatrix.add u b a 2;
  line "Asym add (a,b)=1 (b,a)=2: find (a,b) (b,a), query (a,a)" "" (guard (fun () -> Printf.sprintf "%d %d %b" (AsymSymbolMatrix.find u a b) (AsymSymbolMatrix.find u b a) (AsymSymbolMatrix.query u a a)));
  let cp = AsymSymbolMatrix.copy u in
  AsymSymbolMatrix.add u a b 10;
  line "  copy, then add (a,b)=10 to the original: copy's (a,b)" "" (string_of_int (AsymSymbolMatrix.find cp a b));
  AsymSymbolMatrix.filter_add u a b (function Some x -> x + 1 | None -> 0);
  AsymSymbolMatrix.filter_add u c c (function Some x -> x + 1 | None -> 0);
  line "  filter_add (a,b) (+1), (c,c) (new 0)" "" (Printf.sprintf "%d %d" (AsymSymbolMatrix.find u a b) (AsymSymbolMatrix.find u c c));
  AsymSymbolMatrix.filter_remove u c c (fun _ -> None);
  AsymSymbolMatrix.filter_remove u a b (fun x -> Some (x * 2));
  line "  filter_remove (c,c) to None, (a,b) doubled" "" (Printf.sprintf "%b %d" (AsymSymbolMatrix.query u c c) (AsymSymbolMatrix.find u a b));
  AsymSymbolMatrix.remove u b a;
  line "  remove (b,a); query (b,a) (a,b)" "" (Printf.sprintf "%b %b" (AsymSymbolMatrix.query u b a) (AsymSymbolMatrix.query u a b));
  let mp = AsymSymbolMatrix.map (fun x -> x + 100) u in
  let acc = ref [] in
  AsymSymbolMatrix.iter (fun x y v -> acc := Printf.sprintf "%s,%s=%d" (n x) (n y) v :: !acc) mp;
  line "  map (+100), iter" "" (sorted !acc);
  line "  fold (sum) of the original" "" (string_of_int (AsymSymbolMatrix.fold (fun acc _ _ v -> acc + v) 0 u))
