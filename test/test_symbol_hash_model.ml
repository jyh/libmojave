(* Lm_symbol_hash (hash-consed symbols) by property, on symbols make s i
   from a fixed LCG (s over a b x 0 1 9 _, i from 0 to 120), as
   test_symbol_model does for Lm_symbol:
   - add inverts string_of_symbol (not for make "010" 0, which add never
     makes; test_symbol_model shows that case);
   - compare is a total order agreeing with eq (antisymmetric,
     transitive on triples, 0 exactly when eq), and eq values hash alike;
   - compare_pair, compare_triple and compare_lists are compare,
     lexicographically, a shorter list first;
   - new_symbol, new_symbol_string, new_symbol_pre give a fresh number
     and keep the name; new_name_gen returns f's answer on the first
     candidate it accepts; reintern v is eq v (re-interning, as after
     unmarshaling, keeps the symbol).
   string_of_ext_symbol is left to test_symbol_model. Per check: how many
   cases differ, and the first. *)
open Pin
module Y = Lm_symbol_hash

let seed = ref 107
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 12) mod n
let name () = let a = "abx019_" in String.init (1 + rnd 6) (fun _ -> a.[rnd (String.length a)])
let leading_zero_numeral s = s.[0] = '0' && String.for_all (fun c -> c >= '0' && c <= '9') s
let rec sym () =
  let s = name () and i = if rnd 3 = 0 then 0 else rnd 121 in
  if i = 0 && leading_zero_numeral s then sym () else Y.make s i
let show v = Printf.sprintf "(%d, %S)" (Y.to_int v) (Y.to_string v)
let sign c = compare c 0

let check name n case =
  seed := 107;
  let bad = ref 0 and first = ref "" in
  for _ = 1 to n do
    match case () with
    | true, _ -> ()
    | false, d -> incr bad; if !first = "" then first := d ()
  done;
  line (name ^ ": cases that differ") (string_of_int n ^ " cases")
    (Printf.sprintf "%d%s" !bad (if !bad = 0 then "" else " (first: " ^ !first ^ ")"))

let rec lex = function
  | x :: xs, y :: ys -> let c = sign (Y.compare x y) in if c <> 0 then c else lex (xs, ys)
  | [], [] -> 0 | [], _ -> -1 | _, [] -> 1

let () =
  check "add (string_of_symbol v) is eq v" 3000 (fun () ->
      let v = sym () in
      let s = Y.string_of_symbol v in
      let back = Y.add s in
      Y.eq back v, (fun () -> Printf.sprintf "%s -> %S -> %s" (show v) s (show back)));
  check "compare: 0 iff eq; antisymmetric; transitive; eq values hash alike" 3000 (fun () ->
      let a = sym () and b = if rnd 3 = 0 then Y.make "a" (rnd 3) else sym () and c = sym () in
      let a' = Y.make (Y.to_string a) (Y.to_int a) in
      let ab = sign (Y.compare a b) and ba = sign (Y.compare b a) and bc = sign (Y.compare b c) and ac = sign (Y.compare a c) in
      let ok = (ab = 0) = Y.eq a b && ab = - ba && (not (ab <= 0 && bc <= 0) || ac <= 0)
               && Y.eq a a' && Y.compare a a' = 0 && Y.hash a = Y.hash a' in
      ok, (fun () -> Printf.sprintf "%s %s %s" (show a) (show b) (show c)));
  check "compare_pair, compare_triple, compare_lists: lexicographic" 3000 (fun () ->
      let l1 = List.init (rnd 4) (fun _ -> sym ()) in
      let l2 = if rnd 2 = 0 then l1 @ List.init (rnd 2) (fun _ -> sym ()) else List.init (rnd 4) (fun _ -> sym ()) in
      let a = sym () and b = sym () and c = sym () and b' = if rnd 2 = 0 then sym () else Y.make "q" 1 in
      let ok = sign (Y.compare_lists l1 l2) = lex (l1, l2)
               && sign (Y.compare_pair (a, b) (a, b')) = lex ([ a; b ], [ a; b' ])
               && sign (Y.compare_triple (a, b, c) (a, b', c)) = lex ([ a; b; c ], [ a; b'; c ]) in
      ok, (fun () -> String.concat " " (List.map show l1) ^ " / " ^ String.concat " " (List.map show l2)));
  seed := 107;
  let seen = Hashtbl.create 100 and bad = ref 0 and first = ref "" in
  for k = 1 to 3000 do
    let v = sym () in
    Hashtbl.replace seen (Y.to_int v) ();
    let f, how = match k mod 3 with
      | 0 -> Y.new_symbol v, "new_symbol"
      | 1 -> Y.new_symbol_string (Y.to_string v), "new_symbol_string"
      | _ -> Y.new_symbol_pre "p" v, "new_symbol_pre" in
    let ok = not (Hashtbl.mem seen (Y.to_int f)) && Y.to_string f = Y.to_string v && Y.to_int f > 0 in
    if not ok then begin incr bad; if !first = "" then first := Printf.sprintf "%s %s -> %s" how (show v) (show f) end;
    Hashtbl.replace seen (Y.to_int f) ()
  done;
  line "new_symbol, new_symbol_string, new_symbol_pre: fresh number, same name: cases that differ" "3000 cases"
    (Printf.sprintf "%d%s" !bad (if !bad = 0 then "" else " (first: " ^ !first ^ ")"));
  check "new_name_gen: the first make v i that f accepts" 1000 (fun () ->
      let v = sym () in
      let stop = rnd 6 in
      let got = Y.new_name_gen v (fun c -> if Y.to_int c >= stop then Some (Y.to_int c, Y.to_string c) else None) in
      got = (stop, Y.to_string v), (fun () -> Printf.sprintf "%s stop %d: got (%d, %S)" (show v) stop (fst got) (snd got)));
  check "reintern v is eq v; is_interned = number 0" 3000 (fun () ->
      let v = sym () in
      Y.eq (Y.reintern v) v && Y.is_interned v = (Y.to_int v = 0), (fun () -> show v))
