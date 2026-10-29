(* Lm_num against a model, from a fixed LCG. Lm_num keeps a small int
   until an operation might overflow, then switches to Lm_big_int; its
   division truncates toward zero, as OCaml's ints do (mod takes the
   sign of the dividend). The model is a separate bignum (sign and base
   10000 digits, below), checked first against native ints where they
   cannot overflow. Operands mix small ints, values near max_int and
   min_int, and huge values built from strings; after each operation
   the result is compared as a decimal string (compare_num by its sign:
   it returns any negative or positive int, as compare may). power_num
   is checked for exponents 0-12. Fixed cases follow. *)
open Pin
module N = Lm_num

let seed = ref 103
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n

(* The reference: neg, and digits little-endian in base 10000, no
   leading zeros; zero is (false, []). *)

let base = 10000
let rec strip = function [] -> [] | l -> (match List.rev l with 0 :: r -> strip (List.rev r) | _ -> l)
let norm (n, d) = let d = strip d in if d = [] then (false, []) else (n, d)
let cmp_mag a b =
  let la = List.length a and lb = List.length b in
  if la <> lb then compare la lb else compare (List.rev a) (List.rev b)
let rec add_mag a b c =
  match a, b with
  | [], [] -> if c = 0 then [] else [ c ]
  | x :: a, [] | [], x :: a -> let s = x + c in (s mod base) :: add_mag a [] (s / base)
  | x :: a, y :: b -> let s = x + y + c in (s mod base) :: add_mag a b (s / base)
let rec sub_mag a b c = (* a >= b *)
  match a, b with
  | [], _ -> []
  | x :: a, [] -> let s = x - c in if s < 0 then (s + base) :: sub_mag a [] 1 else s :: sub_mag a [] 0
  | x :: a, y :: b -> let s = x - y - c in if s < 0 then (s + base) :: sub_mag a b 1 else s :: sub_mag a b 0
let add (na, a) (nb, b) =
  if na = nb then norm (na, add_mag a b 0)
  else if cmp_mag a b >= 0 then norm (na, sub_mag a b 0) else norm (nb, sub_mag b a 0)
let neg (n, d) = norm (not n, d)
let sub a b = add a (neg b)
let mul_small a k = strip (let rec go a c = match a with [] -> if c = 0 then [] else (c mod base) :: go [] (c / base) | x :: a -> let s = x * k + c in (s mod base) :: go a (s / base) in go a 0)
let mul (na, a) (nb, b) =
  let r = List.fold_right (fun y acc -> add_mag (mul_small a y) (0 :: acc) 0) b [] in
  norm (na <> nb, strip r)
(* Long division of magnitudes: quotient and remainder. *)
let divmod_mag a b =
  let digits = List.rev a in
  let q, r =
    List.fold_left (fun (q, r) d ->
        let r = strip (d :: r) in
        let lo = ref 0 and hi = ref (base - 1) in
        while !lo < !hi do
          let mid = (!lo + !hi + 1) / 2 in
          if cmp_mag (mul_small b mid) r <= 0 then lo := mid else hi := mid - 1
        done;
        (!lo :: q, strip (sub_mag r (mul_small b !lo) 0))) ([], []) digits in
  (strip q, r)
(* Truncating division: the quotient's sign is the product of the signs,
   the remainder's is the dividend's. *)
let quo (na, a) (nb, b) = let q, _ = divmod_mag a b in norm (na <> nb, q)
let rem (na, a) (_, b) = let _, r = divmod_mag a b in norm (na, r)
let of_string s =
  let neg, s = if s <> "" && s.[0] = '-' then (true, String.sub s 1 (String.length s - 1)) else (false, s) in
  let n = String.length s in
  let rec go i acc = if i <= 0 then acc else let j = max 0 (i - 4) in go j (int_of_string (String.sub s j (i - j)) :: acc) in
  norm (neg, List.rev (go n []))
let to_string (n, d) =
  match List.rev d with
  | [] -> "0"
  | x :: r -> (if n then "-" else "") ^ string_of_int x ^ String.concat "" (List.map (Printf.sprintf "%04d") r)
let of_int i = of_string (string_of_int i)

let bad = Hashtbl.create 8 and first = Hashtbl.create 8 and order = ref [] and steps = Hashtbl.create 8
let check op ok d =
  if not (List.mem op !order) then order := op :: !order;
  Hashtbl.replace steps op (1 + try Hashtbl.find steps op with Not_found -> 0);
  if not ok then begin
    Hashtbl.replace bad op (1 + try Hashtbl.find bad op with Not_found -> 0);
    if not (Hashtbl.mem first op) then Hashtbl.replace first op (d ())
  end

(* The control: the reference against native ints, where no overflow
   is possible. *)
let () =
  for _ = 1 to 3000 do
    let a = rnd 2000001 - 1000000 and b = rnd 2001 - 1000 in
    let ra = of_int a and rb = of_int b in
    check "reference vs native: add" (to_string (add ra rb) = string_of_int (a + b)) (fun () -> Printf.sprintf "%d + %d" a b);
    check "reference vs native: sub" (to_string (sub ra rb) = string_of_int (a - b)) (fun () -> Printf.sprintf "%d - %d" a b);
    check "reference vs native: mul" (to_string (mul ra rb) = string_of_int (a * b)) (fun () -> Printf.sprintf "%d * %d" a b);
    if b <> 0 then begin
      check "reference vs native: quo" (to_string (quo ra rb) = string_of_int (a / b)) (fun () -> Printf.sprintf "%d / %d" a b);
      check "reference vs native: rem" (to_string (rem ra rb) = string_of_int (a mod b)) (fun () -> Printf.sprintf "%d mod %d" a b)
    end
  done

let operand () =
  match rnd 6 with
  | 0 -> string_of_int (rnd 200 - 100)
  | 1 -> string_of_int (rnd 70000 - 35000)
  | 2 -> string_of_int (max_int - rnd 3)
  | 3 -> string_of_int (min_int + rnd 3)
  | 4 -> (if rnd 2 = 0 then "-" else "") ^ string_of_int (1 + rnd 9) ^ String.init (19 + rnd 15) (fun _ -> Char.chr (48 + rnd 10))
  | _ -> string_of_int (rnd 2000000000 - 1000000000)

let guard f = try f () with e -> "EXN " ^ Printexc.to_string e

let () =
  for _ = 1 to 6000 do
    let sa = operand () and sb = operand () in
    (* An operand that fits an int is built either way: num_of_string
       makes min_int a bignum, num_of_int (and arithmetic that lands on
       it) keeps it small. *)
    let mk s = match int_of_string_opt s with Some i when rnd 2 = 0 -> N.num_of_int i | _ -> N.num_of_string s in
    let a = mk sa and b = mk sb and ra = of_string sa and rb = of_string sb in
    let zero = to_string rb = "0" in
    let ops = [
      ("add_num", (fun () -> N.string_of_num (N.add_num a b)), (fun () -> to_string (add ra rb)));
      ("sub_num", (fun () -> N.string_of_num (N.sub_num a b)), (fun () -> to_string (sub ra rb)));
      ("mult_num", (fun () -> N.string_of_num (N.mult_num a b)), (fun () -> to_string (mul ra rb)));
      ("neg_num", (fun () -> N.string_of_num (N.neg_num a)), (fun () -> to_string (neg ra)));
      ("abs_num", (fun () -> N.string_of_num (N.abs_num a)), (fun () -> to_string (if fst ra then neg ra else ra)));
      ("compare_num (sign)", (fun () -> string_of_int (compare (N.compare_num a b) 0)), (fun () -> let d = sub ra rb in string_of_int (if snd d = [] then 0 else if fst d then -1 else 1)));
    ] @ (if zero then [] else [
      ("div_num", (fun () -> N.string_of_num (N.div_num a b)), (fun () -> to_string (quo ra rb)));
      ("mod_num", (fun () -> N.string_of_num (N.mod_num a b)), (fun () -> to_string (rem ra rb)));
    ]) in
    List.iter (fun (name, got, want) ->
        let g = guard got and w = want () in
        check name (g = w) (fun () -> Printf.sprintf "%s %s %s: Lm_num %s, model %s" name sa sb g w)) ops
  done;
  for _ = 1 to 600 do
    let sa = operand () and e = rnd 13 in
    let a = N.num_of_string sa and ra = of_string sa in
    let rec pw k = if k = 0 then of_int 1 else mul ra (pw (k - 1)) in
    let g = guard (fun () -> N.string_of_num (N.power_num a (N.num_of_int e))) and w = to_string (pw e) in
    check "power_num" (g = w) (fun () -> Printf.sprintf "power_num %s %d: Lm_num %s, model %s" sa e g w)
  done;
  List.iter (fun op ->
      let k = try Hashtbl.find bad op with Not_found -> 0 in
      line op (string_of_int (Hashtbl.find steps op) ^ " cases")
        (if k = 0 then "0 disagree" else Printf.sprintf "%d disagree (first: %s)" k (Hashtbl.find first op)))
    (List.rev !order)

(* Fixed cases: the int edges, and a negative exponent (power_num
   counted the exponent down to zero; from a negative one it never got
   there, so it is not run here). *)
let () =
  let s n = N.string_of_num n in
  let mi = N.num_of_int min_int in
  line "div_num min_int (-1)" "" (guard (fun () -> s (N.div_num mi (N.num_of_int (-1)))));
  line "mod_num min_int (-1)" "" (guard (fun () -> s (N.mod_num mi (N.num_of_int (-1)))));
  line "neg_num min_int" "" (guard (fun () -> s (N.neg_num mi)));
  line "abs_num min_int" "" (guard (fun () -> s (N.abs_num mi)));
  line "mult_num min_int (-1)" "" (guard (fun () -> s (N.mult_num mi (N.num_of_int (-1)))));
  line "pred_num min_int" "" (guard (fun () -> s (N.pred_num mi)))

(* A negative exponent: before the fix power_num looped (it counted the
   exponent down and never reached zero), so this comes with the fix. *)
let () =
  line "power_num 2 (-1)" "" (guard (fun () -> N.string_of_num (N.power_num (N.num_of_int 2) (N.num_of_int (-1)))));
  line "power_num 2 (-(10^30))" "" (guard (fun () -> N.string_of_num (N.power_num (N.num_of_int 2) (N.num_of_string "-1000000000000000000000000000000"))))
