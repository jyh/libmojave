(* Lm_rawfloat (and Lm_float80 under it, which keeps a C long double)
   against OCaml's floats. Operands come from a fixed LCG: half edge values
   (zeros, ones, halves, powers of two near the integer limits, the extremes
   of double), half random finite bit patterns. Per operation: the number
   of cases that differ, and the first.
   long double is 80-bit on x86-64 Linux and 64-bit on Apple Silicon and
   MSVC, so an operation done in long double and read back as a double may
   round twice: arithmetic is compared to within one unit in the last place
   (equal, or adjacent doubles), conversions and comparisons exactly.
   NaN is left out of the arithmetic (one line records how compare treats
   it). Integer conversions are compared where the C types hold the value:
   to_int below 2^30, to_int64 and to_rawint below 2^62. Not called:
   Lm_float80's blit functions (blit_float80 asserts that a long double is
   at least 10 bytes, which aborts where it is 8). *)
open Pin
module F = Lm_rawfloat

let seed = ref 61
let rnd30 () = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; !seed
let rnd n = (rnd30 () lsr 14) mod n (* high bits: the low ones have short periods *)
let rnd64 () =
  Int64.logxor (Int64.shift_left (Int64.of_int (rnd30 ())) 34)
    (Int64.logxor (Int64.shift_left (Int64.of_int (rnd30 ())) 17) (Int64.of_int (rnd30 ())))

let edges = [| 0.; -0.; 1.; -1.; 0.5; -0.5; 2.; 3.; 10.; 0.1; -0.1; 1e-300; -1e-300; 1e300; -1e300;
               ldexp 1. 30; ldexp 1. 31; ldexp 1. 53; ldexp 1. 62; ldexp 1. 63; ldexp 1. 64;
               -. ldexp 1. 63; max_float; -. max_float; min_float; epsilon_float; 4.9e-324; infinity;
               neg_infinity |]
let rec finite_bits () =
  let x = Int64.float_of_bits (rnd64 ()) in
  if Float.is_nan x then finite_bits () else x
let value () = if rnd 2 = 0 then edges.(rnd (Array.length edges)) else finite_bits ()
let precs = [| F.Single; F.Double; F.LongDouble |]
let prec () = precs.(rnd 3)

(* equal, both NaN, or adjacent doubles of one sign *)
let near a b =
  a = b || (Float.is_nan a && Float.is_nan b)
  || (Float.sign_bit a = Float.sign_bit b && not (Float.is_nan a || Float.is_nan b)
      && Int64.abs (Int64.sub (Int64.bits_of_float a) (Int64.bits_of_float b)) = 1L)
let same a b = Int64.bits_of_float a = Int64.bits_of_float b || (Float.is_nan a && Float.is_nan b)

let n = 3000

let check name case =
  seed := 61;
  let bad = ref 0 and first = ref "" and tried = ref 0 in
  for _ = 1 to n do
    match case () with
    | None -> ()
    | Some (ok, desc) ->
        incr tried;
        if not ok then begin incr bad; if !first = "" then first := desc () end
  done;
  line (name ^ ": cases that differ") (string_of_int !tried ^ " cases")
    (Printf.sprintf "%d%s" !bad (if !bad = 0 then "" else " (first: " ^ !first ^ ")"))

let f x = Printf.sprintf "%h" x
let gotf r = guard (fun () -> f (F.to_float r))

let () =
  check "of_float, to_float (exact)" (fun () ->
      let x = value () in
      let r = F.of_float (prec ()) x in
      Some (same (F.to_float r) x, fun () -> Printf.sprintf "%s: got %s" (f x) (gotf r)));
  check "of_int, to_float (|i| < 2^30, exact)" (fun () ->
      let i = (rnd30 () lsl 1) - 0x3fffffff in
      let r = F.of_int (prec ()) i in
      Some (same (F.to_float r) (float_of_int i), fun () -> Printf.sprintf "%d: got %s" i (gotf r)));
  check "to_int (|x| < 2^30, truncated)" (fun () ->
      let x = value () in
      if not (Float.abs x < ldexp 1. 30) then None
      else
        let got = (try F.to_int (F.of_float F.Double x) with _ -> min_int) in
        Some (got = truncate x, fun () -> Printf.sprintf "%s: got %d, want %d" (f x) got (truncate x)));
  check "to_int64 (|x| < 2^62, truncated)" (fun () ->
      let x = value () in
      if not (Float.abs x < ldexp 1. 62) then None
      else
        let got = F.to_int64 (F.of_float F.Double x) in
        Some (got = Int64.of_float x, fun () -> Printf.sprintf "%s: got %Ld" (f x) got));
  check "to_rawint Int64 signed (|x| < 2^62)" (fun () ->
      let x = value () in
      if not (Float.abs x < ldexp 1. 62) then None
      else
        let got = Lm_rawint.to_int64 (F.to_rawint Lm_rawint.Int64 true (F.of_float F.Double x)) in
        Some (got = Int64.of_float x, fun () -> Printf.sprintf "%s: got %Ld" (f x) got));
  check "of_rawint Int64, signed and unsigned (within 1 ulp)" (fun () ->
      let v = rnd64 () and s = rnd 2 = 0 in
      let want =
        if s || v >= 0L then Int64.to_float v
        else 2. *. Int64.to_float (Int64.logor (Int64.shift_right_logical v 1) (Int64.logand v 1L)) in
      let r = F.of_rawint F.Double (Lm_rawint.of_int64 Lm_rawint.Int64 s v) in
      Some (near (F.to_float r) want,
            fun () -> Printf.sprintf "%s %Ld: got %s, want %s" (if s then "signed" else "unsigned") v (gotf r) (f want)));
  check "of_string (%.17g of x), exact" (fun () ->
      let x = value () in
      let s = Printf.sprintf "%.17g" x in
      let r = F.of_string (prec ()) s in
      Some (same (F.to_float r) x, fun () -> Printf.sprintf "%S: got %s" s (gotf r)));
  check "of_string (to_string x) = x (how metaprl's MCC terms store a rawfloat)" (fun () ->
      let x = value () in
      let s = F.to_string (F.of_float F.Double x) in
      let r = F.of_string F.Double s in
      Some (same (F.to_float r) x, fun () -> Printf.sprintf "%s: to_string %S" (f x) s));
  (* arithmetic *)
  let unary name op m =
    check (name ^ " (within 1 ulp)") (fun () ->
        let x = value () in
        let r = (try op (F.of_float (prec ()) x) with e -> F.of_string F.Double (Printexc.to_string e)) in
        Some (near (F.to_float r) (m x), fun () -> Printf.sprintf "%s: got %s, want %s" (f x) (gotf r) (f (m x)))) in
  unary "neg" F.neg (fun x -> -. x);
  unary "abs" F.abs Float.abs;
  unary "succ" F.succ (fun x -> x +. 1.);
  unary "pred" F.pred (fun x -> x -. 1.);
  unary "sqrt" F.sqrt sqrt;
  unary "sin" F.sin sin;
  unary "cos" F.cos cos;
  let binary name op m =
    check (name ^ " (within 1 ulp)") (fun () ->
        let x = value () and y = value () in
        let p = prec () in
        let r = (try op (F.of_float p x) (F.of_float p y) with _ -> F.of_float F.Double nan) in
        let want = m x y in
        Some (near (F.to_float r) want,
              fun () -> Printf.sprintf "%s, %s: got %s, want %s" (f x) (f y) (gotf r) (f want))) in
  binary "add" F.add ( +. );
  binary "sub" F.sub ( -. );
  binary "mul" F.mul ( *. );
  binary "div" F.div ( /. );
  binary "atan2" F.atan2 atan2;
  binary "min" F.min (fun x y -> if x < y then x else y);
  binary "max" F.max (fun x y -> if x > y then x else y);
  check "compare (sign; -0 = 0)" (fun () ->
      let x = value () and y = value () in
      let c = compare (F.compare (F.of_float F.Double x) (F.of_float F.Double y)) 0 in
      let want = if x < y then -1 else if x > y then 1 else 0 in
      Some (c = want, fun () -> Printf.sprintf "%s, %s: got %d" (f x) (f y) c));
  check "is_zero" (fun () ->
      let x = value () in
      let z = F.is_zero (F.of_float F.Double x) in
      Some (z = (x = 0.), fun () -> Printf.sprintf "%s: got %b" (f x) z));
  (* fixed cases *)
  let d = F.of_float F.Double in
  line "to_int 7.0, -7.0, 1e9" "" (String.concat " " (List.map (fun x -> guard (fun () -> string_of_int (F.to_int (d x)))) [ 7.; -7.; 1e9 ]));
  line "of_rawint unsigned Int64 max (2^64 - 1)" "" (guard (fun () -> f (F.to_float (F.of_rawint F.Double (Lm_rawint.max_int Lm_rawint.Int64 false)))));
  (* values exact in every width, so the line is the same where long double is 64- or 80-bit *)
  line "to_string 0.5, -2.25, 123456789, 2^40" "" (String.concat " " (List.map (fun x -> guard (fun () -> str (F.to_string (d x)))) [ 0.5; -2.25; 123456789.; ldexp 1. 40 ]));
  line "compare nan nan, nan 1, 1 nan" "" (String.concat " " (List.map (fun (x, y) -> guard (fun () -> string_of_int (F.compare (d x) (d y)))) [ nan, nan; nan, 1.; 1., nan ]));
  line "rem 7 2" "" (guard (fun () -> gotf (F.rem (d 7.) (d 2.))));
  line "Single 0.1 (precision is a tag: not rounded)" "" (guard (fun () ->
      let r = F.of_float F.Single 0.1 in
      Printf.sprintf "%s, single would be %s" (gotf r) (f (Int32.float_of_bits (Int32.bits_of_float 0.1)))));
  line "of_rawfloat Single of a Double 1/3" "" (guard (fun () -> gotf (F.of_rawfloat F.Single (d (1. /. 3.)))))
