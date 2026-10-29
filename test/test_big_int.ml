(* Lm_big_int (the bignum under Lm_num) against native ints, from a fixed
   LCG: operands up to 2^40 in magnitude, so every sum and difference fits
   in 63 bits; a product that overflows the native int is skipped, and the
   line says how many were compared. Per operation, the number of cases
   whose decimal string differs from the native result. Division is checked
   against OCaml's truncated / and mod, and against the identity
   a = b * q + r with |r| < |b|, which any sign convention must keep.
   Then identities on numbers beyond native ints. *)
open Pin
module B = Lm_big_int

let seed = ref 83
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n
let operand () =
  let mag = match rnd 4 with
    | 0 -> rnd 10
    | 1 -> rnd 40000
    | 2 -> rnd 0x3fffffff
    | _ -> (rnd 0x3fffffff * 1024) + rnd 1024 in
  if rnd 2 = 0 then mag else - mag
let s = B.string_of_big_int
let b = B.big_int_of_int

let () =
  let n = 4000 in
  let ops = [
    "add", (fun x y -> Some (x + y)), (fun x y -> B.add_big_int x y);
    "sub", (fun x y -> Some (x - y)), (fun x y -> B.sub_big_int x y);
    "mult", (fun x y -> if x <> 0 && (x * y) / x <> y then None else Some (x * y)), (fun x y -> B.mult_big_int x y);
    "div (truncated)", (fun x y -> if y = 0 then None else Some (x / y)), (fun x y -> B.div_big_int x y);
    "mod (truncated)", (fun x y -> if y = 0 then None else Some (x mod y)), (fun x y -> B.mod_big_int x y);
  ] in
  List.iter (fun (name, native, big) ->
      seed := 83;
      let bad = ref 0 and first = ref "" and tried = ref 0 in
      for _ = 1 to n do
        let x = operand () and y = operand () in
        match native x y with
        | None -> ()
        | Some r ->
            incr tried;
            let got = try s (big (b x) (b y)) with e -> Printexc.to_string e in
            if got <> string_of_int r then begin
              incr bad;
              if !first = "" then first := Printf.sprintf "%d, %d: %s, native %d" x y got r
            end
      done;
      line (name ^ ": cases that differ from native ints") (string_of_int !tried ^ " pairs")
        (Printf.sprintf "%d%s" !bad (if !bad = 0 then "" else " (first: " ^ !first ^ ")"))) ops;
  (* a = b * q + r, |r| < |b|, by the library's own div and mod *)
  seed := 83;
  let bad = ref 0 and first = ref "" and tried = ref 0 in
  for _ = 1 to n do
    let x = operand () and y = operand () in
    if y <> 0 then begin
      incr tried;
      let q = B.div_big_int (b x) (b y) and r = B.mod_big_int (b x) (b y) in
      let ok = B.eq_big_int (B.add_big_int (B.mult_big_int (b y) q) r) (b x)
               && B.compare_big_int (B.abs_big_int r) (B.abs_big_int (b y)) < 0 in
      if not ok then begin incr bad; if !first = "" then first := Printf.sprintf "%d, %d: q %s r %s" x y (s q) (s r) end
    end
  done;
  line "a = b * div a b + mod a b, |mod| < |b|" (string_of_int !tried ^ " pairs") (Printf.sprintf "%d%s" !bad (if !bad = 0 then "" else " (first: " ^ !first ^ ")"));
  (* comparison, conversion, zero *)
  seed := 83;
  let bad_cmp = ref 0 and bad_str = ref 0 and bad_int = ref 0 in
  for _ = 1 to n do
    let x = operand () and y = operand () in
    if compare (B.compare_big_int (b x) (b y)) 0 <> compare (compare x y) 0 || B.eq_big_int (b x) (b y) <> (x = y) then incr bad_cmp;
    if s (B.big_int_of_string (string_of_int x)) <> string_of_int x then incr bad_str;
    (match B.integer_big_int (b x) with v -> if v <> x then incr bad_int | exception _ -> if abs x < 1 lsl 30 then incr bad_int)
  done;
  line "compare_big_int / eq_big_int: cases that differ" "" (string_of_int !bad_cmp);
  line "big_int_of_string (string_of_int x): cases that differ" "" (string_of_int !bad_str);
  line "integer_big_int: cases that differ (or raise below 2^30)" "" (string_of_int !bad_int);
  line "integer_big_int 2^30, 2^40" "" (String.concat " " (List.map (fun x -> guard (fun () -> string_of_int (B.integer_big_int (b x)))) [ 1 lsl 30; 1 lsl 40 ]));
  let z = B.sub_big_int (b (-3)) (b (-3)) and z2 = B.add_big_int (b (-3)) (b 3) and z3 = B.mult_big_int (b (-5)) (b 0) in
  line "zero from -3 - -3, -3 + 3, -5 * 0" "" (String.concat " " (List.map s [ z; z2; z3 ]));
  line "  equal to zero, compare with zero" "" (String.concat " " (List.map (fun z -> Printf.sprintf "%b/%d" (B.eq_big_int z (b 0)) (B.compare_big_int z (b 0))) [ z; z2; z3 ]));
  line "div and mod by zero" "" (guard (fun () -> s (B.div_big_int (b 1) (b 0))) ^ " " ^ guard (fun () -> s (B.mod_big_int (b 1) (b 0))));
  (* beyond native ints *)
  let big1 = B.big_int_of_string "123456789012345678901234567890123" and big2 = B.big_int_of_string "-98765432109876543210987" in
  line "big: string round trip" "" (s big1 ^ " " ^ s big2);
  line "big: (a + b) - b = a, (a * b) / b = a, a * b mod b = 0" ""
    (Printf.sprintf "%b %b %b" (B.eq_big_int (B.sub_big_int (B.add_big_int big1 big2) big2) big1)
       (B.eq_big_int (B.div_big_int (B.mult_big_int big1 big2) big2) big1)
       (B.is_zero_big_int (B.mod_big_int (B.mult_big_int big1 big2) big2)));
  line "big: a * b" "" (s (B.mult_big_int big1 big2));
  line "big: a / b, a mod b" "" (s (B.div_big_int big1 big2) ^ " " ^ s (B.mod_big_int big1 big2));
  line "div10 / mult10 of 1234567" "" (let d, q = B.div10 (b 1234567) in Printf.sprintf "%d %s %s" d (s q) (s (B.mult10 (b 1234567))));
  line "to_int32 (of_int32 x) for 0, 1, -1, max_int" "" (String.concat " " (List.map (fun x -> Int32.to_string (B.to_int32 (B.of_int32 x))) [ 0l; 1l; -1l; Int32.max_int ]))
  (* of_int32 Int32.min_int does not terminate: Int32.neg min_int is
     min_int, and an arithmetic shift of it never reaches 0. Not run here,
     since it would hang the test. *)
;;
(* Lm_num computes small operands with native / and mod and hands big ones
   to Lm_big_int, so the two paths should agree. *)
let () =
  let module N = Lm_num in
  let n = N.num_of_string and s = N.string_of_num in
  line "Lm_num: (-17) mod (-10), small path" "" (s (N.mod_num (n "-17") (n "-10")));
  line "Lm_num: (-100000000000000000000017) mod (-10), big path" "" (s (N.mod_num (n "-100000000000000000000017") (n "-10")));
  line "Lm_num: (40000 * 3) mod (-7), a product past 2^15 is big" "" (s (N.mod_num (N.mult_num (n "40000") (n "3")) (n "-7")) ^ " (native " ^ string_of_int ((40000 * 3) mod (-7)) ^ ")");
  line "Lm_num: (-100000000000000000000) * 0, equal to 0" "" (string_of_bool (N.eq_num (N.mult_num (n "-100000000000000000000") (n "0")) (n "0")))
