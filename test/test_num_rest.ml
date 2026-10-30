(* The rest of Lm_num and Lm_big_int, by property, on numbers built from
   a fixed LCG's decimal strings (up to 25 digits, either sign, often
   near 2^30 and 2^62):
   - lt_num, le_num, ge_num, gt_num and is_zero agree with compare_num;
     compare_num agrees with comparing the decimal strings as integers;
   - integer_num and int_of_num give the int when |n| < 2^30 and raise
     otherwise, and is_integer_num says which (Lm_num's ints are 31-bit
     like its min_int and max_int);
   - Lm_big_int: is_integer_big_int is true exactly when
     integer_big_int succeeds; neg_big_int negates; of_string is
     big_int_of_string; quo_big_int and rem_big_int are div_big_int and
     mod_big_int.
   Per check: how many cases differ, and the first. *)
open Pin
module N = Lm_num
module B = Lm_big_int

let seed = ref 109
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 12) mod n
let digits () =
  match rnd 4 with
  | 0 -> string_of_int (rnd 1000)
  | 1 -> string_of_int ((1 lsl 30) - 2 + rnd 4)
  | 2 -> string_of_int ((1 lsl 62) - 2 + rnd 2)
  | _ -> let s = String.init (1 + rnd 25) (fun _ -> Char.chr (48 + rnd 10)) in
         let rec strip i = if i < String.length s - 1 && s.[i] = '0' then strip (i + 1) else i in
         String.sub s (strip 0) (String.length s - strip 0)
let dec () = (if rnd 2 = 0 then "-" else "") ^ digits ()
(* compare two decimal integer strings *)
let cmp_dec a b =
  let neg s = String.length s > 0 && s.[0] = '-' in
  let mag s = if neg s then String.sub s 1 (String.length s - 1) else s in
  let zero s = mag s = "0" in
  let cm a b = compare (String.length a, a) (String.length b, b) in
  match (neg a && not (zero a)), (neg b && not (zero b)) with
  | false, false -> cm (mag a) (mag b)
  | true, true -> cm (mag b) (mag a)
  | true, false -> -1
  | false, true -> 1

let check name n case =
  seed := 109;
  let bad = ref 0 and first = ref "" in
  for _ = 1 to n do
    match case () with
    | true, _ -> ()
    | false, d -> incr bad; if !first = "" then first := d ()
  done;
  line (name ^ ": cases that differ") (string_of_int n ^ " cases")
    (Printf.sprintf "%d%s" !bad (if !bad = 0 then "" else " (first: " ^ !first ^ ")"))
let sign c = compare c 0

let () =
  check "compare_num against the decimal strings; lt/le/ge/gt/is_zero against compare_num" 3000 (fun () ->
      let a = dec () and b = if rnd 4 = 0 then "0" else dec () in
      let x = N.num_of_string a and y = N.num_of_string b in
      let c = sign (N.compare_num x y) in
      let ok = c = sign (cmp_dec a b)
               && N.lt_num x y = (c < 0) && N.le_num x y = (c <= 0) && N.ge_num x y = (c >= 0)
               && N.gt_num x y = (c > 0) && N.eq_num x y = (c = 0) && N.is_zero y = (cmp_dec b "0" = 0) in
      ok, (fun () -> Printf.sprintf "%s %s: compare %d" a b c));
  check "integer_num, int_of_num, is_integer_num: the int when |n| < 2^30, else raise" 3000 (fun () ->
      let a = dec () in
      let x = N.num_of_string a in
      let fits = cmp_dec a (string_of_int (1 lsl 30)) < 0 && cmp_dec a (string_of_int (- (1 lsl 30))) > 0 in
      let r f = try Some (f x) with _ -> None in
      let want = if fits then Some (int_of_string a) else None in
      let ok = r N.integer_num = want && r N.int_of_num = want && N.is_integer_num x = fits in
      ok, (fun () -> Printf.sprintf "%s: integer_num %s, int_of_num %s, is_integer_num %b" a
             (opt string_of_int (r N.integer_num)) (opt string_of_int (r N.int_of_num)) (N.is_integer_num x)));
  check "Lm_big_int: is_integer_big_int iff integer_big_int succeeds; neg; of_string" 3000 (fun () ->
      let a = dec () in
      let x = B.big_int_of_string a in
      let ok = B.is_integer_big_int x = (try ignore (B.integer_big_int x); true with _ -> false)
               && B.eq_big_int (B.add_big_int x (B.neg_big_int x)) (B.big_int_of_int 0)
               && B.eq_big_int (B.of_string a) x in
      ok, (fun () -> Printf.sprintf "%s: is_integer %b" a (B.is_integer_big_int x)));
  check "Lm_big_int: quo_big_int, rem_big_int are div_big_int, mod_big_int" 1000 (fun () ->
      let x = B.big_int_of_string (dec ()) and y = B.big_int_of_string (dec ()) in
      if B.is_zero_big_int y then true, (fun () -> "")
      else
        B.eq_big_int (B.quo_big_int x y) (B.div_big_int x y) && B.eq_big_int (B.rem_big_int x y) (B.mod_big_int x y),
        (fun () -> B.string_of_big_int x ^ " " ^ B.string_of_big_int y));
  (* zero written with a minus sign *)
  let z = B.big_int_of_int 0 in
  List.iter (fun str ->
      let x = B.big_int_of_string str in
      line "Lm_big_int: big_int_of_string, then eq 0, compare 0, is_zero, string" (Pin.str str)
        (Printf.sprintf "%b %d %b %S" (B.eq_big_int x z) (B.compare_big_int x z) (B.is_zero_big_int x) (B.string_of_big_int x)))
    [ "-0"; "-000"; "0" ];
  List.iter (fun str ->
      let x = N.num_of_string str in
      line "Lm_num: num_of_string, then eq_num 0, compare_num 0, is_zero" (Pin.str str)
        (Printf.sprintf "%b %d %b" (N.eq_num x (N.num_of_int 0)) (N.compare_num x (N.num_of_int 0)) (N.is_zero x)))
    [ "-0"; "-000000000000000000000000000000" ];
  (* a leading zero before decimal digits *)
  line "Lm_big_int: big_int_of_string of 0123, 0789, -0123, 007, 0x1f, 0b101, 0o17" ""
    (String.concat " " (List.map (fun str -> guard (fun () -> B.string_of_big_int (B.big_int_of_string str)))
                          [ "0123"; "0789"; "-0123"; "007"; "0x1f"; "0b101"; "0o17" ]))
