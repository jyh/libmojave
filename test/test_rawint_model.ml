(* Lm_rawint (fixed-width integers: 8, 16, 32, 64 bits, signed or
   unsigned) against a model on Int64: a value of n bits is its low n bits,
   sign-extended when signed and zero-extended when not, which is how C
   converts. The model wraps every result that way; division and comparison
   of unsigned values use Int64.unsigned_div, unsigned_rem and
   unsigned_compare. A binary operation on two types works in the wider
   precision, and is unsigned when either operand is (the library's
   stated rule, which is C's for these widths).
   Operands come from a fixed LCG, half of them edge values (0, +-1, the
   bounds of each width and their neighbours), half random 64-bit patterns.
   Per operation: the number of cases whose Int64 value differs from the
   model's, and the first one.
   Not compared: division by zero (one line records what it does), shift
   counts outside 0 .. width-1 (C leaves them undefined), of_float outside
   +-2^62. A field is the bits off .. off+len-1, read as a len-bit number,
   sign-extended when the value is signed (what the code's mask for signed
   values aims at), with off+len within the width. *)
open Pin
module R = Lm_rawint

(* Lm_fmarshal over a string, for the round trip below *)
module IO = struct
  type t = int
  type in_channel = { s : string; mutable pos : int }
  type out_channel = Buffer.t
  let magic_of_int i = i
  let int_of_magic i = i
  let input_byte ic = let c = ic.s.[ic.pos] in ic.pos <- ic.pos + 1; Char.code c
  let input_buffer ic b off len = Bytes.blit_string ic.s ic.pos b off len; ic.pos <- ic.pos + len
  let output_byte oc i = Buffer.add_char oc (Char.chr (i land 0xff))
  let output_buffer oc s off len = Buffer.add_substring oc s off len
end
module F = Lm_fmarshal.Make (IO)

let seed = ref 59
let rnd30 () = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; !seed
let rnd n = (rnd30 () lsr 14) mod n (* high bits: the low ones have short periods *)
let rnd64 () =
  Int64.logxor (Int64.shift_left (Int64.of_int (rnd30 ())) 34)
    (Int64.logxor (Int64.shift_left (Int64.of_int (rnd30 ())) 17) (Int64.of_int (rnd30 ())))

let bits = function R.Int8 -> 8 | R.Int16 -> 16 | R.Int32 -> 32 | R.Int64 -> 64
let pname = function R.Int8 -> "Int8" | R.Int16 -> "Int16" | R.Int32 -> "Int32" | R.Int64 -> "Int64"
let precs = [| R.Int8; R.Int16; R.Int32; R.Int64 |]

(* the model: the low [bits p] bits of x, extended *)
let wrap p signed x =
  let m = 64 - bits p in
  if m = 0 then x
  else if signed then Int64.shift_right (Int64.shift_left x m) m
  else Int64.shift_right_logical (Int64.shift_left x m) m

let edges =
  let l = [ 0L; 1L; 2L; 127L; 128L; 255L; 256L; 32767L; 32768L; 65535L; 65536L;
            0x7fffffffL; 0x80000000L; 0xffffffffL; 0x100000000L; Int64.max_int ] in
  Array.of_list (List.concat_map (fun x -> [ x; Int64.neg x; Int64.pred x ]) l @ [ Int64.min_int ])
let raw () = if rnd 2 = 0 then edges.(rnd (Array.length edges)) else rnd64 ()
let ty () = precs.(rnd 4), rnd 2 = 0

(* a canonical value of type (p, s): the model's value, built with of_int64
   and checked separately on the first line *)
let operand (p, s) = let v = wrap p s (raw ()) in v, R.of_int64 p s v
let tname (p, s) = pname p ^ (if s then "s" else "u")
let show (p, s) v = Printf.sprintf "%s %Ld" (tname (p, s)) v

let n = 3000

(* run [n] cases; [case ()] returns None (skipped) or Some (ok, description) *)
let check name case =
  seed := 59;
  let bad = ref 0 and first = ref "" and tried = ref 0 in
  for _ = 1 to n do
    match case () with
    | None -> ()
    | Some (ok, desc) ->
        incr tried;
        if not ok then begin incr bad; if !first = "" then first := desc () end
  done;
  line (name ^ ": cases that differ from the model") (string_of_int !tried ^ " cases")
    (Printf.sprintf "%d%s" !bad (if !bad = 0 then "" else " (first: " ^ !first ^ ")"))

let got r = guard (fun () -> Int64.to_string (R.to_int64 r))
let agree r want = (try R.to_int64 r = want with _ -> false)
let desc_un t a r want () = Printf.sprintf "%s: got %s, model %Ld" (show t a) (got r) want
let desc_bin t1 a t2 b r want () =
  Printf.sprintf "%s, %s: got %s, model %Ld" (show t1 a) (show t2 b) (got r) want

let () =
  (* construction *)
  check "of_int64 (the cast)" (fun () ->
      let (p, s) as t = ty () and x = raw () in
      let r = R.of_int64 p s x and want = wrap p s x in
      Some (agree r want && R.precision r = p && R.signed r = s,
            fun () -> Printf.sprintf "%s of %Ld: got %s, model %Ld" (tname t) x (got r) want));
  check "of_int" (fun () ->
      let (p, s) as t = ty () and x = Int64.to_int (raw ()) in
      let r = R.of_int p s x and want = wrap p s (Int64.of_int x) in
      Some (agree r want, fun () -> Printf.sprintf "%s of %d: got %s, model %Ld" (tname t) x (got r) want));
  check "of_int32" (fun () ->
      let (p, s) as t = ty () and x = Int64.to_int32 (raw ()) in
      let r = R.of_int32 p s x and want = wrap p s (Int64.of_int32 x) in
      Some (agree r want, fun () -> Printf.sprintf "%s of %ld: got %s, model %Ld" (tname t) x (got r) want));
  check "of_string (Int64.to_string x)" (fun () ->
      let (p, s) as t = ty () and x = raw () in
      let r = (try R.of_string p s (Int64.to_string x) with _ -> R.of_int64 R.Int8 true 7L) in
      let want = wrap p s x in
      Some (agree r want, fun () -> Printf.sprintf "%s of %S: got %s, model %Ld" (tname t) (Int64.to_string x) (got r) want));
  check "of_float (|x| < 2^62)" (fun () ->
      let (p, s) as t = ty () and x = Int64.to_float (Int64.shift_right (raw ()) 2) in
      let r = R.of_float p s x and want = wrap p s (Int64.of_float x) in
      Some (agree r want, fun () -> Printf.sprintf "%s of %.17g: got %s, model %Ld" (tname t) x (got r) want));
  check "of_rawint (conversion)" (fun () ->
      let t1 = ty () and (p, s) as t2 = ty () in
      let a, ra = operand t1 in
      let r = R.of_rawint p s ra and want = wrap p s a in
      Some (agree r want, fun () -> Printf.sprintf "%s to %s: got %s, model %Ld" (show t1 a) (tname t2) (got r) want));
  line "max_int, min_int of each type, differing from the model" ""
    (let bad = List.concat_map (fun p -> List.filter_map (fun s ->
         let mx = R.to_int64 (R.max_int p s) and mn = R.to_int64 (R.min_int p s) in
         let wmx = if s then wrap p s (Int64.shift_right_logical (-1L) (65 - bits p)) else wrap p s (-1L) in
         let wmn = if s then Int64.neg (Int64.succ wmx) else 0L in
         if mx = wmx && mn = wmn then None
         else Some (Printf.sprintf "%s %Ld/%Ld" (tname (p, s)) mx mn)) [ true; false ]) (Array.to_list precs) in
     if bad = [] then "none" else String.concat ", " bad);
  (* conversions out *)
  check "to_byte, to_short (as C char, short of the same signedness)" (fun () ->
      let (_, s) as t = ty () in
      let a, ra = operand t in
      let b = R.to_byte ra and h = R.to_short ra in
      let wb = Int64.to_int (wrap R.Int8 s a) and wh = Int64.to_int (wrap R.Int16 s a) in
      Some (b = wb && h = wh, fun () -> Printf.sprintf "%s: got %d %d, model %d %d" (show t a) b h wb wh));
  check "is_zero, is_one" (fun () ->
      let t = ty () in
      let a, ra = operand t in
      Some (R.is_zero ra = (a = 0L) && R.is_one ra = (a = 1L),
            fun () -> Printf.sprintf "%s: got %b %b" (show t a) (R.is_zero ra) (R.is_one ra)));
  (* unary *)
  List.iter (fun (name, f, m) ->
      check name (fun () ->
          let (p, s) as t = ty () in
          let a, ra = operand t in
          let r = (try f ra with _ -> ra) and want = wrap p s (m s a) in
          Some (agree r want, desc_un t a r want)))
    [ "neg", R.neg, (fun _ a -> Int64.neg a);
      "succ", R.succ, (fun _ a -> Int64.succ a);
      "pred", R.pred, (fun _ a -> Int64.pred a);
      "abs (identity when unsigned)", R.abs, (fun s a -> if s then Int64.abs a else a);
      "lognot", R.lognot, (fun _ a -> Int64.lognot a) ];
  (* binary, on one type and on two *)
  let join (p1, s1) (p2, s2) = (if bits p1 >= bits p2 then p1 else p2), s1 && s2 in
  let binary name f m ~same ~skip =
    check name (fun () ->
        let t1 = ty () in
        let t2 = if same then t1 else ty () in
        let a, ra = operand t1 and b, rb = operand t2 in
        let (p, s) = join t1 t2 in
        let a' = wrap p s a and b' = wrap p s b in
        if skip b' then None
        else
          let r = (try f ra rb with _ -> R.of_int64 R.Int8 true 7L) and want = wrap p s (m s a' b') in
          Some (agree r want && R.precision r = p && R.signed r = s, desc_bin t1 a t2 b r want)) in
  let ops = [
    "add", R.add, (fun _ -> Int64.add);
    "sub", R.sub, (fun _ -> Int64.sub);
    "mul", R.mul, (fun _ -> Int64.mul);
    "logand", R.logand, (fun _ -> Int64.logand);
    "logor", R.logor, (fun _ -> Int64.logor);
    "logxor", R.logxor, (fun _ -> Int64.logxor);
  ] in
  List.iter (fun (name, f, m) -> binary (name ^ ", one type") f m ~same:true ~skip:(fun _ -> false)) ops;
  List.iter (fun (name, f, m) -> binary (name ^ ", two types") f m ~same:false ~skip:(fun _ -> false)) ops;
  let div s a b = if s then Int64.div a b else Int64.unsigned_div a b in
  let rem s a b = if s then Int64.rem a b else Int64.unsigned_rem a b in
  binary "div, one type (b <> 0)" R.div div ~same:true ~skip:(fun b -> b = 0L);
  binary "rem, one type (b <> 0)" R.rem rem ~same:true ~skip:(fun b -> b = 0L);
  binary "div, two types (b <> 0)" R.div div ~same:false ~skip:(fun b -> b = 0L);
  binary "rem, two types (b <> 0)" R.rem rem ~same:false ~skip:(fun b -> b = 0L);
  line "div, rem by zero (Int32 signed)" ""
    (let z = R.of_int R.Int32 true 0 and one = R.of_int R.Int32 true 1 in
     guard (fun () -> got (R.div one z)) ^ " " ^ guard (fun () -> got (R.rem one z)));
  (* comparison *)
  let cmp s a b = if s then Int64.compare a b else Int64.unsigned_compare a b in
  check "compare (sign), two types" (fun () ->
      let t1 = ty () and t2 = ty () in
      let a, ra = operand t1 and b, rb = operand t2 in
      let (p, s) = join t1 t2 in
      let c = compare (R.compare ra rb) 0 and want = compare (cmp s (wrap p s a) (wrap p s b)) 0 in
      Some (c = want, fun () -> Printf.sprintf "%s, %s: got %d, model %d" (show t1 a) (show t2 b) c want));
  check "min, max, one type" (fun () ->
      let (p, s) as t = ty () in
      let a, ra = operand t and b, rb = operand t in
      let wmin, wmax = if cmp s a b <= 0 then a, b else b, a in
      let mn = R.min ra rb and mx = R.max ra rb in
      Some (agree mn wmin && agree mx wmax,
            fun () -> ignore p; Printf.sprintf "%s, %s: got %s %s, model %Ld %Ld" (show t a) (show t b) (got mn) (got mx) wmin wmax));
  (* shifts, by 0 .. width-1 *)
  let shift name f m =
    check name (fun () ->
        let (p, s) as t = ty () in
        let a, ra = operand t in
        let j = rnd (bits p) in
        let r = (try f ra j with _ -> ra) and want = wrap p s (m s a j) in
        Some (agree r want, fun () -> Printf.sprintf "%s by %d: got %s, model %Ld" (show t a) j (got r) want)) in
  shift "shift_left" (fun r j -> R.shift_left r (R.of_int R.Int32 true j)) (fun _ a j -> Int64.shift_left a j);
  shift "shift_left_int" R.shift_left_int (fun _ a j -> Int64.shift_left a j);
  shift "shift_right (arithmetic when signed)" (fun r j -> R.shift_right r (R.of_int R.Int32 true j))
    (fun s a j -> if s then Int64.shift_right a j else Int64.shift_right_logical a j);
  (* fields *)
  let take s a off len =
    let x = Int64.shift_right_logical a off in
    let m = 64 - len in
    if s then Int64.shift_right (Int64.shift_left x m) m else Int64.shift_right_logical (Int64.shift_left x m) m in
  check "field off len (off + len <= width)" (fun () ->
      let (p, s) as t = ty () in
      let a, ra = operand t in
      let len = 1 + rnd (bits p) in
      let off = rnd (bits p - len + 1) in
      let r = (try R.field ra off len with _ -> ra) and want = wrap p s (take s a off len) in
      Some (agree r want, fun () -> Printf.sprintf "%s off %d len %d: got %s, model %Ld" (show t a) off len (got r) want));
  check "set_field off len (off + len <= width)" (fun () ->
      let (p, s) as t = ty () in
      let a, ra = operand t and b, rb = operand t in
      let len = 1 + rnd (bits p) in
      let off = rnd (bits p - len + 1) in
      let m = if len = 64 then -1L else Int64.pred (Int64.shift_left 1L len) in
      let want = wrap p s (Int64.logor (Int64.logand a (Int64.lognot (Int64.shift_left m off)))
                             (Int64.shift_left (Int64.logand b m) off)) in
      let r = (try R.set_field ra off len rb with _ -> ra) in
      Some (agree r want, fun () -> Printf.sprintf "%s off %d len %d := %Ld: got %s, model %Ld" (show t a) off len b (got r) want));
  (* Lm_fmarshal writes a rawint and reads it back *)
  check "Lm_fmarshal round trip" (fun () ->
      let t = ty () in
      let a, ra = operand t in
      let b = Buffer.create 16 in
      let back = (try F.marshal b (Lm_fmarshal.Lm_rawint ra);
                    (match F.unmarshal { IO.s = Buffer.contents b; pos = 0 } with
                     | Lm_fmarshal.Lm_rawint r -> Some r | _ -> None) with _ -> None) in
      let ok = match back with
        | Some r -> R.to_int64 r = a && R.precision r = fst t && R.signed r = snd t
        | None -> false in
      Some (ok, fun () -> Printf.sprintf "%s: got %s" (show t a)
                            (match back with Some r -> show (R.precision r, R.signed r) (R.to_int64 r) | None -> "nothing")));
  (* a few readable lines *)
  let v p s x = R.of_int p s x in
  let pr r = Printf.sprintf "%s %s" (tname (R.precision r, R.signed r)) (got r) in
  line "Int8s 127 + 1" "" (guard (fun () -> pr (R.add (v R.Int8 true 127) (v R.Int8 true 1))));
  line "Int8s of_int 200" "" (guard (fun () -> pr (v R.Int8 true 200)));
  line "Int8u of_string \"-1\"" "" (guard (fun () -> pr (R.of_string R.Int8 false "-1")));
  line "Int64u max / 2" "" (guard (fun () -> pr (R.div (R.max_int R.Int64 false) (v R.Int64 false 2))));
  line "compare Int64u max, Int64u max-1" "" (guard (fun () ->
      string_of_int (R.compare (R.max_int R.Int64 false) (R.pred (R.max_int R.Int64 false)))));
  line "Int8s field 0x05 off 0 len 4" "" (guard (fun () -> pr (R.field (v R.Int8 true 5) 0 4)))
