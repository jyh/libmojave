(* Lm_symbol by property, on symbols make s i from a fixed LCG (s over
   a b x 0 1 9 _, up to 6 characters; i from 0 to 120, often 0):
   - add inverts string_of_symbol (how metaprl prints a variable and
     reads it back), and string_of_ext_symbol where it does not quote.
     Not for an all-digit name with a leading zero and number 0
     (make "010" 0): add never makes one (it reads "010" as ("0", 10)),
     and it prints as that string; a pin line shows it;
   - compare_pair and compare_triple are compare, lexicographically;
   - new_symbol, new_symbol_string and new_symbol_pre give a symbol whose
     number no other symbol made so far has, with the same name;
   - new_name_gen v f returns f's answer on the first of make v 0,
     make v 1, ... for which f answers;
   - reintern is add of the name; is_interned is "number 0".
   Per check: how many cases differ, and the first. Then pins of the
   printers. The sets of numbers handed out depend on the counter, so
   the pins print no fresh number. *)
open Pin
module Y = Lm_symbol

let seed = ref 83
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 12) mod n
let name () =
  let a = "abx019_" in
  String.init (1 + rnd 6) (fun _ -> a.[rnd (String.length a)])
let leading_zero_numeral s = s.[0] = '0' && String.for_all (fun c -> c >= '0' && c <= '9') s
let rec sym () =
  let s = name () and i = if rnd 3 = 0 then 0 else rnd 121 in
  if i = 0 && leading_zero_numeral s then sym () else Y.make s i
let show v = Printf.sprintf "(%d, %S)" (Y.to_int v) (Y.to_string v)

let check name n case =
  seed := 83;
  let bad = ref 0 and first = ref "" in
  for _ = 1 to n do
    match case () with
    | true, _ -> ()
    | false, d -> incr bad; if !first = "" then first := d ()
  done;
  line (name ^ ": cases that differ") (string_of_int n ^ " cases")
    (Printf.sprintf "%d%s" !bad (if !bad = 0 then "" else " (first: " ^ !first ^ ")"))

let lex cmp l1 l2 =
  let rec go = function
    | x :: xs, y :: ys -> let c = cmp x y in if c <> 0 then c else go (xs, ys)
    | _ -> 0 in
  go (l1, l2)
let sign c = compare c 0

let () =
  check "add (string_of_symbol v) = v" 3000 (fun () ->
      let v = sym () in
      let s = Y.string_of_symbol v in
      let back = Y.add s in
      Y.eq back v, (fun () -> Printf.sprintf "%s -> %S -> %s" (show v) s (show back)));
  check "add (string_of_ext_symbol v) = v (when not quoted)" 3000 (fun () ->
      let v = sym () in
      let s = Y.string_of_ext_symbol v in
      if String.length s > 0 && s.[0] = '`' then true, (fun () -> "")
      else
        let back = Y.add s in
        Y.eq back v, (fun () -> Printf.sprintf "%s -> %S -> %s" (show v) s (show back)));
  check "compare_pair, compare_triple: lexicographic compare" 3000 (fun () ->
      let a = sym () and b = sym () and c = sym () and a' = (if rnd 2 = 0 then sym () else Y.make "a" 0) in
      let b' = if rnd 2 = 0 then b else sym () and c' = sym () in
      let p = sign (Y.compare_pair (a, b) (a', b')) = sign (lex Y.compare [ a; b ] [ a'; b' ]) in
      let t = sign (Y.compare_triple (a, b, c) (a, b', c')) = sign (lex Y.compare [ a; b; c ] [ a; b'; c' ]) in
      p && t, (fun () -> Printf.sprintf "%s %s %s / %s %s %s" (show a) (show b) (show c) (show a') (show b') (show c')));
  (* fresh symbols: numbers no symbol made so far has *)
  seed := 83;
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
  check "reintern = add of the name; is_interned = number 0" 3000 (fun () ->
      let v = sym () in
      Y.eq (Y.reintern v) (Y.add (Y.to_string v)) && Y.is_interned v = (Y.to_int v = 0),
      (fun () -> show v));
  (* printers *)
  line "make \"010\" 0: string_of_symbol, then add" "" (let v = Y.make "010" 0 in
      let s = Y.string_of_symbol v in str s ^ " -> " ^ show (Y.add s));
  let vs = [ Y.make "x" 0; Y.make "x" 3; Y.make "x1" 2; Y.make "a b" 0; Y.make "a b" 4 ] in
  List.iter (fun v ->
      line "string_of_symbol / string_of_ext_symbol" (show v) (str (Y.string_of_symbol v) ^ " / " ^ str (Y.string_of_ext_symbol v))) vs;
  let b = Buffer.create 16 in
  let fmt = Lm_printf.formatter_of_buffer b in
  Y.pp_print_symbol_list fmt vs; Lm_printf.pp_print_string fmt "|";
  Y.pp_print_symbol_list fmt []; Lm_printf.pp_print_string fmt "|";
  Y.pp_print_symbol_set fmt (Y.SymbolSet.of_list vs); Lm_printf.pp_print_string fmt "|";
  Y.pp_print_ext_symbol fmt (Y.make "a b" 4);
  Lm_printf.pp_print_flush fmt ();
  line "pp_print_symbol_list | [] | pp_print_symbol_set | pp_print_ext_symbol" "" (str (Buffer.contents b));
  let oc = Lm_printf.stdstr in
  ignore (Lm_printf.flush_stdstr ());
  Y.output_symbol oc (Y.make "x" 3); Lm_printf.output_string oc "|";
  Y.output_symbol_list oc vs; Lm_printf.output_string oc "|";
  Y.output_symbol_set oc (Y.SymbolSet.of_list vs);
  let s = Lm_printf.flush_stdstr () in
  line "output_symbol | output_symbol_list | output_symbol_set" "" (str s);
  let module H = Lm_symbol_hash in
  List.iter (fun (s, i) ->
      let v = H.make s i in
      line "Lm_symbol_hash: string_of_symbol / string_of_ext_symbol" (Printf.sprintf "(%d, %S)" i s)
        (str (H.string_of_symbol v) ^ " / " ^ str (H.string_of_ext_symbol v)))
    [ "x1", 2; "x", 12; "9", 66 ];
  line "new_number () increases" "" (let a = Y.new_number () in let b = Y.new_number () in string_of_bool (b > a))
