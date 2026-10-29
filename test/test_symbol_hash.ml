(* Lm_symbol_hash (the hash-consed symbols) against Lm_symbol, which
   test_symbol pins, from a fixed LCG: names over letters, digits and
   underscores, so the trailing-number parsing runs. Per operation, the
   number of names where the two disagree. Also Lm_int_util.log2 against
   a floor log2. *)
open Pin
module Y = Lm_symbol
module H = Lm_symbol_hash

let seed = ref 97
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n
let name () = String.init (1 + rnd 5) (fun _ -> "ab_019".[rnd 6])

let () =
  let n = 3000 in
  let bad = Hashtbl.create 8 and first = Hashtbl.create 8 and order = ref [] in
  let check op ok d =
    if not (List.mem op !order) then order := op :: !order;
    if not ok then begin
      Hashtbl.replace bad op (1 + try Hashtbl.find bad op with Not_found -> 0);
      if not (Hashtbl.mem first op) then Hashtbl.replace first op d
    end in
  let r f = try f () with e -> "EXN " ^ Printexc.to_string e in
  for _ = 1 to n do
    let s = name () and s2 = name () and i = rnd 30 in
    let d = str s ^ " " ^ str s2 ^ " " ^ string_of_int i in
    check "to_string (add s)" (r (fun () -> Y.to_string (Y.add s)) = r (fun () -> H.to_string (H.add s))) d;
    check "string_of_symbol (add s)" (r (fun () -> Y.string_of_symbol (Y.add s)) = r (fun () -> H.string_of_symbol (H.add s))) d;
    check "string_of_symbol (make s i)" (r (fun () -> Y.string_of_symbol (Y.make s i)) = r (fun () -> H.string_of_symbol (H.make s i))) d;
    check "is_numeric_symbol (make s i)" (r (fun () -> string_of_bool (Y.is_numeric_symbol (Y.make s i))) = r (fun () -> string_of_bool (H.is_numeric_symbol (H.make s i)))) d;
    check "to_int (make s i)" (r (fun () -> string_of_int (Y.to_int (Y.make s i))) = r (fun () -> string_of_int (H.to_int (H.make s i)))) d;
    check "eq (add s) (add s2)" (Y.eq (Y.add s) (Y.add s2) = H.eq (H.add s) (H.add s2)) d;
    check "compare = 0 exactly when eq" (let a = H.add s and b = H.add s2 in (H.compare a b = 0) = H.eq a b && compare (H.compare a b) 0 = - compare (H.compare b a) 0) d;
    let taken = [ s; s ^ "_1"; s ^ "_2" ] in
    check "new_name avoiding s, s_1, s_2"
      (r (fun () -> Y.string_of_symbol (Y.new_name (Y.add s) (fun v -> List.mem (Y.string_of_symbol v) taken)))
       = r (fun () -> H.string_of_symbol (H.new_name (H.add s) (fun v -> List.mem (H.string_of_symbol v) taken)))) d;
    let x = H.add s in
    let m : H.symbol = Marshal.from_string (Marshal.to_string x []) 0 in
    check "marshalled, then reintern: eq, same name" (H.eq (H.reintern m) x && H.string_of_symbol m = H.string_of_symbol x) d
  done;
  List.iter (fun op ->
      let k = try Hashtbl.find bad op with Not_found -> 0 in
      line ("names where " ^ op ^ " disagrees") (string_of_int n ^ " names")
        (Printf.sprintf "%d%s" k (if k = 0 then "" else " (first: " ^ Hashtbl.find first op ^ ")"))) (List.rev !order);
  line "to_string empty_var" "" (str (H.to_string H.empty_var) ^ " " ^ str (Y.to_string Y.empty_var));
  (* log2 against a floor log2 *)
  let rec flog2 i = if i <= 1 then 0 else 1 + flog2 (i lsr 1) in
  let bad = ref 0 and first = ref "" in
  List.iter (fun i -> if Lm_int_util.log2 i <> flog2 i then begin incr bad; if !first = "" then first := string_of_int i end)
    (List.concat_map (fun k -> let p = 1 lsl k in [ p - 1; p; p + 1 ]) (List.init 60 (fun k -> k + 1)) @ [ (1 lsl 61) - 1 ]);
  line "Lm_int_util.log2: values (2^k - 1, 2^k, 2^k + 1, k = 1..60; 2^61 - 1) that differ from floor log2" "" (Printf.sprintf "%d%s" !bad (if !bad = 0 then "" else " (first: " ^ !first ^ ")"));
  line "Lm_int_util.log2 0, 1, -5" "" (String.concat " " (List.map (fun i -> guard (fun () -> string_of_int (Lm_int_util.log2 i))) [ 0; 1; -5 ]));
  line "Lm_int_util.log2 (1 lsl (int_size - 2)), max_int, against floor log2" ""
    (String.concat " " (List.map (fun i -> string_of_bool (Lm_int_util.log2 i = flog2 i)) [ 1 lsl (Sys.int_size - 2); (1 lsl (Sys.int_size - 2)) + 1; max_int ]))
