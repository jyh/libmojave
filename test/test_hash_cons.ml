(* The rest of Lm_hash against models, from a fixed LCG.
   MakeHashCons: dense indices, the same index for equal values, get, length,
   fold and map_array. MakeHashMarshalEq over strings, fine equality = the
   string, coarse = its lowercase: get, equal / compare (coarse),
   fine_equal / fine_compare (fine), intern, and items passed through
   Marshal. compare_int_list against compare, hash_int_list on equal lists.
   Hash values themselves are not printed. *)
open Pin

let seed = ref 71
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n
let word () = String.init (1 + rnd 3) (fun _ -> "aAbB".[rnd 4])
let sign x = compare x 0

let bad = Hashtbl.create 8
let firsts = Hashtbl.create 8
let order = ref []
let check op ok desc =
  if not (List.mem op !order) then order := op :: !order;
  if not ok then begin
    Hashtbl.replace bad op (1 + try Hashtbl.find bad op with Not_found -> 0);
    if not (Hashtbl.mem firsts op) then Hashtbl.replace firsts op desc
  end
let report what n =
  List.iter (fun op ->
      let k = try Hashtbl.find bad op with Not_found -> 0 in
      line (what ^ ": steps where " ^ op ^ " disagrees with the model") (string_of_int n ^ " steps")
        (Printf.sprintf "%d of %d%s" k n (if k = 0 then "" else " (first: " ^ Hashtbl.find firsts op ^ ")")))
    (List.rev !order);
  Hashtbl.reset bad; Hashtbl.reset firsts; order := []

module SArg = struct
  type t = string
  let debug = "test"
  let hash = Hashtbl.hash
  let compare = String.compare
end

module C = Lm_hash.MakeHashCons (SArg)

let () =
  let n = ref 0 in
  for _ = 1 to 50 do
    let st = C.create_state () and m = ref [] in
    for _ = 1 to 60 do
      incr n;
      let s = word () in
      (* the model: each new value gets the next index; C.hash is the index *)
      let i = C.create st s in
      let want = match List.assoc_opt s !m with Some (j, _) -> j | None -> let j = List.length !m in m := !m @ [ (s, (j, i)) ]; j in
      let d = str s in
      check "index" (C.hash i = want && C.compare i (snd (List.assoc s !m)) = 0) d;
      check "get" (List.for_all (fun (s, (_, x)) -> C.get st x = s) !m) d;
      check "length/fold" (C.length st = List.length !m && C.fold (fun acc x -> C.hash x :: acc) [] st = List.rev_map (fun (_, (j, _)) -> j) !m) d;
      check "map_array" (Array.to_list (C.map_array (fun x s -> (C.hash x, s)) st) = List.map (fun (s, (j, _)) -> (j, s)) !m) d
    done
  done;
  report "MakeHashCons" !n

module EArg = struct
  type t = string
  let debug = "test"
  let fine_hash = Hashtbl.hash
  let fine_compare = String.compare
  let coarse_hash s = Hashtbl.hash (String.lowercase_ascii s)
  let coarse_compare a b = String.compare (String.lowercase_ascii a) (String.lowercase_ascii b)
  let reintern s = s
end

module E = Lm_hash.MakeHashMarshalEq (EArg)

let marshal (x : E.t) : E.t = Marshal.from_string (Marshal.to_string x []) 0

let () =
  let n = ref 0 and created = Hashtbl.create 64 and items = ref [] in
  for _ = 1 to 2000 do
    incr n;
    let s = word () in
    let d = str s in
    let was = Hashtbl.mem created s in
    check "intern" ((match E.intern s with x -> was && E.get x = s | exception Not_found -> not was)) d;
    Hashtbl.replace created s ();
    let x = E.create s in
    items := (s, x) :: !items;
    let x = if rnd 3 = 0 then marshal x else x in
    check "get" (E.get x = s) d;
    let s2, y = List.nth !items (rnd (List.length !items)) in
    let y = if rnd 3 = 0 then marshal y else y in
    let d2 = d ^ " vs " ^ str s2 in
    let lc = String.lowercase_ascii in
    check "equal (coarse)" (E.equal x y = (lc s = lc s2) && (E.compare x y = 0) = (lc s = lc s2)
                            && sign (E.compare x y) = - sign (E.compare y x)) d2;
    check "fine_equal" (E.fine_equal x y = (s = s2) && (E.fine_compare x y = 0) = (s = s2)
                        && sign (E.fine_compare x y) = - sign (E.fine_compare y x)) d2;
    check "hash (coarse)" (lc s <> lc s2 || E.hash x = E.hash y) d2;
    check "create returns the interned item" (E.create s == E.create s) d
  done;
  report "MakeHashMarshalEq" !n

let () =
  let n = 2000 and bad = ref 0 and badh = ref 0 in
  for _ = 1 to n do
    let l1 = List.init (rnd 4) (fun _ -> rnd 3 - 1) and l2 = List.init (rnd 4) (fun _ -> rnd 3 - 1) in
    if sign (Lm_hash.compare_int_list l1 l2) <> sign (compare l1 l2) then incr bad;
    if Lm_hash.hash_int_list 7 l1 <> Lm_hash.hash_int_list 7 (List.map (fun x -> x) l1) then incr badh
  done;
  line "compare_int_list: pairs that disagree with compare" (string_of_int n ^ " pairs") (string_of_int !bad);
  line "hash_int_list: equal lists with different codes" "" (string_of_int !badh);
  line "hash_int_list: [1; 2] and [2; 1] differ" "" (string_of_bool (Lm_hash.hash_int_list 0 [ 1; 2 ] <> Lm_hash.hash_int_list 0 [ 2; 1 ]))
