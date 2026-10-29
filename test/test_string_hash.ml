(* Lm_string_hash (Lm_hash.MakeHashMarshal over strings) against a model:
   strings themselves. Random strings from a fixed LCG over a small
   alphabet, plus pairs of distinct strings with the SAME Hashtbl.hash
   (found by search), so the collision path of compare is exercised.
   Items are also passed through Marshal, which leaves them stale until
   their first use. Checked: get, intern (Not_found exactly when never
   created), equal and compare against string equality, compare as a total
   order, and StringHashSet / StringHashTable against a model set and map.
   Hash values themselves are not printed. *)
open Pin
module H = Lm_string_hash.StringHash
module S = Lm_string_hash.StringHashSet
module T = Lm_string_hash.StringHashTable

let seed = ref 53
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n
let word () = String.init (1 + rnd 3) (fun _ -> "abc".[rnd 3])
let sign x = compare x 0
let marshal (x : H.t) : H.t = Marshal.from_string (Marshal.to_string x []) 0

(* Distinct strings with equal Hashtbl.hash: search the 4-letter words. *)
let collisions =
  let seen = Hashtbl.create 100000 and found = ref [] in
  let n = 26 * 26 * 26 * 26 in
  (try
     for i = 0 to n - 1 do
       let s = String.init 4 (fun j -> Char.chr (97 + (i / int_of_float (26. ** float j)) mod 26)) in
       let h = Hashtbl.hash s in
       (match Hashtbl.find_opt seen h with
        | Some s' -> found := (s', s) :: !found; if List.length !found >= 8 then raise Exit
        | None -> ());
       Hashtbl.replace seen h s
     done
   with Exit -> ());
  List.rev !found

let () =
  line "collision pairs found among 4-letter words" "" (string_of_int (List.length collisions));
  let bad = Hashtbl.create 8 and first = Hashtbl.create 8 and n = ref 0 in
  let check op ok desc =
    if not ok then begin
      Hashtbl.replace bad op (1 + try Hashtbl.find bad op with Not_found -> 0);
      if not (Hashtbl.mem first op) then Hashtbl.replace first op desc
    end
  in
  let created = Hashtbl.create 64 in
  let items = ref [] in
  let mk s = Hashtbl.replace created s (); let x = H.create s in items := (s, x) :: !items; x in
  List.iter (fun (a, b) -> ignore (mk a); ignore (mk b)) collisions;
  let set = ref S.empty and mset = ref [] and table = ref T.empty and mtable = ref [] in
  for _ = 1 to 3000 do
    incr n;
    let s = if rnd 5 = 0 && collisions <> [] then (let a, b = List.nth collisions (rnd (List.length collisions)) in if rnd 2 = 0 then a else b) else word () in
    let desc = str s in
    (* intern before create *)
    let was = Hashtbl.mem created s in
    check "intern" ((match H.intern s with x -> was && H.get x = s | exception Not_found -> not was)) desc;
    let x = mk s in
    let x = if rnd 3 = 0 then marshal x else x in
    check "get" (H.get x = s) desc;
    check "intern" (H.equal (H.intern s) x && H.compare (H.intern s) x = 0) desc;
    (* against an older item, possibly also marshalled *)
    let s2, y = List.nth !items (rnd (List.length !items)) in
    let y = if rnd 3 = 0 then marshal y else y in
    let d2 = desc ^ " vs " ^ str s2 in
    check "equal" (H.equal x y = (s = s2) && H.equal y x = (s = s2)) d2;
    check "compare" ((H.compare x y = 0) = (s = s2) && sign (H.compare x y) = - sign (H.compare y x)) d2;
    (* sets and tables *)
    set := S.add !set x;
    if not (List.mem s !mset) then mset := s :: !mset;
    table := T.add !table x (String.length s);
    mtable := (s, String.length s) :: List.remove_assoc s !mtable;
    check "set" (S.cardinal !set = List.length !mset && List.for_all (fun s -> S.mem !set (H.create s)) !mset
                 && List.sort compare (List.map H.get (S.elements !set)) = List.sort compare !mset) desc;
    check "table" (List.for_all (fun (s, v) -> T.find !table (H.create s) = v) !mtable && T.cardinal !table = List.length !mtable) desc
  done;
  (* compare is a total order: sorting all items twice, from two
     orders, gives the same sequence of strings *)
  let all = List.sort_uniq compare (List.map fst !items) in
  let xs = List.map H.create all in
  let a = List.map H.get (List.sort H.compare xs) and b = List.map H.get (List.sort H.compare (List.rev_map marshal xs)) in
  let trans = ref true in
  List.iter (fun p -> List.iter (fun q -> List.iter (fun r ->
      if H.compare p q < 0 && H.compare q r < 0 && H.compare p r >= 0 then trans := false) xs) xs) (List.filteri (fun i _ -> i < 40) xs);
  check "order" (a = b && !trans) "sorting";
  List.iter (fun op ->
      let k = try Hashtbl.find bad op with Not_found -> 0 in
      line ("steps where " ^ op ^ " disagrees with the model") "3000 strings"
        (Printf.sprintf "%d of %d%s" k !n (if k = 0 then "" else " (first: " ^ Hashtbl.find first op ^ ")")))
    [ "intern"; "get"; "equal"; "compare"; "set"; "table"; "order" ];
  (* fixed cases on one collision pair *)
  match collisions with
  | (a, b) :: _ ->
      let x = H.create a and y = H.create b in
      line "a collision pair: same hash, equal, compare = 0" "" (Printf.sprintf "%b %b %b" (H.hash x = H.hash y) (H.equal x y) (H.compare x y = 0));
      line "  after marshalling one" "" (Printf.sprintf "%b %b" (H.equal (marshal x) y) (H.compare (marshal x) y = 0));
      line "  create returns the same item twice" "" (string_of_bool (H.create a == x));
      line "  reintern of a marshalled item returns the interned one" "" (string_of_bool (H.reintern (marshal x) == x))
  | [] -> ()
