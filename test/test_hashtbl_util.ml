(* Lm_hashtbl_util against a model, from a fixed LCG: tables built by
   Hashtbl.add, so a key can carry shadowed bindings. The model of a table
   is the list of its adds, newest first; a key's current value is the
   newest. Checked: to_list gives each key once with its current value;
   add_list adds in list order; add_hashtbl dst src gives dst each of
   src's keys at src's current value and leaves dst's other keys alone. *)
open Pin
module U = Lm_hashtbl_util

let seed = ref 103
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n
let build () =
  let t = Hashtbl.create 8 and m = ref [] in
  for _ = 1 to rnd 10 do let k = rnd 5 and v = rnd 100 in Hashtbl.add t k v; m := (k, v) :: !m done;
  t, !m
let current m = List.sort_uniq compare (List.filter_map (fun (k, _) -> Some (k, List.assoc k m)) m)

let () =
  let n = 2000 in
  let bad_list = ref 0 and bad_addl = ref 0 and bad_addh = ref 0 and first = ref "" in
  for _ = 1 to n do
    let t, m = build () in
    if List.sort compare (U.to_list t) <> current m then incr bad_list;
    let t2, m2 = build () and extra = List.init (rnd 4) (fun _ -> (rnd 5, rnd 100)) in
    U.add_list t2 extra;
    let m2' = List.rev_append extra m2 in
    if List.for_all (fun (k, _) -> Hashtbl.find t2 k = List.assoc k m2') m2' |> not then incr bad_addl;
    let dst, md = build () and src, ms = build () in
    U.add_hashtbl dst src;
    let ok = List.for_all (fun (k, v) -> Hashtbl.find dst k = v) (current ms)
             && List.for_all (fun (k, v) -> List.mem_assoc k ms || Hashtbl.find dst k = v) (current md) in
    if not ok then begin
      incr bad_addh;
      if !first = "" then first := Printf.sprintf "src adds %s" (list (fun (k, v) -> Printf.sprintf "%d:%d" k v) (List.rev ms))
    end
  done;
  line "to_list: tables where it is not each key's current value" (string_of_int n ^ " tables") (string_of_int !bad_list);
  line "add_list: tables where a key's value is not the last added" "" (string_of_int !bad_addl);
  line "add_hashtbl: pairs where dst is wrong" "" (Printf.sprintf "%d%s" !bad_addh (if !bad_addh = 0 then "" else " (first: " ^ !first ^ ")"));
  let src = Hashtbl.create 4 and dst = Hashtbl.create 4 in
  Hashtbl.add src 1 "old"; Hashtbl.add src 1 "new";
  U.add_hashtbl dst src;
  line "src binds 1 to old, then new; add_hashtbl: dst's 1" "" (str (Hashtbl.find dst 1))
