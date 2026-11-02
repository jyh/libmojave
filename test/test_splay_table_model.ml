(* Lm_splay_table's method record (Lm_splay_table.create) against a model,
   Stdlib.Map from int to the data list, on random histories from a fixed
   LCG: the methods the 2026-11-02 coverage run never reached through
   MakeTable (make, elements, mem_filt, not_mem_filt, intersectp, of_list,
   cardinal, deletemax) beside the ones it did (add, replace, remove,
   find_all, union). find_all is "last added first" (lm_map_sig.ml); union
   appends with the ord_append given to create, the first table's data
   first; remove takes the first (newest) value and, with the last, the key
   (the comment at lm_splay_table.ml's remove). One line per method: how
   many steps disagree, and the first. *)
open Pin
open Lm_map_sig
module R = Map.Make (Int)

let t = Lm_splay_table.create (fun _ _ _ -> ()) compare ( @ )

let seed = ref 279
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n

let bad = Hashtbl.create 8 and first = Hashtbl.create 8 and order = ref [] and steps = Hashtbl.create 8
let check op ok d =
  if not (List.mem op !order) then order := op :: !order;
  Hashtbl.replace steps op (1 + try Hashtbl.find steps op with Not_found -> 0);
  if not ok then begin
    Hashtbl.replace bad op (1 + try Hashtbl.find bad op with Not_found -> 0);
    if not (Hashtbl.mem first op) then Hashtbl.replace first op (d ())
  end

let il l = list string_of_int l
let show_el l = list (fun (k, d) -> string_of_int k ^ ":" ^ il d) l

(* A random table, built by a random history, and its model. *)
let gen () =
  let a = ref t.empty and r = ref R.empty and hist = ref [] in
  for _ = 1 to rnd 12 do
    let k = rnd 20 and d = rnd 100 in
    match rnd 5 with
    | 0 | 1 -> a := t.add !a k d; r := R.update k (fun o -> Some (d :: Option.value o ~default:[])) !r;
      hist := Printf.sprintf "add %d %d" k d :: !hist
    | 2 -> let l = [ d; d + 1 ] in a := t.replace !a k l; r := R.add k l !r;
      hist := Printf.sprintf "replace %d [%d;%d]" k d (d + 1) :: !hist
    | 3 -> a := t.remove !a k;
      (* remove takes the first (newest) value, and the key with its last *)
      r := R.update k (function Some (_ :: (_ :: _ as tl)) -> Some tl | _ -> None) !r;
      hist := Printf.sprintf "remove %d" k :: !hist
    | _ -> let l = [ d ] in a := t.union !a (t.make k l);
      r := R.union (fun _ x y -> Some (x @ y)) !r (R.singleton k l);
      hist := Printf.sprintf "union make %d [%d]" k d :: !hist
  done;
  !a, !r, String.concat ", " (List.rev !hist)

let () =
  for _ = 1 to 600 do
    let a, ra, ha = gen () and b, rb, hb = gen () in
    let h () = ha ^ " | " ^ hb in
    check "elements (and add, replace, remove, make, union)" (t.elements a = R.bindings ra)
      (fun () -> h () ^ ": " ^ show_el (t.elements a) ^ " vs " ^ show_el (R.bindings ra));
    let k = rnd 20 in
    check "find_all" (t.find_all a k = Option.value (R.find_opt k ra) ~default:[])
      (fun () -> h () ^ Printf.sprintf " find_all %d" k);
    check "cardinal" (t.cardinal a = R.cardinal ra) (fun () -> h ());
    let ks = List.init (rnd 8) (fun _ -> rnd 20) in
    check "mem_filt" (t.mem_filt a ks = List.filter (fun k -> R.mem k ra) ks)
      (fun () -> h () ^ " mem_filt " ^ il ks ^ ": " ^ il (t.mem_filt a ks));
    check "not_mem_filt" (t.not_mem_filt a ks = List.filter (fun k -> not (R.mem k ra)) ks)
      (fun () -> h () ^ " not_mem_filt " ^ il ks ^ ": " ^ il (t.not_mem_filt a ks));
    check "intersectp" (t.intersectp a b = R.exists (fun k _ -> R.mem k rb) ra)
      (fun () -> h ());
    let u = R.union (fun _ x y -> Some (x @ y)) ra rb in
    check "union of two tables" (t.elements (t.union a b) = R.bindings u)
      (fun () -> h () ^ ": " ^ show_el (t.elements (t.union a b)) ^ " vs " ^ show_el (R.bindings u));
    check "of_list (elements a) = a" (t.elements (t.of_list (R.bindings ra)) = R.bindings ra) (fun () -> h ());
    if not (R.is_empty ra) then begin
      let k, d, rest = t.deletemax a in
      let mk, md = R.max_binding ra in
      check "deletemax" (k = mk && d = md && t.elements rest = R.bindings (R.remove mk ra)) (fun () -> h ())
    end
  done;
  List.iter (fun op ->
      let k = try Hashtbl.find bad op with Not_found -> 0 in
      line op (string_of_int (Hashtbl.find steps op) ^ " steps")
        (if k = 0 then "0 disagree" else Printf.sprintf "%d disagree (first: %s)" k (Hashtbl.find first op)))
    (List.rev !order)
