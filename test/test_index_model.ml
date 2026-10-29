(* Lm_index (a trie: each key maps to a sub-index and a datum) against a
   model trie written here, on random sequences of every adding and
   removing operation, single-level and along a key list, plain and _i.
   Keys a b c d, paths up to 3 deep, data ints, from a fixed LCG. After
   each operation the two are compared by every (path, datum) pair
   (fold_all), or by the exception both raise. Per check: how many of N
   sequences diverge, and the first step that does.
   The model's conventions, where the interface is silent:
   - filter_add f: f gets the old datum (None if absent); the key keeps
     its sub-index, a new key gets an empty one;
   - filter_remove f: f gets the datum; None removes the key, Some d keeps
     it with d. This is Lm_map.filter_remove's convention, the only one
     under which the 'a in 'a -> 'a option means anything; remove is
     filter_remove (fun _ -> None);
   - the _list forms walk the path, adding each key; the _entry forms
     need every key but the last to exist (Not_found otherwise);
   - removing a key that is not there raises Not_found, as Lm_map does,
     except in an index that has never held an entry (the library's Leaf,
     which the model keeps too), where it does nothing. *)
open Pin
module I = Lm_index.LmMake (struct type t = string let compare = compare end)

(* the model *)
type m = Leaf | Ix of (string * (m * int)) list

let rec m_fold f acc path = function
  | Leaf -> acc
  | Ix l -> List.fold_left (fun acc (k, (sub, d)) -> let p = path @ [ k ] in m_fold f (f acc p d) p sub) acc l
let m_all t = List.rev (m_fold (fun acc p d -> (p, d) :: acc) [] [] t)
let i_all t = List.rev (I.fold_all (fun acc p d -> (p, d) :: acc) [] t)

let entries = function Leaf -> [] | Ix l -> l
let m_set t k e = Ix (List.sort compare ((k, e) :: List.remove_assoc k (entries t)))
let m_find t k = List.assoc k (entries t)
let m_filter_add_i t k f = m_set t k (f (List.assoc_opt k (entries t)))
let m_filter_add t k f = m_filter_add_i t k (function None -> Leaf, f None | Some (s, d) -> s, f (Some d))
let m_filter_remove_i t k f =
  match t with
  | Leaf -> Leaf
  | Ix l ->
      (match List.assoc_opt k l with
       | None -> raise Not_found
       | Some e -> (match f e with None -> Ix (List.remove_assoc k l) | Some e' -> m_set t k e'))
let m_filter_remove t k f =
  m_filter_remove_i t k (fun (s, d) -> match f d with None -> None | Some d' -> Some (s, d'))
let rec m_entry t keys g =
  match keys with
  | [] -> t
  | [ k ] -> g t k
  | k :: rest -> let s, d = m_find t k in m_set t k (m_entry s rest g, d)
let rec m_list t keys fs =
  match keys, fs with
  | [], [] -> t
  | [ k ], [ f ] -> m_filter_add_i t k f
  | k :: ks, f :: fs -> let t = m_filter_add_i t k f in let s, d = m_find t k in m_set t k (m_list s ks fs, d)
  | _ -> invalid_arg "list"

let seed = ref 79
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 12) mod n
let key () = [| "a"; "b"; "c"; "d" |].(rnd 4)
let path () = List.init (1 + rnd 3) (fun _ -> key ())
let show_path p = String.concat "." p

(* the data functions, the same on both sides *)
let fa = function None -> 0 | Some d -> d + 1
let fr d = if d mod 2 = 0 then Some (d * 10 + 1) else None
let fai_i = function None -> I.empty, 0 | Some (s, d) -> s, d + 1
let fai_m = function None -> Leaf, 0 | Some (s, d) -> s, d + 1
let fri_i (s, d) = if d mod 2 = 0 then Some (s, d * 10 + 1) else None
let fri_m (s, d) = if d mod 2 = 0 then Some (s, d * 10 + 1) else None

(* one random operation: its name, and what it does to each side *)
let op () =
  let k = key () and p = path () and d = rnd 100 in
  let ds = List.map (fun _ -> rnd 100) p in
  let ps = show_path p in
  match rnd 18 with
  | 0 -> Printf.sprintf "add %s %d" k d, (fun t -> I.add t k d), (fun t -> m_filter_add t k (fun _ -> d))
  | 1 -> Printf.sprintf "add_i %s (empty, %d)" k d, (fun t -> I.add_i t k (I.empty, d)), (fun t -> m_filter_add_i t k (fun _ -> Leaf, d))
  | 2 -> "remove " ^ k, (fun t -> I.remove t k), (fun t -> m_filter_remove t k (fun _ -> None))
  | 3 -> Printf.sprintf "add_list %s" ps, (fun t -> I.add_list t p ds), (fun t -> m_list t p (List.map (fun d -> fun e -> (match e with None -> Leaf | Some (s, _) -> s), d) ds))
  | 4 -> Printf.sprintf "add_list_i %s" ps, (fun t -> I.add_list_i t p (List.map (fun d -> I.empty, d) ds)), (fun t -> m_list t p (List.map (fun d -> fun _ -> Leaf, d) ds))
  | 5 -> Printf.sprintf "add_entry %s %d" ps d, (fun t -> I.add_entry t p d), (fun t -> m_entry t p (fun t k -> m_filter_add t k (fun _ -> d)))
  | 6 -> Printf.sprintf "add_entry_i %s (empty, %d)" ps d, (fun t -> I.add_entry_i t p (I.empty, d)), (fun t -> m_entry t p (fun t k -> m_filter_add_i t k (fun _ -> Leaf, d)))
  | 7 -> "remove_entry " ^ ps, (fun t -> I.remove_entry t p), (fun t -> m_entry t p (fun t k -> m_filter_remove t k (fun _ -> None)))
  | 8 -> "filter_add " ^ k, (fun t -> I.filter_add t k fa), (fun t -> m_filter_add t k fa)
  | 9 -> "filter_add_i " ^ k, (fun t -> I.filter_add_i t k fai_i), (fun t -> m_filter_add_i t k fai_m)
  | 10 -> "filter_remove " ^ k, (fun t -> I.filter_remove t k fr), (fun t -> m_filter_remove t k fr)
  | 11 -> "filter_remove_i " ^ k, (fun t -> I.filter_remove_i t k fri_i), (fun t -> m_filter_remove_i t k fri_m)
  | 12 -> "filter_add_list " ^ ps, (fun t -> I.filter_add_list t p (List.map (fun _ -> fa) p)),
          (fun t -> m_list t p (List.map (fun _ -> function None -> Leaf, fa None | Some (s, d) -> s, fa (Some d)) p))
  | 13 -> "filter_add_list_i " ^ ps, (fun t -> I.filter_add_list_i t p (List.map (fun _ -> fai_i) p)), (fun t -> m_list t p (List.map (fun _ -> fai_m) p))
  | 14 -> "filter_add_entry " ^ ps, (fun t -> I.filter_add_entry t p fa), (fun t -> m_entry t p (fun t k -> m_filter_add t k fa))
  | 15 -> "filter_add_entry_i " ^ ps, (fun t -> I.filter_add_entry_i t p fai_i), (fun t -> m_entry t p (fun t k -> m_filter_add_i t k fai_m))
  | 16 -> "filter_remove_entry " ^ ps, (fun t -> I.filter_remove_entry t p fr), (fun t -> m_entry t p (fun t k -> m_filter_remove t k fr))
  | _ -> "filter_remove_entry_i " ^ ps, (fun t -> I.filter_remove_entry_i t p fri_i), (fun t -> m_entry t p (fun t k -> m_filter_remove_i t k fri_m))

let show_all l = "[" ^ String.concat "; " (List.map (fun (p, d) -> Printf.sprintf "%s=%d" (show_path p) d) l) ^ "]"

let run name only =
  seed := 79;
  let n = 400 and bad = ref 0 and first = ref "" and steps = ref 0 in
  for _ = 1 to n do
    let rec go i t m =
      if i = 30 then None
      else
        let name, f, g = op () in
        if not (only name) then go (i + 1) t m
        else begin
          incr steps;
          let rt = (try Ok (f t) with e -> Error (Printexc.to_string e)) in
          let rm = (try Ok (g m) with e -> Error (Printexc.to_string e)) in
          match rt, rm with
          | Ok t', Ok m' ->
              if i_all t' = m_all m' then go (i + 1) t' m'
              else Some (Printf.sprintf "step %d, %s: library %s, model %s" i name (show_all (i_all t')) (show_all (m_all m')))
          | Error a, Error b when a = b -> go (i + 1) t m
          | _ ->
              let s = function Ok _ -> "ok" | Error e -> "EXN " ^ e in
              Some (Printf.sprintf "step %d, %s: library %s, model %s (before: %s)" i name (s (Result.map ignore rt)) (s (Result.map ignore rm)) (show_all (m_all m)))
        end in
    match go 0 I.empty Leaf with
    | None -> ()
    | Some d -> incr bad; if !first = "" then first := d
  done;
  line (name ^ ": sequences that diverge") (Printf.sprintf "%d sequences, %d steps" n !steps)
    (Printf.sprintf "%d%s" !bad (if !bad = 0 then "" else " (first: " ^ !first ^ ")"))

let starts p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p
let () =
  run "adds only (add, add_i, add_list(_i), add_entry(_i), filter_add*)" (fun s -> not (starts "remove" s || starts "filter_remove" s));
  run "adds and remove, remove_entry" (fun s -> not (starts "filter_remove" s));
  run "every operation" (fun _ -> true);
  let t = I.add (I.add I.empty "a" 2) "b" 3 in
  let sh t = show_all (i_all t) in
  line "filter_remove a (even 2: Some 21), b (odd 3: None), on [a=2; b=3]" ""
    (guard (fun () -> sh (I.filter_remove t "a" fr)) ^ " " ^ guard (fun () -> sh (I.filter_remove t "b" fr)));
  line "remove q (absent) from [a=2; b=3]; from empty" "" (guard (fun () -> sh (I.remove t "q")) ^ " " ^ guard (fun () -> sh (I.remove I.empty "q")))
