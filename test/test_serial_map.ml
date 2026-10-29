(* Lm_serial_map against a model: an association list in insertion order
   (re-adding a key keeps its place, as the code intends). Random
   add/remove sequences from a fixed LCG; after each step every query is
   compared. remove of an absent key is counted separately. *)
open Pin
module M = Lm_serial_map.SerialMapMake (struct type t = int let compare = compare end)

let seed = ref 8
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n

let () =
  let bad = ref 0 and steps = ref 0 and absent = ref 0 and absent_raise = ref 0 and first = ref "" in
  for _ = 1 to 500 do
    let m = ref M.empty and model = ref [] in
    for _ = 1 to 30 do
      incr steps;
      let k = rnd 10 in
      if rnd 3 = 0 then begin
        if not (List.mem_assoc k !model) then begin
          incr absent;
          match M.remove !m k with
          | m' -> m := m'
          | exception Not_found -> incr absent_raise
        end else begin
          m := M.remove !m k;
          model := List.remove_assoc k !model
        end
      end else begin
        let v = rnd 1000 in
        m := M.add !m k v;
        model := if List.mem_assoc k !model then List.map (fun (k', v') -> if k' = k then k', v else k', v') !model else !model @ [ k, v ]
      end;
      let keys = List.map fst !model and data = List.map snd !model in
      let ok =
        M.keys !m = keys && M.data !m = data && M.cardinal !m = List.length keys && M.is_empty !m = (keys = [])
        && List.for_all (fun k -> M.mem !m k = List.mem_assoc k !model) (List.init 10 Fun.id)
        && List.for_all (fun (k, v) -> M.find !m k = v) !model
        && List.rev (M.fold (fun acc k v -> (k, v) :: acc) [] !m) = !model
        && (let acc = ref [] in M.iter (fun k v -> acc := (k, v) :: !acc) !m; List.rev !acc = !model) in
      if not ok then begin
        incr bad;
        if !first = "" then first := Printf.sprintf "model %s keys %s" (list string_of_int keys) (list string_of_int (M.keys !m))
      end
    done
  done;
  line "steps that disagree with the model" "500 runs x 30 steps" (Printf.sprintf "%d of %d%s" !bad !steps (if !bad = 0 then "" else " (first: " ^ !first ^ ")"));
  line "remove of an absent key raises Not_found" "" (Printf.sprintf "%d of %d" !absent_raise !absent);
  line "find of an absent key" "" (guard (fun () -> string_of_int (M.find M.empty 3)))
