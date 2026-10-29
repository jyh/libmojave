(* Lm_dlist against a model: random sequences of insert (at the head) and
   delete over 8 handles, including re-inserting deleted handles and
   deleting a handle twice, from a fixed LCG. After each step the list is
   read four ways (to_list, iter, fold, a hd/tl walk) and its length. *)
open Pin

let seed = ref 99
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n

let () =
  let runs = 1000 and steps = 30 in
  let bad = ref 0 and first = ref "" and ops = ref 0 in
  for _ = 1 to runs do
    let l = Lm_dlist.create () in
    let hs = Array.init 8 Lm_dlist.create_handle in
    let inside = Array.make 8 false in
    let model = ref [] and trace = Buffer.create 64 in
    for _ = 1 to steps do
      let k = rnd 8 in
      incr ops;
      if inside.(k) || rnd 4 = 0 then begin
        (* delete (also of a handle that is not in the list) *)
        Buffer.add_string trace (Printf.sprintf "d%d " k);
        Lm_dlist.delete hs.(k);
        inside.(k) <- false;
        model := List.filter (( <> ) k) !model
      end else begin
        Buffer.add_string trace (Printf.sprintf "i%d " k);
        Lm_dlist.insert hs.(k) l;
        inside.(k) <- true;
        model := k :: !model
      end;
      let data = List.map Lm_dlist.data in
      let via_list = data (Lm_dlist.to_list l) in
      let via_iter = let acc = ref [] in Lm_dlist.iter (fun h -> acc := Lm_dlist.data h :: !acc) l; List.rev !acc in
      let via_fold = List.rev (Lm_dlist.fold (fun acc h -> Lm_dlist.data h :: acc) [] l) in
      let via_walk =
        if Lm_dlist.is_empty l then []
        else
          let rec walk h = Lm_dlist.data h :: (if Lm_dlist.no_tl h then [] else walk (Lm_dlist.tl h)) in
          walk (Lm_dlist.hd l) in
      let ok = via_list = !model && via_iter = !model && via_fold = !model && via_walk = !model
               && Lm_dlist.length l = List.length !model in
      if not ok then begin
        incr bad;
        if !first = "" then
          first := Printf.sprintf "after %s: model %s, to_list %s, walk %s, length %d" (Buffer.contents trace)
              (list string_of_int !model) (list string_of_int via_list) (list string_of_int via_walk) (Lm_dlist.length l)
      end
    done
  done;
  line "steps that disagree with the model" (Printf.sprintf "%d runs x %d steps" runs steps)
    (Printf.sprintf "%d of %d%s" !bad !ops (if !bad = 0 then "" else " (first: " ^ !first ^ ")"));
  line "hd of an empty list" "" (guard (fun () -> string_of_int (Lm_dlist.data (Lm_dlist.hd (Lm_dlist.create ())))));
  line "tl of a last handle" "" (guard (fun () -> let l = Lm_dlist.create () in let h = Lm_dlist.create_handle 1 in Lm_dlist.insert h l; string_of_int (Lm_dlist.data (Lm_dlist.tl h))));
  line "insert a handle that is already in a list" ""
    (try let l = Lm_dlist.create () and m = Lm_dlist.create () in let h = Lm_dlist.create_handle 1 in
       Lm_dlist.insert h l; Lm_dlist.insert (Lm_dlist.create_handle 0) l; Lm_dlist.insert h m; "inserted"
     with Assert_failure _ -> "EXN Assert_failure")
