(* Lm_listbuf against a list model: random sequences of every operation
   (a fixed LCG), each buffer compared by to_list after each step. *)
open Pin
module B = Lm_listbuf

let seed = ref 31
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n
let gen () = List.init (rnd 5) (fun _ -> rnd 100)

(* a random buffer with its model, built by a random history *)
let rec build depth =
  match if depth = 0 then rnd 3 else rnd 7 with
  | 0 -> B.empty, []
  | 1 -> let x = rnd 100 in B.of_elt x, [ x ]
  | 2 -> let l = gen () in B.of_list l, l
  | 3 -> let b, m = build (depth - 1) and x = rnd 100 in B.add b x, m @ [ x ]
  | 4 -> let b, m = build (depth - 1) and l = gen () in B.add_list b l, m @ l
  | 5 -> let b, m = build (depth - 1) and l = gen () in B.add_rev_list b l, m @ List.rev l
  | _ -> let b1, m1 = build (depth - 1) in let b2, m2 = build (depth - 1) in B.add_listbuf b1 b2, m1 @ m2

let () =
  let bad = ref 0 and first = ref "" in
  for _ = 1 to 5000 do
    let b, m = build 4 in
    if B.to_list b <> m then begin
      incr bad;
      if !first = "" then first := Printf.sprintf "model %s got %s" (list string_of_int m) (list string_of_int (B.to_list b))
    end
  done;
  line "buffers that disagree with the model" "5000 random histories" (Printf.sprintf "%d%s" !bad (if !bad = 0 then "" else " (first: " ^ !first ^ ")"))
