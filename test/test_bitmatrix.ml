(* Lm_bitmatrix against a bool matrix: random add/remove/query on random
   sizes, from a fixed LCG. create w h has h rows of w bits; row i,
   column j. *)
open Pin

let seed = ref 5
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n

let () =
  let bad = ref 0 and ops = ref 0 and first = ref "" in
  for _ = 1 to 300 do
    let w = 1 + rnd 40 and h = 1 + rnd 6 in
    let m = Lm_bitmatrix.create w h and model = Array.make_matrix h w false in
    for _ = 1 to 100 do
      let i = rnd h and j = rnd w in
      incr ops;
      (match rnd 3 with
       | 0 -> Lm_bitmatrix.add m i j; model.(i).(j) <- true
       | 1 -> Lm_bitmatrix.remove m i j; model.(i).(j) <- false
       | _ -> ());
      let ok = ref true in
      for i = 0 to h - 1 do for j = 0 to w - 1 do
          if Lm_bitmatrix.query m i j <> model.(i).(j) then ok := false done done;
      if not !ok then begin
        incr bad;
        if !first = "" then first := Printf.sprintf "w=%d h=%d after (%d,%d)" w h i j
      end
    done
  done;
  line "steps that disagree with the model" "300 matrices x 100 steps"
    (Printf.sprintf "%d of %d%s" !bad !ops (if !bad = 0 then "" else " (first: " ^ !first ^ ")"));
  (* columns past w but inside the last byte are accepted; past it they raise *)
  let m = Lm_bitmatrix.create 3 2 in
  line "create 3 2: add (0,5), query (0,5)" "" (guard (fun () -> Lm_bitmatrix.add m 0 5; string_of_bool (Lm_bitmatrix.query m 0 5)));
  line "create 3 2: query (0,8)" "" (guard (fun () -> string_of_bool (Lm_bitmatrix.query m 0 8)));
  line "create 3 2: query (2,0)" "" (guard (fun () -> string_of_bool (Lm_bitmatrix.query m 2 0)))
