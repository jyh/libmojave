(* Lm_large_array.LargeArray and Lm_large_array_weak.LargeWeakArray against
   a model: a table from index to value, every other index the default
   (LargeArray) or None (the weak array). Random histories of set from a
   fixed LCG, with indices clustered at 0, powers of two and their
   neighbours, where a two-level array changes size. After each set:
   did it raise, does every index below the largest one set read back the
   model's value, and is length past that index. Values are ints, so the
   weak array never loses one to the GC. *)
open Pin
module A = Lm_large_array.LargeArray
module W = Lm_large_array_weak.LargeWeakArray

let seed = ref 31
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n
let index () =
  match rnd 4 with
  | 0 -> rnd 4
  | 1 -> let p = 1 lsl rnd 12 in max 0 (p - 1 + rnd 3)
  | _ -> rnd 600

type counts = { mutable raised : int; mutable wrong : int; mutable short : int; mutable first : string }

let run name create set get length default show =
  let c = { raised = 0; wrong = 0; short = 0; first = "" } and steps = ref 0 in
  for _ = 1 to 200 do
    let a = create () and m = Hashtbl.create 16 and top = ref (-1) and hist = Buffer.create 64 in
    for _ = 1 to 15 do
      incr steps;
      let i = index () and x = rnd 1000 in
      Buffer.add_string hist (Printf.sprintf "set %d %d; " i x);
      let note () = if c.first = "" then c.first <- Buffer.contents hist in
      (match set a i x with
       | () -> Hashtbl.replace m i x; top := max !top i
       | exception _ -> c.raised <- c.raised + 1; note ());
      let ok = ref true in
      for j = 0 to !top do
        let want = try Some (Hashtbl.find m j) with Not_found -> None in
        match get a j with
        | v -> if v <> default want then ok := false
        | exception _ -> ok := false
      done;
      if not !ok then begin c.wrong <- c.wrong + 1; note () end;
      if length a <= !top then begin c.short <- c.short + 1; note () end
    done
  done;
  let n = Printf.sprintf "%d of %d" in
  line (name ^ ": sets that raise") "200 arrays x 15 sets" (n c.raised !steps);
  line (name ^ ": steps where a get disagrees with the model") "" (n c.wrong !steps);
  line (name ^ ": steps where length is not past the largest index set") "" (n c.short !steps);
  line (name ^ ": first disagreement") "" (if c.first = "" then "none" else str c.first);
  (* fixed cases *)
  let a = create () in
  line (name ^ ": length of a new array") "" (string_of_int (length a));
  line (name ^ ": set 0 on a new array") "" (guard (fun () -> set a 0 7; "returned"));
  let a = create () in
  line (name ^ ": set 5, then get 5") "" (guard (fun () -> set a 5 7; show (get a 5)));
  line (name ^ ":   length") "" (string_of_int (length a));
  line (name ^ ":   get (length)") "" (guard (fun () -> show (get a (length a))));
  line (name ^ ":   get (length + 1)") "" (guard (fun () -> show (get a (length a + 1))));
  line (name ^ ":   set 3 inside, then get 3") "" (guard (fun () -> set a 3 9; show (get a 3)))

let () =
  run "LargeArray" (fun () -> A.create (-1)) A.set A.get A.length
    (function None -> -1 | Some x -> x) string_of_int;
  run "LargeWeakArray" W.create W.set W.get W.length
    (fun v -> v) (opt string_of_int)
