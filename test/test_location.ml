open Pin
module L = Lm_location

let sym = Lm_symbol.add
let mk f a b c d = L.create_loc (sym f) a b c d
let show l = L.string_of_location l
let dest l =
  let f, a, b, c, d = L.dest_loc l in
  Printf.sprintf "(%s, %d, %d, %d, %d)" (Lm_symbol.to_string f) a b c d

let locs =
  [ ("a.ml 1 0 1 5", mk "a.ml" 1 0 1 5);
    ("a.ml 1 0 3 2", mk "a.ml" 1 0 3 2);
    ("a.ml 2 4 2 9", mk "a.ml" 2 4 2 9);
    ("b.ml 1 0 1 5", mk "b.ml" 1 0 1 5);
    ("a.ml 0 0 0 0", mk "a.ml" 0 0 0 0);
    ("bogus \"here\"", L.bogus_loc "here") ]

let () =
  List.iter (fun (n, l) ->
      line "string_of_location" n (guard (fun () -> str (show l)));
      line "dest_loc" n (guard (fun () -> dest l));
      line "pp_print_location" n
        (guard (fun () ->
             let b = Buffer.create 16 in
             let fmt = Lm_printf.formatter_of_buffer b in
             L.pp_print_location fmt l;
             Lm_printf.pp_print_flush fmt ();
             str (Buffer.contents b))))
    locs;
  List.iter (fun (n1, l1) ->
      List.iter (fun (n2, l2) ->
          line "compare" (n1 ^ " / " ^ n2) (guard (fun () -> string_of_int (compare (L.compare l1 l2) 0)));
          line "union_loc" (n1 ^ " / " ^ n2) (guard (fun () -> str (show (L.union_loc l1 l2)))))
        locs)
    locs
