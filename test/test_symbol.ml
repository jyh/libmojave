open Pin
module Y = Lm_symbol

let () =
  let a = Y.add "x" and b = Y.add "x" and c = Y.add "y" in
  line "eq (add x) (add x)" "" (string_of_bool (Y.eq a b));
  line "eq (add x) (add y)" "" (string_of_bool (Y.eq a c));
  line "compare sign x y" "" (string_of_int (compare (Y.compare a c) 0));
  List.iter (fun s -> line "to_string∘add" (str s) (guard (fun () -> str (Y.to_string (Y.add s)))))
    [ "x"; "x_1"; "x12"; "_"; ""; "a b" ];
  List.iter (fun s -> line "string_of_symbol∘add" (str s) (guard (fun () -> str (Y.string_of_symbol (Y.add s)))))
    [ "x"; "x_1"; "x12" ];
  List.iter (fun (s, i) -> line "string_of_symbol∘make" (str s ^ " " ^ string_of_int i)
               (guard (fun () -> str (Y.string_of_symbol (Y.make s i)))))
    [ ("x", 0); ("x", 3); ("x1", 2) ];
  List.iter (fun (s, i) -> line "is_numeric_symbol∘make" (str s ^ " " ^ string_of_int i)
               (guard (fun () -> string_of_bool (Y.is_numeric_symbol (Y.make s i)))))
    [ ("x", 0); ("x", 3) ];
  (* new_name avoids names the predicate rejects *)
  let taken = [ "v"; "v_1"; "v_2" ] in
  line "new_name avoiding v v_1 v_2" "" (str (Y.string_of_symbol (Y.new_name (Y.add "v") (fun s -> List.mem (Y.string_of_symbol s) taken))));
  line "to_string empty_var" "" (str (Y.to_string Y.empty_var))
