open Pin
module N = Lm_num

let s = N.string_of_num
let n = N.num_of_string

let () =
  List.iter (fun x -> line "num_of_string∘string_of_num" (str x) (guard (fun () -> s (n x))))
    [ "0"; "42"; "-7"; "123456789012345678901234567890"; "-98765432109876543210"; "007"; ""; "1e3"; "x" ];
  let big = n "123456789012345678901234567890" and k = n "987654321" in
  line "add" "big k" (s (N.add_num big k));
  line "sub" "k big" (s (N.sub_num k big));
  line "mult" "big big" (s (N.mult_num big big));
  line "div" "big k" (guard (fun () -> s (N.div_num big k)));
  line "quo" "big k" (s (N.quo_num big k));
  line "rem" "big k" (s (N.rem_num big k));
  line "mod" "(-17) 5" (s (N.mod_num (n "-17") (n "5")));
  line "quo" "(-17) 5" (s (N.quo_num (n "-17") (n "5")));
  line "rem" "(-17) 5" (s (N.rem_num (n "-17") (n "5")));
  line "power" "2 100" (s (N.power_num (n "2") (n "100")));
  line "power" "(-3) 3" (s (N.power_num (n "-3") (n "3")));
  line "div by zero" "1 0" (guard (fun () -> s (N.div_num (n "1") (n "0"))));
  line "compare sign" "big k" (string_of_int (compare (N.compare_num big k) 0));
  line "is_integer_num" "big" (string_of_bool (N.is_integer_num big));
  line "int_of_num" "big" (guard (fun () -> string_of_int (N.int_of_num big)));
  line "int_of_num" "-7" (string_of_int (N.int_of_num (n "-7")));
  line "to_int32" "2^31" (guard (fun () -> Int32.to_string (N.to_int32 (N.power_num (n "2") (n "31")))));
  line "succ/pred" "0" (s (N.succ_num (n "0")) ^ " " ^ s (N.pred_num (n "0")));
  line "abs/neg" "-5" (s (N.abs_num (n "-5")) ^ " " ^ s (N.neg_num (n "-5")))
