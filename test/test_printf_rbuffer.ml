(* Lm_printf_rbuffer: print an Lm_rformat buffer as text at
   Lm_rformat.default_width (80) on an Lm_printf channel. *)
open Pin
module F = Lm_rformat

(* 30 words of 6 or 7 characters, in a box that breaks at spaces. *)
let doc () =
  let b = F.new_buffer () in
  F.format_szone b;
  F.format_pushm b 2;
  for k = 1 to 30 do
    if k > 1 then F.format_space b;
    F.format_string b ("word" ^ string_of_int k)
  done;
  F.format_popm b;
  F.format_ezone b;
  b

let () =
  line "default_width" "" (string_of_int F.default_width);
  let out = Buffer.create 256 in
  let ch = Lm_printf.formatter_of_buffer out in
  Lm_printf_rbuffer.output_rbuffer ch (doc ());
  Lm_printf.pp_print_flush ch ();
  let s = Buffer.contents out in
  line "output_rbuffer, 30 words" "" (str s);
  line "  longest line" "" (string_of_int (List.fold_left (fun m l -> max m (String.length l)) 0 (String.split_on_char '\n' s)))
