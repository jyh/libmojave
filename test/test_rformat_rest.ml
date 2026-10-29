(* The parts of Lm_rformat no other test calls: format_bound (the
   overflow comes at the insertion after the bound is reached, as its
   interface says), format_azone (an atomic zone counts as one character
   when lines are broken), format_hspace, format_depth, format_flush and
   format_flush_popm, rendered with print_text_string. *)
open Pin
module F = Lm_rformat
module T = Lm_rformat_text

let () =
  let b = F.new_buffer () in
  F.format_bound b 10;
  F.format_string b "123456789012";
  line "bound 10: a 12-character string" "" (str (T.print_text_string 80 b));
  line "bound 10: the next insertion" "" (guard (fun () -> F.format_string b "x"; "inserted"));
  let atom wide =
    let b = F.new_buffer () in
    F.format_szone b;
    F.format_string b "aaaa"; F.format_space b;
    if wide then F.format_string b "bbbbbbbb" else (F.format_azone b; F.format_string b "bbbbbbbb"; F.format_ezone b);
    F.format_space b; F.format_string b "c";
    F.format_ezone b;
    T.print_text_string 10 b in
  line "width 10: aaaa bbbbbbbb c, plain then bbbbbbbb in an atomic zone" "" (str (atom true) ^ " / " ^ str (atom false));
  let b = F.new_buffer () in
  F.format_hzone b; F.format_string b "x"; F.format_hspace b; F.format_string b "y"; F.format_ezone b;
  line "hzone with hspace" "" (str (T.print_text_string 80 b));
  let b = F.new_buffer () in
  let d0 = F.format_depth b in
  F.format_szone b; F.format_pushm b 2; F.format_hzone b;
  let d3 = F.format_depth b in
  F.format_flush b;
  let d_flush = F.format_depth b in
  F.format_flush_popm b;
  let d_popm = F.format_depth b in
  line "format_depth: empty; after szone, pushm, hzone; after format_flush; after format_flush_popm" ""
    (Printf.sprintf "%d %d %d %d" d0 d3 d_flush d_popm)
