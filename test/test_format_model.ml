(* Lm_format (a Format-like API built on Lm_rformat; its interface says
   "Convert to standard format") against Stdlib.Format, on random
   sequences of box opens and closes, prints and break hints, from a
   fixed LCG, including print_newline and force_newline inside open
   boxes and closes past the last open box (Format ends the boxes at a
   newline or flush, and ignores an extra close).
   Where the lines break is Lm_rformat's own layout (its vertical and
   packing boxes are not Format's), so it is not compared. What is: the
   printed text with all white space removed must be the same, at
   margins 1000, 30 and 10, and no sequence may raise. Per check: how
   many sequences differ, and the first. Then exact output where no
   break is taken (the spaces a break prints when it is not taken), the
   setters and getters, make_formatter, flush_str_formatter, the tab
   boxes, and max_boxes with its ellipsis, by pin. *)
open Pin
module F = Lm_format

let seed = ref 103
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 12) mod n

type op = Open of int * int | Close | Str of string | Int of int | Bool of bool | Char of char
        | Space | Cut | Break of int * int | Force | Newline

let gen () =
  let depth = ref 0 in
  let ops = List.init (1 + rnd 25) (fun _ ->
      match rnd 14 with
      | 0 | 1 -> incr depth; Open (rnd 4, rnd 4)
      | 2 -> if !depth > 0 then (decr depth; Close) else Str "c"
      | 3 | 4 -> Str ([| "ab"; "word"; "x"; "longer_token"; "" |].(rnd 5))
      | 5 -> Int (rnd 2000 - 1000)
      | 6 -> Bool (rnd 2 = 0)
      | 7 -> Char "xyz".[rnd 3]
      | 8 -> Space
      | 9 -> Cut
      | 10 -> Break (rnd 3, rnd 3)
      | 11 -> Force
      | 12 -> Close
      | _ -> Newline) in
  ops @ List.init !depth (fun _ -> Close)

let show = function
  | Open (k, n) -> Printf.sprintf "open%d(%d)" k n | Close -> "close" | Str s -> Printf.sprintf "%S" s
  | Int i -> string_of_int i | Bool b -> string_of_bool b | Char c -> Printf.sprintf "%C" c
  | Space -> "space" | Cut -> "cut" | Break (a, b) -> Printf.sprintf "break(%d,%d)" a b
  | Force -> "force" | Newline -> "newline"

let run_lm margin ops =
  let b = Buffer.create 64 in
  let f = F.formatter_of_buffer b in
  F.pp_set_margin f margin;
  List.iter (function
      | Open (0, _) -> F.pp_open_hbox f () | Open (1, n) -> F.pp_open_vbox f n
      | Open (2, n) -> F.pp_open_hvbox f n | Open (_, n) -> F.pp_open_hovbox f n
      | Close -> F.pp_close_box f () | Str s -> F.pp_print_string f s | Int i -> F.pp_print_int f i
      | Bool x -> F.pp_print_bool f x | Char c -> F.pp_print_char f c | Space -> F.pp_print_space f ()
      | Cut -> F.pp_print_cut f () | Break (a, c) -> F.pp_print_break f a c
      | Force -> F.pp_force_newline f () | Newline -> F.pp_print_newline f ()) ops;
  F.pp_print_flush f ();
  Buffer.contents b

let run_std margin ops =
  let b = Buffer.create 64 in
  let f = Format.formatter_of_buffer b in
  Format.pp_set_margin f margin;
  List.iter (function
      | Open (0, _) -> Format.pp_open_hbox f () | Open (1, n) -> Format.pp_open_vbox f n
      | Open (2, n) -> Format.pp_open_hvbox f n | Open (_, n) -> Format.pp_open_hovbox f n
      | Close -> Format.pp_close_box f () | Str s -> Format.pp_print_string f s | Int i -> Format.pp_print_int f i
      | Bool x -> Format.pp_print_bool f x | Char c -> Format.pp_print_char f c | Space -> Format.pp_print_space f ()
      | Cut -> Format.pp_print_cut f () | Break (a, c) -> Format.pp_print_break f a c
      | Force -> Format.pp_force_newline f () | Newline -> Format.pp_print_newline f ()) ops;
  Format.pp_print_flush f ();
  Buffer.contents b

let text s = String.concat "" (String.split_on_char ' ' (String.concat "" (String.split_on_char '\n' s)))

let check name ~margin ~cmp =
  seed := 103;
  let bad = ref 0 and first = ref "" and n = 2000 in
  for _ = 1 to n do
    let ops = gen () in
    let a = guard (fun () -> run_lm margin ops) and b = guard (fun () -> run_std margin ops) in
    if not (cmp a b) then begin
      incr bad;
      if !first = "" then first := Printf.sprintf "[%s]: Lm_format %S, Format %S" (String.concat " " (List.map show ops)) a b
    end
  done;
  line (name ^ ": sequences that differ") (string_of_int n ^ " sequences")
    (Printf.sprintf "%d%s" !bad (if !bad = 0 then "" else " (first: " ^ !first ^ ")"))

let () =
  let same_text a b = text a = text b && not (String.length a > 4 && String.sub a 0 4 = "EXN ") in
  List.iter (fun m -> check (Printf.sprintf "margin %d: the same text, white space aside, and no exception" m)
                ~margin:m ~cmp:same_text) [ 1000; 30; 10 ];
  (* no break taken: what each break prints *)
  let exact name ops = line ("margin 1000, " ^ name) "" (str (guard (fun () -> run_lm 1000 ops)) ^ " (Format " ^ str (run_std 1000 ops) ^ ")") in
  exact "hbox [a; cut; b]" [ Open (0, 0); Str "a"; Cut; Str "b"; Close ];
  exact "hbox [a; break 3 1; b]" [ Open (0, 0); Str "a"; Break (3, 1); Str "b"; Close ];
  exact "hovbox [a; break 3 1; b]" [ Open (3, 0); Str "a"; Break (3, 1); Str "b"; Close ];
  exact "[a; break 2 5; b] (no box)" [ Str "a"; Break (2, 5); Str "b" ];
  exact "hovbox [a; newline; b] then close" [ Open (3, 1); Str "a"; Newline; Str "b"; Close ];
  exact "[a; close; b] (no box open)" [ Str "a"; Close; Str "b" ];
  (* setters and getters *)
  let b = Buffer.create 16 in
  let f = F.formatter_of_buffer b in
  line "default margin, max_indent, max_boxes, ellipsis" "" (Printf.sprintf "%d %d %d %S"
      (F.pp_get_margin f ()) (F.pp_get_max_indent f ()) (F.pp_get_max_boxes f ()) (F.pp_get_ellipsis_text f ()));
  F.pp_set_margin f 40; F.pp_set_max_indent f 20; F.pp_set_max_boxes f 2; F.pp_set_ellipsis_text f "%%";
  line "after setting 40, 20, 2, \"%%\"" "" (Printf.sprintf "%d %d %d %S"
      (F.pp_get_margin f ()) (F.pp_get_max_indent f ()) (F.pp_get_max_boxes f ()) (F.pp_get_ellipsis_text f ()));
  line "over_max_boxes with 0, 1, 2, 3 boxes open (max 2)" "" (String.concat " " (List.map (fun k ->
      let f = F.formatter_of_buffer (Buffer.create 8) in
      F.pp_set_max_boxes f 2;
      for _ = 1 to k do F.pp_open_hbox f () done;
      string_of_bool (F.pp_over_max_boxes f ())) [ 0; 1; 2; 3 ]));
  line "boxes nested past max_boxes 2 print the ellipsis" "" (guard (fun () ->
      let b = Buffer.create 16 in
      let f = F.formatter_of_buffer b in
      F.pp_set_max_boxes f 2; F.pp_set_ellipsis_text f "...";
      F.pp_print_string f "a"; F.pp_open_hbox f (); F.pp_print_string f "b"; F.pp_open_hbox f ();
      F.pp_print_string f "c"; F.pp_open_hbox f (); F.pp_print_string f "d"; F.pp_close_box f ();
      F.pp_close_box f (); F.pp_close_box f (); F.pp_print_flush f (); str (Buffer.contents b)));
  line "make_formatter: what the output functions receive" "" (guard (fun () ->
      let b = Buffer.create 16 and flushes = ref 0 in
      let f = F.make_formatter (fun s o l -> Buffer.add_substring b s o l) (fun () -> incr flushes) in
      F.fprintf f "x=%d@ y=%s@." 3 "z";
      Printf.sprintf "%S, %d flush(es)" (Buffer.contents b) !flushes));
  line "str_formatter, flush_str_formatter twice" "" (guard (fun () ->
      F.fprintf F.str_formatter "%s-%d" "a" 1;
      let a = F.flush_str_formatter () in
      let b = F.flush_str_formatter () in Printf.sprintf "%S %S" a b));
  line "tbox: set_tab, print_tab, print_tbreak" "" (guard (fun () ->
      let b = Buffer.create 16 in
      let f = F.formatter_of_buffer b in
      F.pp_open_tbox f (); F.pp_print_string f "ab"; F.pp_set_tab f (); F.pp_print_string f "cd";
      F.pp_print_tab f (); F.pp_print_string f "ef"; F.pp_print_tbreak f 1 2; F.pp_print_string f "gh";
      F.pp_close_tbox f (); F.pp_print_flush f (); str (Buffer.contents b)));
  line "print_as 2 \"abcd\"; print_float 1.5; print_if_newline" "" (guard (fun () ->
      let b = Buffer.create 16 in
      let f = F.formatter_of_buffer b in
      F.pp_print_as f 2 "abcd"; F.pp_print_float f 1.5; F.pp_print_if_newline f (); F.pp_print_string f "!";
      F.pp_print_flush f (); str (Buffer.contents b)))
