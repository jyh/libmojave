(*
 * Miscellanous printing utilities
 * Taken from code by Jason Hickey
 * 12apr01
 *)
open Lm_symbol
open Lm_printf

(*
 * Blocks indents by this many spaces.
 *)
let tabstop = 2

(*
 * Operators.
 *)

(*
 * Print a list of items with a separator
 *)
let rec print_sep_list sep printer = function
   [] ->
      ()
 | [h] ->
      printer h
 | h :: t ->
      printer h;
      Lm_format.print_string sep;
      Lm_format.print_space ();
      print_sep_list sep printer t

let rec print_sep_list_no_space sep printer = function
   [] ->
      ()
 | [h] ->
      printer h
 | h :: t ->
      printer h;
      Lm_format.print_string sep;
      print_sep_list_no_space sep printer t

let rec print_sep_list_box sep printer = function
   [] ->
      ()
 | [h] ->
      Lm_format.open_box tabstop;
      printer h;
      Lm_format.close_box ()
 | h :: t ->
      Lm_format.open_box tabstop;
      printer h;
      Lm_format.print_string sep;
      Lm_format.close_box ();
      Lm_format.print_space ();
      print_sep_list_box sep printer t

let print_fst_symbol pair =
    pp_print_symbol Lm_format.std_formatter (fst pair)
