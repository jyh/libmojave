(*
 * Miscellanous printing utilities
 * Taken from code by Jason Hickey
 * 12apr01
 *)
open Format

open Lm_symbol

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
      Format.print_string sep;
      Format.print_space ();
      print_sep_list sep printer t

let rec print_sep_list_no_space sep printer = function
   [] ->
      ()
 | [h] ->
      printer h
 | h :: t ->
      printer h;
      Format.print_string sep;
      print_sep_list_no_space sep printer t

let rec print_sep_list_box sep printer = function
   [] ->
      ()
 | [h] ->
      Format.open_box tabstop;
      printer h;
      Format.close_box ()
 | h :: t ->
      Format.open_box tabstop;
      printer h;
      Format.print_string sep;
      Format.close_box ();
      Format.print_space ();
      print_sep_list_box sep printer t

let print_fst_symbol pair =
    pp_print_symbol Format.std_formatter (fst pair)
