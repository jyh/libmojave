(*
 * Override Pervasives IO with Format IO
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000-2005 Jason Hickey, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * jyh@cs.caltech.edu
 *)
include Format

(*
 * For now, just use normal output channels.
 *)
type out_channel = formatter

(*
 * Standard channels.
 *)
let stdout = std_formatter
let stderr = err_formatter
let stdstr = str_formatter

(*
 * Get the string from the string formatter.
 *)
let flush_stdstr = flush_str_formatter

(*
 * Open new output channels.
 *)
let open_out name =
   formatter_of_out_channel (open_out name)

let open_out_bin name =
   formatter_of_out_channel (open_out_bin name)

(*
 * Output.
 *)
let output_char       = pp_print_char
let output_string     = pp_print_string

(*
 * Normal printing.
 *)
let print_char    = pp_print_char std_formatter
let print_int     = pp_print_int std_formatter
let print_string  = pp_print_string std_formatter

let prerr_char    = pp_print_char err_formatter
let prerr_int     = pp_print_int err_formatter
let prerr_string  = pp_print_string err_formatter

(*
 * Print a newline and flush.
 *)
let flush buf  = pp_print_flush buf ()
let eflush buf = pp_print_newline buf ()

(*
 * Printing functions.
 *)
let printf  = printf
let eprintf = eprintf
let sprintf = sprintf
let fprintf = fprintf
let bprintf = bprintf

(*
 * Formatting functions.
 *)
let set_all_formatter_output_functions out flush newline spaces =
   set_all_formatter_output_functions ~out ~flush ~newline ~spaces

let pp_set_all_formatter_output_functions buf out flush newline spaces =
   pp_set_all_formatter_output_functions buf ~out ~flush ~newline ~spaces

(*
 * List separated by semicolons.
 *)
let rec print_any_list print out l =
   match l with
      [h] ->
         print out h
    | h::t ->
         print out h;
         output_string out "; ";
         print_any_list print out t
    | [] ->
         ()

let print_string_list =
   print_any_list pp_print_string

let print_int_list =
   print_any_list pp_print_int

(*
 * Get a formatter.
 *)
let out_channel_of_formatter out =
   out

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
