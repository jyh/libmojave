(*
 * Utilities for use with the format library.
 * Copyright (C) 2002, Justin David Smith, Caltech
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
open Lm_format

(* pp_print_paragraph_bare buf text
   Prints a paragraph of text.  Normally, the text is allowed to break at
   any whitespace character.  Line breaks are forced at any newline char.
   The &nbsp; sequence can be used to insert a nonbreaking space.  This
   assumes the caller has already opened an HOV box and that the caller
   will close the box when finished.  *)
val pp_print_paragraph_bare : formatter -> string -> unit


(* pp_print_paragraph buf text
   Prints a paragraph of text in its own HOV box.  Normally, the text is
   allowed to break at any whitespace character.  Line breaks are forced
   at any newline character.  The &nbsp; sequence can be used to insert
   a nonbreaking space.  *)
val pp_print_paragraph : formatter -> string -> unit


(* y_formatter buf1 buf2
   Builds a Y formatter given two other formatters.  Data printed to the Y
   formatter will be written to *both* formatters that are given as input.
   As a result, this acts as a `Y'' which splits the output.  Pretty nice,
   eh?  *)
val y_formatter : formatter -> formatter -> formatter
