(*
 * Source file locations.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2002-2005 Jason Hickey, Caltech
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
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_printf
open Lm_symbol

type loc

(*
 * Don't use this if you can avoid it.
 *)
val bogus_loc : string -> loc

(*
 * This is the normal way to make a location.
 *    filename, start_line, start_char, end_line, end_char
 *)
val create_loc : symbol -> int -> int -> int -> int -> loc

(*
 * For marshaling.
 *)
val dest_loc : loc -> symbol * int * int * int * int

(*
 * Combine two locations.
 * The resulting span covers both.
 *)
val union_loc : loc -> loc -> loc

(*
 * Print a file location.
 *)
val pp_print_location : out_channel -> loc -> unit
val string_of_location : loc -> string

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)