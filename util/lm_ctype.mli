(*
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2005 Jason Hickey, Caltech
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

(*
 * Locale queries.
 * See man pages for isdigit, isprint, isalnum, isupper and isgraph for more information.
 *)
val is_digit : char -> bool
val is_print : char -> bool
val is_alnum : char -> bool
val is_upper : char -> bool
val is_graph : char -> bool

(*
 * String operations.
 *)
val is_capitalized : string -> bool
val quote: string -> string

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
