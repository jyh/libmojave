(*
 * Sets of strings.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Mojave Group, Caltech
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
 * String sets and string tables
 *)
module OrderedString = struct
   type t = string
   let compare = Lm_string_util.string_compare
end

module StringSet = Lm_set.LmMake (OrderedString)
module StringTable = Lm_map.LmMake (OrderedString)
module StringMTable = Lm_map.LmMakeList (OrderedString)

(*
 * String sets and string tables with lexicographical ordering.
 *)
module LexOrderedString = struct
   type t = string
   let compare (s1: t) s2 = Pervasives.compare s1 s2
end

module LexStringSet = Lm_set.LmMake (LexOrderedString)
module LexStringTable = Lm_map.LmMake (LexOrderedString)
module LexStringMTable = Lm_map.LmMakeList (LexOrderedString)

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
