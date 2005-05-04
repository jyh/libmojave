(*
 * Utilities on arrays.
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

(* Membership in an array *)
val mem : 'a -> 'a array -> bool
val index : 'a -> 'a array -> int
val exists : ('a -> bool) -> 'a array -> bool
val find_index : ('a -> bool) -> 'a array -> int

(* Raises Failure *)
val iter2 : ('a -> 'b -> unit) -> 'a array -> 'b array -> unit

(* Test boolean values *)
val all_true : bool array -> bool
val exists_true : bool array -> bool
val for_all : ('a -> bool) -> 'a array -> bool
val exists : ('a -> bool) -> 'a array -> bool

(*
 * replace A i j B creates a copy of array A
 * where j elements with indices from i to i + j - 1 are
 * replaced with B's elements (if B has length different from j,
 * new array will have length different from A).
 * Raises invalid_argument if numbers out of range.
 *)
val replace : 'a array -> int -> int -> 'a list -> 'a array

val append_list : 'a array -> 'a list -> 'a array
val append_list_array : 'a array -> 'a list -> 'a array -> 'a array

(*
 * Map over a sub-array.
 *)
val sub_map : ('a -> 'b) -> 'a array -> int -> int -> 'b array

(*
 * This function builds arrays out of sub-arrays.
 *)
type ('a, 'b) array_part =
   ArrayElement of 'a
 | ArrayArray of 'b * int * int

val collect : ('a, 'a array) array_part list -> 'a array

(*
 * Sorts an array, than eliminates the duplicate elements
 * and moves the remaining elements into an initial segment
 * of the input array. Returns the # of distinct elements.
 *)
val distinct: ('a -> 'a -> int) -> 'a array -> int

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)