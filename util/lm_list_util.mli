(*
 * Extra utilities for lists.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 1999 Jason Hickey, Caltech
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

(*
 * Iterate over two lists, but stop at the
 * end of the shorter one.
 *)
val short_iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit

(*
 * Iteration over three lists.
 *)
val iter3 : ('a -> 'b -> 'c -> unit) -> 'a list -> 'b list -> 'c list -> unit

(*
 * Generalize fold_left over three lists.
 *)
val fold_left3 : ('a -> 'b -> 'c -> 'd -> 'a) -> 'a -> 'b list -> 'c list -> 'd list -> 'a

(*
 * Find the index of an element in a list.
 *)
val find_index : ('a -> 'a -> bool) -> 'a -> 'a list -> int

(*
 * Split the first elements from the last.
 * raises Invalid_argument if the list is empty.
 *)
val split_last : 'a list -> 'a list * 'a
val last : 'a list -> 'a

(*
 * Split the list into two parts.
 *)
val split : int -> 'a list -> 'a list * 'a list

(*
 * Association list with an equality.
 *)
val assoc_eq : ('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> 'b

(*
 * Return the first n elements of the list.
 *)
val firstn : int -> 'a list -> 'a list

(*
 * Remove the first n elements of the list.
 *)
val nth_tl : int -> 'a list -> 'a list

(*
 * Remove an element from a list.
 *)
val remove : 'a -> 'a list -> 'a list

(*
 * Replace the nth element of the list.
 *)
val replace_nth : int -> 'a -> 'a list -> 'a list

(*
 * Apply a function to the value in
 * an association list.
 *)
val apply_assoc : 'a -> ('a * 'b) list -> ('b -> 'b) -> ('a * 'b) list

(*
 * Add a value to an association list.
 * If the key already exists, replace the value.
 *)
val add_assoc : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list

(*
 * Subtract two lists as if they were sets.
 *)
val subtract_list : 'a list -> 'a list -> 'a list

val map2to1 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val fold_filter : ('a -> 'b -> 'a * bool) -> 'a -> 'b list -> 'a * 'b list

(*
 * fold_map
 *)
val fold_map : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list
val fold_map2to1 : ('a -> 'b -> 'c -> 'a * 'd) -> 'a -> 'b list -> 'c list -> 'a * 'd list
val fold_map1to2 : ('a -> 'b -> 'a * 'c * 'd) -> 'a -> 'b list -> 'a * 'c list * 'd list

(*
 * check lengths equal
 *)
val length_eq : 'a list -> 'b list -> bool

(*
 * Set operations.
 *)
val union : 'a list -> 'a list -> 'a list

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
