(*
 * Doubly-linked lists.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2001-2005 Jason Hickey, Caltech
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
 * Lists contain handles.
 *)
type 'a t
type 'a handle

(*
 * Creation.
 *)
val create : unit -> 'a t
val create_handle : 'a -> 'a handle
val data : 'a handle -> 'a

(*
 * List operations.
 *)
val is_empty : 'a t -> bool
val hd : 'a t -> 'a handle
val tl : 'a handle -> 'a handle
val no_tl : 'a handle -> bool
val to_list : 'a t -> 'a handle list

(*
 * Standard ops.
 *)
val length : 'a t -> int

(*
 * Iteration.
 *)
val iter : ('a handle -> unit) -> 'a t -> unit
val fold : ('a -> 'b handle -> 'a) -> 'a -> 'b t -> 'a

(*
 * Insertion/deletion.
 *)
val insert : 'a handle -> 'a t -> unit
val delete : 'a handle -> unit

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
