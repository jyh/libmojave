(*
 * A trace is like a nested list.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2001 Jason Hickey, Caltech
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
open Lm_format

type 'a trace = 'a trace_node list

and 'a trace_node =
   Elem of 'a
 | Lm_trace of 'a * 'a trace

(*
 * Convert to a list.
 *)
val to_list : 'a trace -> 'a list
val of_list : 'a list -> 'a trace

(*
 * Usual map, fold functions.
 *)
val map : ('a -> 'b) -> 'a trace -> 'b trace
val map_depth : (int -> 'a -> 'b) -> 'a trace -> 'b trace
val fold : ('a -> 'b -> 'a) -> 'a -> 'b trace -> 'a
val iter : ('a -> unit) -> 'a trace -> unit
val iter_depth : (int -> 'a -> unit) -> 'a trace -> unit
val fold_map : ('a -> 'b -> 'a * 'c) -> 'a -> 'b trace -> 'a * 'c trace
val header_nodes : 'a trace -> 'a list
val special_nodes : 'a trace -> 'a list

(*
 * Printing.
 *)
val pp_print : formatter -> (formatter -> 'a -> unit) -> 'a trace -> unit

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
