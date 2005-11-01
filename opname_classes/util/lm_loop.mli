(*
 * This module implements dominator calculations and
 * loop-nest trees.
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
open Lm_printf
open Lm_symbol
open Lm_trace

(*
 * State type.
 *)
type 'a t

(*
 * Lm_debug flag.
 *)
val debug_loop : bool ref

(*
 * Printing.
 *)
val pp_print_trace : formatter -> ('a -> symbol) -> 'a trace -> unit

(*
 * Calculate the loop nest tree.
 * The arguments are:
 *    node_name : 'a -> symbol
 *    node_succ : 'a -> symbol list
 *    root : 'a
 *    nodes : 'a list
 *
 * You have to provide the root node for the graph.
 * Typically, this means you should create a trivial
 * node that has edges to all loop header nodes.
 *)
val create : string -> ('a -> symbol) -> ('a -> symbol list) -> 'a -> 'a list -> 'a t
val loop_nest : 'a t -> ('a -> symbol) -> 'a trace
val dominators : 'a t -> ('a -> symbol) -> symbol SymbolTable.t

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
