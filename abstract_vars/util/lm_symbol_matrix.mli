(*
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2002 Jason Hickey, Caltech
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
open Lm_symbol

(************************************************************************
 * Types
 *)

(*
 * An adjacency matrix.
 *)
module type SymbolMatrixSetSig =
sig
   type t

   (*
    * Create a new table.
    * The int is the initial size.
    *)
   val create : int -> t

   (*
    * Test for membership.
    *)
   val query : t -> symbol -> symbol -> bool

   (*
    * Add an edge.
    *)
   val add : t -> symbol -> symbol -> unit

   (*
    * Iterate over the table.
    *)
   val iter : (symbol -> symbol -> unit) -> t -> unit
end

(*
 * An adjacency matrix.
 *)
module type SymbolMatrixTableSig =
sig
   type 'a t

   (*
    * Create a new table.
    * The int is the initial size.
    *)
   val create : int -> 'a t

   (*
    * This is an imperative data structure.
    * The copy operation builds an identical copy.
    *)
   val copy : 'a t -> 'a t

   (*
    * Test for membership.
    *)
   val find : 'a t -> symbol -> symbol -> 'a
   val query : 'a t -> symbol -> symbol -> bool

   (*
    * Add an edge.
    *)
   val add : 'a t -> symbol -> symbol -> 'a -> unit
   val filter_add : 'a t -> symbol -> symbol -> ('a option -> 'a) -> unit

   (*
    * Remove an edge.
    *)
   val remove : 'a t -> symbol -> symbol -> unit
   val filter_remove : 'a t -> symbol -> symbol -> ('a -> 'a option) -> unit

   (*
    * Iterate over the table.
    *)
   val iter : (symbol -> symbol -> 'a -> unit) -> 'a t -> unit

   (*
    * Map over the table.
    *)
   val map : ('a -> 'a) -> 'a t -> 'a t

   (*
    * Fold over the table.
    *)
   val fold : ('a -> symbol -> symbol -> 'b -> 'a) -> 'a -> 'b t -> 'a
end

(*
 * The order can be used to determine
 * define symmetric tables.
 *)
module type OrderSig =
sig
   val reorder : symbol -> symbol -> symbol * symbol
end

(*
 * This is another implementation of a sparse bit matrix.
 * This is really a hash table, but performance may be better
 * than the hashtable.
 *)
module SymbolMatrix : SymbolMatrixSetSig

(*
 * This is another implementation of a sparse bit matrix.
 * This is really a hash table, but performance may be better
 * than the hashtable.
 *)
module MakeSymbolMatrixTable (Order : OrderSig) : SymbolMatrixTableSig

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
