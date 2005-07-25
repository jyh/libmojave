(*
 * Various hash functions and hash-cons tables.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005 Mojave Group, Caltech
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

(************************************************************************
 * A basic table for adding a hash code to every element.
 * Nothing else is done, so comparisons are still slow.
 * This table is safe to marshal.
 *)
module type HashArgSig =
sig
   type t

   (* For debugging *)
   val debug : string

   (* The client needs to provide hash and comparison functions *)
   val hash : t -> int
   val compare : t -> t -> int
end;;

(*
 * A basic hashtbale.
 *)
module type HashSig =
sig
   type elt
   type t

   (* Creation *)
   val create : elt -> t
   val get : t -> elt

   (* Hash code *)
   val hash : t -> int

   (* Comparison *)
   val compare : t -> t -> int
end;;

module MakeHash (Arg : HashArgSig)
: HashSig with type elt = Arg.t;;

(************************************************************************
 * Table-based hash-consing.
 * Items are represented by their indexes into a table.
 *
 * This is the fastest implementation, but it is not safe to marshal
 * unless you also marshal the table.
 *
 * If you need a version that is safe to marshal, consider using the
 * HashMarshal below.  It is only slightly slower.
 *)
module type HashConsSig =
sig
   type hash
   type state
   type elt
   type t

   (* States *)
   val create_state : unit -> state
   val length : state -> int

   (* Normal creation *)
   val icreate : state -> hash -> t
   val create : state -> elt -> t
   val get : state -> t -> elt

   (* Hash code *)
   val hash : t -> int

   (* Comparison *)
   val compare : t -> t -> int

   (* Map over an array of hash codes *)
   val map_array : (t -> elt -> 'a) -> state -> 'a array

   (* Fold over all of the items *)
   val fold : ('a -> t -> 'a) -> 'a -> state -> 'a
end

module MakeHashCons (Arg : HashArgSig)
: HashConsSig
  with type elt = Arg.t
  with type hash = MakeHash(Arg).t;;

(************************************************************************
 * Marshalable version.
 *
 * This takes a slightly different approach, wrapping the value in
 * a triple of a hash code and a dummy ref cell.  During marshaling,
 * the cell will point somewhere else, so we know that the value
 * must be reinterned.  The hash codes are preseved across
 * marshaling.
 *)

(*
 * The client needs to provide these functions.
 *)
module type HashMarshalArgSig =
sig
   type t

   (* For debugging *)
   val debug : string

   (* The client needs to provide hash and comparison functions *)
   val hash : t -> int
   val compare : t -> t -> int
   val reintern : t -> t
end;;

(*
 * This is what we get.
 *
 * BUG: we break abstraction here a little because
 * it is hard to define the type recursively otherwise.
 *)
type 'a hash_marshal_item

module type HashMarshalSig =
sig
   type elt
   type t = elt hash_marshal_item

   (* Creation *)
   val create   : elt -> t

   (* The intern function fails with Not_found if the node does not already exist *)
   val intern   : elt -> t

   (* Destructors *)
   val get      : t -> elt
   val hash     : t -> int

   (* Comparison *)
   val compare  : t -> t -> int

   (* Rehash the value *)
   val reintern : t -> t
end;;

(*
 * Make a hash item.
 *)
module MakeHashMarshal (Arg : HashMarshalArgSig)
: HashMarshalSig with type elt = Arg.t;;

val pp_print_hash_stats : formatter -> unit

(************************************************************************
 * Better-than-usual hashes.
 *)
module type HashCodeSig =
sig
   type t

   val create     : unit -> t
   val add_int    : t -> int -> unit
   val add_float  : t -> float -> unit
   val add_string : t -> string -> unit
   val code       : t -> int
end;;

module type HashDigestSig =
sig
   type t

   val create     : unit -> t
   val add_bits   : t -> int -> unit
   val add_bool   : t -> bool -> unit
   val add_int    : t -> int -> unit
   val add_float  : t -> float -> unit
   val add_string : t -> string -> unit
   val digest     : t -> string
end;;

module HashCode : HashCodeSig;;
module HashDigest : HashDigestSig;;

(************************************************************************
 * Helper functions.
 *)
val hash_int_list : int -> int list -> int
val compare_int_list : int list -> int list -> int

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
