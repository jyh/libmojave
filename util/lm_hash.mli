(*
 * A "hash-cons" utility.
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

(*
 * The client needs to provide these functions.
 *)
module type HashArgSig =
sig
   type t

   (* For debugging *)
   val debug : string

   (* The client needs to provide hash and comparison functions *)
   val hash : t -> int
   val compare : t -> t -> int
end

(*
 * This is what we get.
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
end

(*
 * Make a hash item.
 *)
module MakeHash (Arg : HashArgSig)
: HashSig with type elt = Arg.t;;

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

(*
 * This provides hash-consing.
 *)
module MakeHashCons (Arg : HashArgSig)
: HashConsSig
  with type elt = Arg.t
  with type hash = MakeHash(Arg).t;;

(*
 * The default function for combinding hash values.
 * XXX: JYH: we should try using a smarter hash function.
 *)
val hash_combine : int -> int -> int
val hash_int_list : int -> int list -> int
val hash_list : ('a -> int) -> int -> 'a list -> int

(*
 * Comparison utilities.
 *)
val compare_int_list : int list -> int list -> int
val compare_list : ('a -> int) -> 'a list -> 'a list -> int

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
