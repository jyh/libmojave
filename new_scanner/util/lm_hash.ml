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

(************************************************************************
 * Utilities.
 *
 * A generic hash module to make comparisons faster.
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
: HashSig with type elt = Arg.t =
struct
   type elt = Arg.t

   (* %%MAGICBEGIN %% *)
   type t = int * elt
   (* %%MAGICEND%% *)

   let create x =
      Arg.hash x, x

   let get (_, x) =
      x

   let hash (i, _) =
      i

   let compare ((i1 : int), x1) ((i2 : int), x2) =
      if i1 = i2 then
         Arg.compare x1 x2
      else if i1 < i2 then
         -1
      else
         1
end;;

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
  with type hash = MakeHash(Arg).t =
struct
   (* %%MAGICBEGIN%% *)
   type elt = Arg.t
   type t = int

   module Key = MakeHash (Arg);;
   module KeyTable = Lm_map.LmMake (Key);;
   type hash = Key.t

   (*
    * We need both directions.
    *)
   type state =
      { mutable key_table : int KeyTable.t;
        mutable int_table : elt array
      }
   (* %%MAGICEND%% *)

   let create_state () =
      { key_table = KeyTable.empty;
        int_table = [||]
      }

   let length state =
      KeyTable.cardinal state.key_table

   let set state i x =
      let table = state.int_table in
      let len = Array.length table in
         if len = 0 then
            let table = Array.create 32 x in
               state.int_table <- table
         else if i = len then
            let table2 = Array.create (len * 2) x in
               Array.blit table 0 table2 0 len;
               state.int_table <- table2
         else
            table.(i) <- x

   let icreate state item =
      try KeyTable.find state.key_table item with
         Not_found ->
            let index = KeyTable.cardinal state.key_table in
               state.key_table <- KeyTable.add state.key_table item index;
               set state index (Key.get item);
               index

   let create state x =
      icreate state (Key.create x)

   let get state index =
      state.int_table.(index)

   let hash index =
      index

   let compare index1 index2 =
      index1 - index2

   let map_array f state =
      Array.mapi f (Array.sub state.int_table 0 (KeyTable.cardinal state.key_table))

   let fold f x state =
      let len = KeyTable.cardinal state.key_table in
      let rec fold i x =
         if i = len then
            x
         else
            fold (succ i) (f x i)
      in
         fold 0 x
end;;

(*
 * The default function for combinding hash values.
 * XXX: JYH: we should try using a smarter hash function.
 *)
let hash_combine i1 i2 =
    (i1 lsl 2) lxor (i1 lsr 2) lxor i2

let hash_int_list code l =
   List.fold_left hash_combine code l

let hash_list hash code l =
   List.fold_left (fun code x ->
         hash_combine code (hash x)) code l

(*
 * Comparison utilities.
 *)
let rec compare_int_list (l1 : int list) (l2 : int list) =
   match l1, l2 with
      i1 :: l1, i2 :: l2 ->
         if i1 < i2 then
            -1
         else if i1 > i2 then
            1
         else
            compare_int_list l1 l2
    | [], _ ::_ ->
         -1
    | _ :: _, [] ->
         1
    | [], [] ->
         0

let rec compare_list hash l1 l2 =
   match l1, l2 with
      i1 :: l1, i2 :: l2 ->
         let i1 = hash i1 in
         let i2 = hash i2 in
            if i1 < i2 then
               -1
            else if i1 > i2 then
               1
            else
               compare_list hash l1 l2
    | [], _ ::_ ->
         -1
    | _ :: _, [] ->
         1
    | [], [] ->
         0

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
