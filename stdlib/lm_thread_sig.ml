(*
 * Our personal implementation of threads.
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
module type MutexSig =
sig
   type t

   val create      : unit -> t
   val lock        : t -> unit
   val try_lock    : t -> bool
   val unlock      : t -> unit
end

module type ConditionSig =
sig
   type t
   type mutex

   val create    : unit -> t
   val wait      : t -> mutex -> unit
   val signal    : t -> unit
   val broadcast : t -> unit
end

(*
 * The "state" provides thread-local storage with
 * read and write locks.  By default, all threads
 * share the same state.  All elements that are
 * added are available in all threads, but
 * each thread may have a different value for
 * the element.
 *
 * The read and write functions are locked
 * using a fair readers-writers protocol.
 *)
module type StateSig =
sig
   type t
   type 'a entry

   (* State operations *)
   val create     : unit -> t
   val current    : unit -> t
   val set        : t -> unit
   val with_state : t -> ('a -> 'b) -> 'a -> 'b

   (* Global variables *)
   val shared_val  : string -> 'a -> 'a entry
   val private_val : string -> 'a -> ('a -> 'a) -> 'a entry
   val read        : 'a entry -> ('a -> 'b) -> 'b
   val write       : 'a entry -> ('a -> 'b) -> 'b
   val unlock      : 'a entry -> (unit -> 'b) -> 'b

   (* This function is only valid within a lock *)
   val get         : 'a entry -> 'a
end

module type ThreadSig =
sig
   type t
   type id

   val enabled : bool
   val create : ('a -> 'b) -> 'a -> t
   val self : unit -> t
   val id : t -> int
end

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
