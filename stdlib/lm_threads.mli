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

   val create : unit -> t
   val lock : t -> unit
   val unlock : t -> unit
end

module Mutex : MutexSig

module type ConditionSig =
sig
   type t

   val create : unit -> t
   val wait : t -> Mutex.t -> unit
   val signal : t -> unit
end

module Condition : ConditionSig

module type ThreadSig =
sig
   type t
   type id
   val create : ('a -> 'b) -> 'a -> t
   val self : unit -> t
   val id : t -> int
end

module Thread : ThreadSig

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
