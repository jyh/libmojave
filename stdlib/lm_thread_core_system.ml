(*
 * Our personal implementation of threads.  Each thread has
 * thread-local state.
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
module MutexCore =
struct
   type t = Mutex.t

   let create   = Mutex.create
   let lock     = Mutex.lock
   let try_lock = Mutex.try_lock
   let unlock   = Mutex.unlock
end

module ConditionCore =
struct
   type t     = Condition.t
   type mutex = Mutex.t

   let create = Condition.create
   let wait   = Condition.wait
   let signal = Condition.signal
end

(*
 * Thread implementation.
 *)
module ThreadCore =
struct
   type t = Thread.t
   type id = int

   let enabled = true
   let create = Thread.create
   let self = Thread.self
   let id = Thread.id
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
