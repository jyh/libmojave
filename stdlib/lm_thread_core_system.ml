(*
 * Our personal implementation of threads.  Each thread has
 * thread-local state.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Mojave Group, Caltech
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 * 
 * Additional permission is given to link this library with the
 * OpenSSL project's "OpenSSL" library, and with the OCaml runtime,
 * and you may distribute the linked executables.  See the file
 * LICENSE.libmojave for more details.
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

   let create    = Condition.create
   let wait      = Condition.wait
   let signal    = Condition.signal
   let broadcast = Condition.broadcast
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
   let sigmask =
      if Sys.os_type = "Win32" then
         (fun _ mask -> mask)
      else
         Thread.sigmask
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
