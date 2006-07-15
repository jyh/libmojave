(*
 * An null implementation of threads.
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

(*
 * Locks are not required when not using threads.
 *)
module MutexCore =
struct
   type t = unit

   let create () =
      ()

   let lock () =
      ()

   let try_lock () =
      false

   let unlock () =
      ()
end

(*
 * Conditions are not required when not using threads.
 *)
module ConditionCore =
struct
   type t = unit
   type mutex = MutexCore.t

   let create () =
      ()

   let wait _ _ =
      ()

   let signal () =
      ()

   let broadcast () =
      ()
end

(*
 * Threads are null.  The create function doesn't work without
 * threads, so raise an exception.
 *)
module ThreadCore =
struct
   type t = unit
   type id = unit

   let enabled = false

   let create f x =
      raise (Invalid_argument "Lm_thread.Thread.create: threads are not enabled in this application")

   let self () =
      ()

   let id () =
      0

   let sigmask _ mask =
      mask
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
