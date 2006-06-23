(*
 * An null implementation of threads.
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
