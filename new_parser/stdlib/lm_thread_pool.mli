(*
 * Select doesn't work on Win32, so use threads instead.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Jason Hickey, Caltech
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
 * Debugging.
 *)
val debug_thread : bool ref

(*
 * Are threads enabled?
 *)
val enabled : bool

(*
 * Start a job in a new thread.
 * If the bool is false, the result is
 * not returned by wait when the thread exits.
 *)
val create : bool -> (unit -> unit) -> int

(*
 * When a job performs blocking IO, it should
 * unlock the main lock.
 *)
val blocking_section : ('a -> 'b) -> 'a -> 'b

(*
 * Wait for any of the jobs to complete.
 *)
val wait : unit -> int list

(*
 * Wait for a specific job to complete.
 *)
val waitpid : int -> unit

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
