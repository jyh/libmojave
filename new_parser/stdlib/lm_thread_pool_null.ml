(*
 * On Win32, select does not work on pipes.  Instead, we use
 * threads to call all the handlers.  We keep a thread pool.
 * When a thread makes progress, it wakes up the main process,
 * and returns to the pool.  Each file descriptor is assigned
 * a thread.
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
open Lm_printf
open Lm_debug

(*
 * Build debugging.
 *)
let debug_thread =
   create_debug (**)
      { debug_name = "thread";
        debug_description = "Display thread debugging";
        debug_value = false
      }

(*
 * Threads are not enabled.
 *)
let enabled = false

(*
 * Temporarily unlock the pool while performing IO.
 *)
let blocking_section f x =
   f x

(*
 * Start a thread doing something.
 *)
let create f =
   raise (Invalid_argument "Lm_thread_pool_null.create: threads are not enabled")

(*
 * Wait until something happens.
 *)
let wait () =
   raise (Invalid_argument "Lm_thread_pool_null.wait: threads are not enabled")

let waitpid _ =
   raise (Invalid_argument "Lm_thread_pool_null.waitpid: threads are not enabled")

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
