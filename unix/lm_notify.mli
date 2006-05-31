(*
 * File-change notification services.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
 * Event manager.
 *)
type t

(*
 * Events.
 *)
type code =
   Changed
 | Deleted
 | StartExecuting
 | StopExecuting
 | Created
 | Moved
 | Acknowledge
 | Exists
 | EndExist
 | DirectoryChanged

type event =
   { notify_code : code;
     notify_name : string
   }

(*
 * Debugging.
 *)
val debug_notify      : bool ref
val string_of_code    : code -> string

(*
 * Methods.
 *)
val enabled           : bool
val create            : unit -> t
val close             : t -> unit
val file_descr        : t -> Unix.file_descr
val monitor           : t -> string -> bool -> unit
val pending           : t -> bool
val next_event        : t -> event

val suspend           : t -> string -> unit
val resume            : t -> string -> unit
val cancel            : t -> string -> unit

val suspend_all       : t -> unit
val resume_all        : t -> unit
val cancel_all        : t -> unit

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
