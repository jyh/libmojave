(*
 * A simple database.  This is a low-level ionterface.
 * See, for example, omake_db.ml to see a higher-level
 * interface.
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
type t = Unix.file_descr

type tag = int
type magic = string
type digest = string

(*
 * Some kinds of entries are host-independent.
 *)
type host =
   HostIndependent
 | HostDependent

(*
 * These functions assume that the file is locked.
 *    tag: the kind of entry
 *    magic: the magic number for this version
 *    digest: the source file digest (or use the empty string)
 *
 * These functions operate by side-effect, modifying the file.
 *    add: remove the old entry and add a new one
 *    find: find an existing entry, or raise Not_found if it doesn't exist
 *    remove: remove an old entry, does not fail.
 *)
val add    : t -> tag * host -> magic -> digest -> 'a -> unit
val find   : t -> tag * host -> magic -> digest -> 'a
val remove : t -> tag * host -> magic -> unit

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
