(*
 * Utilities on filenames.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2005 Jason Hickey, Caltech
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
 * Search for the index after the drive letter.
 *)
type root =
   NullRoot
 | DriveRoot of char

type 'a path =
   RelativePath of 'a
 | AbsolutePath of root * 'a

(*
 * Pathname separators.
 *)
val separator_char   : char
val separator_string : string
val separators       : string

(*
 * Remove quotations from a string that represents a filename.
 *)
val unescape_string  : string -> string

(*
 * Normalize function will give the canonical
 * lowercase name on Windows.  It is a nop on
 * Unix.
 *)
val normalize_string : string -> string
val normalize_path   : string list -> string list

(*
 * A null root directory.
 *)
val null_root       : root

(*
 * Get the root string.
 *)
val string_of_root  : root -> string

(*
 * Skip the drive letter if it exists.
 *)
val drive_skip      : string -> int

(*
 * Parse filenames.
 *)
val filename_string : string -> string path
val filename_path   : string -> string list path

(*
 * Split into root, suffix.
 *)
val split : string -> string * string

(*
 * Get the name without suffix.
 *)
val root : string -> string
val suffix : string -> string
val strip_suffixes : string -> string

(*
 * Path simplification.
 * Remove . and .. entries.
 *)
type pathname = string list

val split_path : string -> pathname
val simplify_path : pathname -> pathname
val concat_path : pathname -> string

(*
 * Path searching.
 *)
val is_executable : string -> string option
val which : string -> string
val which_dir : string -> string -> string
val where : string -> string list

(*
 * Make an entire hierarchy.
 *)
val mkdirhier : string -> int -> unit

(*
 * Get the listing in a directory.
 *)
val lsdir : string -> string list

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
