(*
 * Filename globbing.
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

type glob_options =
   GlobNoBraces         (* Do not perform csh-style brace expansion *)
 | GlobNoEscape         (* The \ character does not escape special characters *)
 | GlobNoCheck          (* If an expansion fails, return the expansion literally *)
 | GlobDot              (* Allow wildcards to match filenames with a leading . *)
 | GlobOnlyDirs         (* Return only directories in the result *)
 | GlobCVSIgnore        (* Ignore files as specified by .cvsignore files *)
 | GlobIgnore of string list  (* Ignore the files that match the pattern *)
 | GlobAllow of string list   (* Allow only files that match the pattern *)
 | GlobIgnoreFun of (string -> bool)  (* Ignore the files determined by the function *)
 | GlobAllowFun of (string -> bool)   (* Allow only the files determined by the function *)

(*
 * The glob function returns two lists:
 *    1. a list of directories
 *    2. a list of files of other types
 *)
val glob : glob_options list -> string -> string list -> string list * string list

(*
 * Get the entries in a directory.
 *)
val list_dirs : glob_options list -> string list -> string list * string list
val list_dirs_rec : glob_options list -> string list -> string list * string list
val subdirs_of_dirs : glob_options list -> string list -> string list

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
