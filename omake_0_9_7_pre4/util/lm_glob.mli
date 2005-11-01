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
open Lm_lexer

type glob_options =
   GlobNoBraces         (* Do not perform csh-style brace expansion *)
 | GlobNoTilde          (* Do not perform tilde-expansion *)
 | GlobNoEscape         (* The \ character does not escape special characters *)
 | GlobNoCheck          (* If an expansion fails, return the expansion literally *)
 | GlobIgnoreCheck      (* If an expansion fails, it expands to nothing *)
 | GlobDot              (* Allow wildcards to match filenames with a leading . *)
 | GlobOnlyDirs         (* Return only directories in the result *)
 | GlobCVSIgnore        (* Ignore files as specified by .cvsignore files *)
 | GlobIgnore of string list  (* Ignore the files that match the pattern *)
 | GlobAllow of string list   (* Allow only files that match the pattern *)
 | GlobIgnoreFun of (string -> bool)  (* Ignore the files specified by the function *)
 | GlobAllowFun of (string -> bool)   (* Allow only the files specified by the function *)
 | GlobHomeDir of string              (* Home directory for ~ expansion *)
 | GlobProperSubdirs                  (* Include only proper subdirs in listing *)

(*
 * The initial home directory for tilde expansion.
 * The globber does its best to figure this out.
 *)
val home_dir : string

(*
 * Try to collapse a filename.
 * Tilde-expansion will invert this process.
 *)
val tilde_collapse : string -> string

(*
 * The glob function returns two lists:
 *    1. a list of directories
 *    2. a list of files of other types
 *
 * The second argument to the glob and the glob_argv functions is the directory
 * where to perform expansion. If the glob pattern is relative, the results are
 * left relative (to that directory) as well.
 *
 * Raises Failure if the syntax is ill-formed.
 *)
val glob : glob_options list -> string -> string list -> string list * string list

(*
 * Glob a command line.
 * Preserves the argument ordering.
 *)
val glob_argv : glob_options list -> string -> string list -> string list

(*
 * Get the entries in a directory.
 *
 *    list_dirs root dirs
 *       root: the directory prefix, not appended to the output strings
 *       dirs: the directories to list
 *)
val list_dirs : glob_options list -> string -> string list -> string list * string list
val list_dirs_rec : glob_options list -> string -> string list -> string list * string list
val subdirs_of_dirs : glob_options list -> string -> string list -> string list

(*
 * Utilities.
 *)
val regex_of_shell_pattern : glob_options list -> string -> LmStr.t

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
