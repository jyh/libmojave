(*
 * String utilities.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000 Jason Hickey, Caltech
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
 * jyh@cs.caltech.edu
 *)

(*
 * Hex representation of a string.
 *)
val hexify : string -> string

(*
 * Find a char in a string.
 *)
val strchr : string -> char -> int

(*
 * Standard definition of white space.
 *)
val white : string
val quotes : string

(*
 * Split a string into substrings.
 * The string is split on any character in delims.  Empty substrings
 * are returned as empty strings in the list.  For example:
 *   split ".-" "foo.bar--ba??z"
 * returns
 *   ["foo"; "bar"; ""; "ba??z"]
 *)
val split : string -> string -> string list

(*
 * Split a string str into a list of substrings.
 * The string is split on any character in delims.  Quotations
 * are not split.
 *
 * Empty substrings are _not_ returned as empty strings in the list.
 * For example:
 *   tokens "" ".-" "foo.bar--ba??z"
 * returns
 *   ["foo"; "bar"; "ba??z"]
 *)
val tokens : string -> string -> string -> string list
val tokens_std : string -> string list

(*
 * A third way to split into substrings.
 * The tokens are separated by white space,
 * and tokens may be quoted.
 *)
val parse_args : string -> string list

(*
 * Concatenate strings with a separator.
 *)
val concat : string -> string list -> string

(*
 * Search for a pattern in the indicated buffer, within the start
 * and length constraints applied to the buffer.  Note that this
 * uses a very inefficient algorithm; at some point I (JDS) will
 * get around to converting this to the Knuth-Morris-Pratt or
 * maybe Rabin-Karp algorithm.
 *
 * On success, this returns the offset (RELATIVE TO start!) of
 * the first match found; on failure, this raises Not_found.
 *)
val strpat : string -> int -> int -> string -> int

(*
 * Trim whitespace at outer boundaries from a string.
 *)
val trim : string -> string

(*
 * Trim all consecutive whitespace from a string, respecting
 * quotes.
 *)
val trim_all : string -> string -> string -> string
val trim_std : string -> string

(*
 * C version of a string.
 *)
val c_escaped : string -> string

(*
 * Read the file into a string.
 * Raises Sys_error if the file can't be opened.
 *)
val string_of_file : string -> string

(*
 * Lm_debug versions of standard library.
 *)
val create : string -> int -> string
val make : string -> int -> char -> string
val sub : string -> string -> int -> int -> string

(*
 * String environments.
 *)
module StringSet : Lm_set.LmSet with type elt = string
module StringTable : Lm_map.LmMap with type key = string
module StringMTable : Lm_map.LmMapList with type key = string

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
