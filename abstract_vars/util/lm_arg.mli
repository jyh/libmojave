(*
 * Parsing command line arguments, MCC-style. Arguments to options
 * may be separated from the option by a space, or may be placed  
 * immediately after the option (without space) IF the option is 
 * not ambiguous.  Also, options may be abbreviated as long as the
 * short form is not ambiguous.
 *
 * ---------------------------------------------------------------- 
 *
 * Copyright (C) 2002, Justin David Smith, Caltech
 * Based on original code, Copyright (C) 2000 Jason Hickey, Caltech
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


type spec =
   Unit of (unit -> unit)
 | Set of bool ref
 | Clear of bool ref
 | String of (string -> unit)
 | Int of (int -> unit)
 | Float of (float -> unit)
 | Rest of (string -> unit)
 
type section = (string * spec * string) list

type sections = (string * section) list


val parse : sections -> (string -> unit) -> string -> unit
val usage : sections -> string -> unit
