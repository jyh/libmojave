(*
 * Simple readline interface.
 * Copyright (C) 2002 Justin David Smith, Caltech
 * Copyright (C) 2000-2005 Jason Hickey, Alexey Nogin, Cornell University
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
 *)


(* initialize_readline ()
   Initialise the readline library.  *)
val initialize_readline : unit -> unit


(* register_commands commands
   Register a list of commands for tab completion.  This will clear
   the previous command-list; only one command-list may be installed
   at a time.  Command completion only applies to the first word on
   the command-line.  *)
val register_commands : string list -> unit

(* load_history filename
   Load a command history file (one command per line) and append it to the
   current history.
   Will raise Sys_error if the file cannot be opened.
   Will raise Not_found if the file does not exist. *)
val read_history : string -> unit

(* save_history filename
   Save the currently registered command history to a file (one command per
   line) deleting any previous contents.
   Will raise Sys_error if the file cannot be opened. *)
val write_history : string -> unit

(* history_truncate_file filename nlines
   Truncate history file filename to nlines in length, keeping most recent
   commands.
   Will raise Sys_error on failure *)
val history_truncate_file : string -> int -> unit

(* readline prompt
   Displays a readline prompt, and accepts a line of input from the user.
   Tab completion will be enabled as approprate.  Be sure to call the
   initialize_readline () function before calling this function.  This
   will raise End_of_file if the user strikes ^D.  *)
val readline : string -> string
