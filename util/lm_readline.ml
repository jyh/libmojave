(*
 * Simple readline interface.
 * Copyright (C) 2002 Justin David Smith, Caltech
 * Copyright (C) 2000 Jason Hickey, Alexey Nogin, Cornell University
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


(* C readline function returns None at EOF *)
external caml_initialise_readline : unit -> unit = "caml_initialise_readline"
external caml_register_commands : string list -> unit = "caml_register_commands"
external caml_readline : string -> string option = "caml_readline"


(* initialise_readline ()
   Initialise the readline library.  *)
let initialise_readline () =
   caml_initialise_readline ()


(* register_commands commands
   Register a list of commands for tab completion.  This will clear
   the previous command-list; only one command-list may be installed
   at a time.  Command completion only applies to the first word on
   the command-line.  *)
let register_commands commands =
   caml_register_commands commands


(* readline prompt
   Displays a readline prompt, and accepts a line of input from the user.
   Tab completion will be enabled as approprate.  Be sure to call the
   initialise_readline () function before calling this function.  This
   will raise End_of_file if the user strikes ^D.  *)
let readline prompt =
   match caml_readline prompt with
      Some s ->
         s
    | None ->
         raise End_of_file
