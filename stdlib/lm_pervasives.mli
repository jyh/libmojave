(*
 * Oveeride some basic functions, mostly for debugging hooks.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
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
 * jyh@cs.cornell.edu
 *)

(*
 * Override of input functions is mainly for debugging.
 *)
val open_in     : string -> in_channel
val open_in_bin : string -> in_channel

(*
 * Type t of buffers.
 *)
type out_channel

(*
 * Normal buffers.
 *)
val stdout : out_channel
val stderr : out_channel
val stdstr : out_channel

(*
 * Capailibities.
 *)
val capability_divert : bool

(*
 * Get the string from the stdstr channel.
 *)
val flush_stdstr : unit -> string

(*
 * Open new channels.
 *)
val open_out     : string -> out_channel
val open_out_bin : string -> out_channel

(*
 * Simple printing.
 *)
val output_char    : out_channel -> char -> unit
val output_string  : out_channel -> string -> unit
val output_rbuffer : out_channel -> Lm_rformat.buffer -> unit

(*
 * These functions are bad style for functional programs.
 *)
val print_char    : char -> unit
val print_int     : int -> unit
val print_string  : string -> unit
val print_rbuffer : Lm_rformat.buffer -> unit

val prerr_char    : char -> unit
val prerr_int     : int -> unit
val prerr_string  : string -> unit
val prerr_rbuffer : Lm_rformat.buffer -> unit

(*
 * Flush the output.
 *)
val flush  : out_channel -> unit
val eflush : out_channel -> unit

(*
 * Also, include the printf-style functions.
 *)
module PervasivesPrintf :
sig
   val eprintf : ('a, out_channel, unit) format -> 'a
   val printf  : ('a, out_channel, unit) format -> 'a
   val sprintf : ('a, unit, string) format -> 'a
   val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
end

(*
 * List printing helpers.
 *)
val print_any_list : (out_channel -> 'a -> unit) -> out_channel -> 'a list -> unit
val print_string_list : out_channel -> string list -> unit
val print_int_list : out_channel -> int list -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
