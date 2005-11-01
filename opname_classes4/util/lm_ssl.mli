(*
 * Interface for SSL.
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

type t
type ssl_out
type ssl_in

exception SSLSigPipe

(*
 * SSL interface.
 *)
val enabled        : bool
val socket         : string -> t
val bind           : t -> Unix.inet_addr -> int -> unit
val getsockname    : t -> Unix.inet_addr * int
val listen         : t -> string -> int -> unit
val accept         : t -> t
val connect        : t -> Unix.inet_addr -> int -> unit
val shutdown       : t -> unit
val close          : t -> unit

(*
 * For restart.
 *)
val fd             : t -> int
val serve          : int -> string -> string -> t

(*
 * Buffered output.
 *)
val out_channel_of_ssl : t -> ssl_out

val fprintf : ssl_out -> ('a, ssl_out, unit) format -> 'a

val output_char : ssl_out -> char -> unit
val output_string : ssl_out -> string -> unit
val output_buffer : ssl_out -> Buffer.t -> unit
val output : ssl_out -> string -> int -> int -> unit
val flush : ssl_out -> unit
val close_out : ssl_out -> unit

(*
 * Buffered input.
 *)
val in_channel_of_ssl : t -> ssl_in
val input_char : ssl_in -> char
val input_line : ssl_in -> string
val really_input : ssl_in -> string -> int -> int -> unit
val close_in : ssl_in -> unit

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
