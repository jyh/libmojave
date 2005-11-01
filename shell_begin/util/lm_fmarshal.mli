(*
 * A generic marshaler.
 * For marshaling, we need a
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2001 Jason Hickey, Caltech
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
 * All items eventually become ints, floats, or strings.
 *)
type 'a item =
   Bool of bool
 | Int of int
 | Magic of 'a
 | Lm_rawint of Lm_rawint.rawint
 | Float of float
 | Lm_rawfloat of Lm_rawfloat.rawfloat
 | String of string
 | List of 'a item list

(************************************************************************
 * MARSHALING
 ************************************************************************)

(*
 * IO module.
 *)
module type MarshalIOSig =
sig
   type t
   type in_channel
   type out_channel

   (* Convert between magic ids *)
   val magic_of_int : int -> t
   val int_of_magic : t -> int

   (* IO *)
   val input_byte : in_channel -> int
   val input_buffer : in_channel -> string -> int -> int -> unit
   val output_byte : out_channel -> int -> unit
   val output_buffer : out_channel -> string -> int -> int -> unit
end

(*
 * Marshal module.
 *)
module type MarshalSig =
sig
   type t
   type in_channel
   type out_channel

   val marshal : out_channel -> t item -> unit
   val unmarshal : in_channel -> t item
end

(*
 * Output routines.
 *)
module Make (IO : MarshalIOSig)
: MarshalSig
  with type t = IO.t
  with type in_channel = IO.in_channel
  with type out_channel = IO.out_channel

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
