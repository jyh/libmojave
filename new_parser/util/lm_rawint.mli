(*
 *
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
 * Various forms of raw integers.
 *)
type int_precision =
   Int8
 | Int16
 | Int32
 | Int64

(*
 * Signed or unsigned.
 *)
type int_signed = bool

(*
 * Actual numbers.
 *)
type rawint

val precision : rawint -> int_precision
val signed : rawint -> int_signed
val is_zero : rawint -> bool
val is_one : rawint -> bool

(*
 * Conversion.
 *)
val of_rawint : int_precision -> int_signed -> rawint -> rawint

(*
 * Constructors.
 *)
val of_int : int_precision -> int_signed -> int -> rawint
val of_float : int_precision -> int_signed -> float -> rawint
val of_int32 : int_precision -> int_signed -> int32 -> rawint
val of_int64 : int_precision -> int_signed -> int64 -> rawint
val of_nativeint : int_precision -> int_signed -> nativeint -> rawint
val of_string : int_precision -> int_signed -> string -> rawint

val to_byte : rawint -> int
val to_short : rawint -> int
val to_int : rawint -> int
val to_float : rawint -> float
val to_int32 : rawint -> int32
val to_int64 : rawint -> int64
val to_nativeint : rawint -> nativeint
val to_string : rawint -> string

(*
 * Comparison.
 *)
val compare : rawint -> rawint -> int

(*
 * Basic arithmetic.
 *)
val neg    : rawint -> rawint
val uminus : rawint -> rawint
val succ   : rawint -> rawint
val pred   : rawint -> rawint
val abs    : rawint -> rawint

val add : rawint -> rawint -> rawint
val sub : rawint -> rawint -> rawint
val mul : rawint -> rawint -> rawint
val div : rawint -> rawint -> rawint
val rem : rawint -> rawint -> rawint
val min : rawint -> rawint -> rawint
val max : rawint -> rawint -> rawint
val max_int : int_precision -> int_signed -> rawint
val min_int : int_precision -> int_signed -> rawint

(* i, off, len *)
val field : rawint -> int -> int -> rawint
val set_field : rawint -> int -> int -> rawint -> rawint

(*
 * Bitwise operations.
 *)
val logand : rawint -> rawint -> rawint
val logor : rawint -> rawint -> rawint
val logxor : rawint -> rawint -> rawint
val lognot : rawint -> rawint
val shift_left : rawint -> rawint -> rawint
val shift_left_int : rawint -> int -> rawint
val shift_right : rawint -> rawint -> rawint

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
