(*
 * This is a generic printf builder.  We take "simple" printing
 * functions, and turn them into a general printf.
 *
 * Formatted printing.
 * Here are the format strings we handle.
 *    d or i: print an integer in decminal
 *    u: print an unsigned integer in decimal
 *    x: print an integer in unsigned hex in lowercase
 *    X: print an integer in unsigned hex in uppercase
 *    o: print an integer in unsigned octal
 *    s: print a string
 *    c: print a character
 *    f: print a float in decimal
 *    e,E: print a float in exponent notation
 *    g,G: print a float in best notation
 *    b: print a Boolean
 *    a: user-defined printer
 *    t: user-defined printer
 *    %: print the '%' char
 *
 * From the printf man page, each format specifier has
 *    1. 0 or more flags
 *       #: use alternate notation
 *       0: 0-pad the number
 *       '-': left-justify the field
 *       ' ': leave a space before the number
 *       '+': always print the sign of the number
 *    2. An optional field width in decimal
 *    3. An optional precision, specified as a '.' followed
 *       by a decimal number.
 *    4. A format specifier
 *
 * For Format:
 *    @]: close_box
 *    @,: print_cut
 *    @ : print_space
 *    @\n: force_newline
 *    @;: print_break
 *    @?: print_flush
 *    @.: print_newline
 *    @<n>: print_length
 *    @@: plain @ char
 *
 * Note the copious use of Obj.magic
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
 * Argument module.
 *)
module type PrintfArgsSig =
sig
   (* Some buffer type *)
   type t

   (* The printers *)
   val print_char : t -> char -> unit
   val print_string : t -> string -> unit

   (* Format functions *)
   val open_box : t -> int -> unit
   val open_hbox : t -> unit
   val open_vbox : t -> int -> unit
   val open_hvbox : t -> int -> unit
   val open_hovbox : t -> int -> unit
   val close_box : t -> unit

   val print_cut : t -> unit
   val print_space : t -> unit
   val force_newline : t -> unit
   val print_break : t -> int -> int -> unit
   val print_flush : t -> unit
   val print_newline : t -> unit
end

(*
 * What this module provides.
 *)
module type PrintfSig =
sig
   (* Some buffer type *)
   type t

   (* Printf functions *)
   val fprintf : t -> ('a, t, unit) format -> 'a
end

(*
 * Here's the actual printf module.
 *)
module MakePrintf (Args : PrintfArgsSig)
: PrintfSig with type t = Args.t

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
