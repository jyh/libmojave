(*
 * Our slow implementation of numbers
 * without using C libraries.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/htmlman/default.html or visit http://metaprl.org/
 * for more information.
 *
 * Copyright (C) 1998-2005 PRL Group, Cornell University and Caltech
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 * 
 * Additional permission is given to link this library with the
 * OpenSSL project's "OpenSSL" library, and with the OCaml runtime,
 * and you may distribute the linked executables.  See the file
 * LICENSE.libmojave for more details.
 *
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 * Modified by: Aleksey Nogin <nogin@cs.caltech.edu>
 *)

type num

(*
 * Operations.
 *)
val add_num : num -> num -> num
val sub_num : num -> num -> num
val mult_num : num -> num -> num
val div_num : num -> num -> num
val mod_num : num -> num -> num
val quo_num : num -> num -> num
val rem_num : num -> num -> num
val neg_num : num -> num
val abs_num : num -> num
val power_num : num -> num -> num

val succ_num : num -> num
val pred_num : num -> num

(*
 * Comparision.
 *)
val lt_num : num -> num -> bool
val le_num : num -> num -> bool
val eq_num : num -> num -> bool
val ge_num : num -> num -> bool
val gt_num : num -> num -> bool
val compare_num : num -> num -> int
val is_zero : num -> bool

(*
 * Conversion.
 *
 * is_integer_num n is true when n fits Lm_num's Int range, |n| < 2^30
 * (31-bit ints, as on a 32-bit OCaml), NOT whether n fits an OCaml int:
 * on 64-bit, 2^30 is a native int and is_integer_num says false for it.
 * integer_num and int_of_num raise outside that range. To read a num as a
 * native int, use int_of_string_opt (string_of_num n).
 * (test/test_num_rest.ml pins the range; metaprl's Filter_summary.dest_id
 * read a 64-bit hash this way and refused ids >= 2^30.)
 *)
val is_integer_num : num -> bool
val integer_num : num -> int
val num_of_int : int -> num
val int_of_num : num -> int

val string_of_num : num -> string
val num_of_string : string -> num
val to_string : num -> string
val of_string : string -> num
val to_int32 : num -> Int32.t
val of_int32 : Int32.t -> num

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
