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
 * Copyright (C) 1998-2005 Jason Hickey, Cornell University
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
 * Author: Jason Hickey <jyh@cs.cornell.edu>
 *)
open Lm_big_int

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Have simple ints and big ints.
 *)
type num =
   Int of int
 | Big_int of big_int

(*
 * This is the max value represented in an int.
 *)
let shift_int = 30
let max_int = pred (1 lsl shift_int)
let min_int = -max_int

let shift_mult_int = 15
let max_mult_int = pred (1 lsl shift_mult_int)
let min_mult_int = -max_mult_int

(************************************************************************
 * IMPLEMENTATIONS                                                      *
 ************************************************************************)

(*
 * Catch overflows in addition.
 *)
let add_num i j =
   match i, j with
      Int i, Int j ->
         let sum = i + j in
            if (i>0) & (j>0) then
               if max_int - i < j then
                  Big_int (add_big_int (big_int_of_int i) (big_int_of_int j))
               else
                  Int sum
            else if (i<0) & (j<0) then
               if min_int - i > j then
                  Big_int (add_big_int (big_int_of_int i) (big_int_of_int j))
               else
                  Int sum
            else
               Int sum
    | Int i, Big_int j ->
         Big_int (add_big_int (big_int_of_int i) j)
    | Big_int i, Int j ->
         Big_int (add_big_int i (big_int_of_int j))
    | Big_int i, Big_int j ->
         Big_int (add_big_int i j)

let sub_num i j =
   match i, j with
      Int i, Int j ->
         let diff = i - j in
            if (i>0) & (j<0) then
               if i > max_int + j then
                  Big_int (sub_big_int (big_int_of_int i) (big_int_of_int j))
               else
                  Int diff
            else if (i<0) & (j>0) then
               if i < min_int + j then
                  Big_int (sub_big_int (big_int_of_int i) (big_int_of_int j))
               else
                  Int diff
            else
               Int diff
    | Int i, Big_int j ->
         Big_int (sub_big_int (big_int_of_int i) j)
    | Big_int i, Int j ->
         Big_int (sub_big_int i (big_int_of_int j))
    | Big_int i, Big_int j ->
         Big_int (sub_big_int i j)

let succ_num i =
   add_num i (Int 1)

let pred_num i =
   sub_num i (Int 1)

(*
 * Catch overflows in multiplication.
 *)
let mult_int i j =
   if (i >= min_mult_int) &&
      (i <= max_mult_int) &&
      (j >= min_mult_int) &&
      (j <= max_mult_int)
   then
      Int (i * j)
   else
      Big_int (mult_big_int (big_int_of_int i) (big_int_of_int j))

let mult_num i j =
   match i, j with
      Int i, Int j ->
         mult_int i j
    | Int i, Big_int j ->
         Big_int (mult_big_int (big_int_of_int i) j)
    | Big_int i, Int j ->
         Big_int (mult_big_int i (big_int_of_int j))
    | Big_int i, Big_int j ->
         Big_int (mult_big_int i j)

let div_num i j =
   match i, j with
      _ , Int 0 ->
         raise (Invalid_argument "Lm_num.div_num: division by zero")
    | Int i, Int j ->
         Int (i / j)
    | Int i, Big_int j ->
         Big_int (div_big_int (big_int_of_int i) j)
    | Big_int i, Int j ->
         Big_int (div_big_int i (big_int_of_int j))
    | Big_int i, Big_int j ->
         Big_int (div_big_int i j)

let mod_num i j =
   match i, j with
      _ , Int 0 ->
         raise (Invalid_argument "Lm_num.mod_num: division by zero")
    | Int i, Int j ->
         Int (i mod j)
    | Int i, Big_int j ->
         Big_int (mod_big_int (big_int_of_int i) j)
    | Big_int i, Int j ->
         Big_int (mod_big_int i (big_int_of_int j))
    | Big_int i, Big_int j ->
         Big_int (mod_big_int i j)

let quo_num = div_num
let rem_num = mod_num

(*
 * Power.  We stop large powers here--they will just take
 * forever.
 *)
let power_aux i j =
   if j = 0 then
      Int 1
   else
      let rec collect total j =
         if j = 0 then
            total
         else
            collect (mult_num total i) (pred j)
      in
         collect i (pred j)

let power_num i j =
   match j with
      Int j ->
         power_aux i j
    | Big_int j ->
         if is_integer_big_int j then
            power_aux i (integer_big_int j)
         else
            raise (Invalid_argument "power_num: argument is too big")

(*
 * Absolute value.
 *)
let abs_num = function
   Int i ->
      Int (abs i)
 | Big_int i ->
      Big_int (abs_big_int i)

(*
 * Equality.
 *)
let eq_num i j =
   match i, j with
      Int i, Int j ->
         i = j
    | Int i, Big_int j ->
         eq_big_int (big_int_of_int i) j
    | Big_int i, Int j ->
         eq_big_int i (big_int_of_int j)
    | Big_int i, Big_int j ->
         eq_big_int i j

let compare_num i j =
   match i, j with
      Int i, Int j ->
         Pervasives.compare i j
    | Int i, Big_int j ->
         compare_big_int (big_int_of_int i) j
    | Big_int i, Int j ->
         compare_big_int i (big_int_of_int j)
    | Big_int i, Big_int j ->
         compare_big_int i j

let lt_num i j =
   compare_num i j < 0

let le_num i j =
   compare_num i j <= 0

let gt_num i j =
   compare_num i j > 0

let ge_num i j =
   compare_num i j >= 0

let is_zero = function
   Int 0 -> true
 | Int _ -> false
 | Big_int i -> is_zero_big_int i

(************************************************************************
 * CONVERSION                                                           *
 ************************************************************************)

(*
 * Integer conversions.
 *)
let is_integer_num = function
   Int _ ->
      true
 | Big_int i ->
      is_integer_big_int i

let integer_num = function
   Int i ->
      i
 | Big_int i ->
      integer_big_int i

let num_of_int i =
   Int i

let int_of_num = integer_num

(*
 * String conversions.
 *)
let string_of_num = function
   Int i ->
      string_of_int i
 | Big_int i ->
      string_of_big_int i

let num_of_string s =
   let i = big_int_of_string s in
      if is_integer_big_int i then
         Int (integer_big_int i)
      else
         Big_int i

let to_string = string_of_num
let of_string = num_of_string

(*
 * Int32 conversions.
 *)
let to_int32 = function
   Int i ->
      Int32.of_int i
 | Big_int i ->
      Lm_big_int.to_int32 i

let of_int32 i =
   Big_int (Lm_big_int.of_int32 i)

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)