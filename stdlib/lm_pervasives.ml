(*
 * Initial built-in values.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000 Jason Hickey, Caltech
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

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Base types.
 *)
external type int = "%int"
external type char = "%char"
external type float = "%float"
external type unit = "%unit"
external type bool = "%bool"
external type exn = "%exn"
external type 'a list = "%list"
external type 'a array = "%array"
external type ('a, 'b, 'c) format = "%format"

(*
 * New additions.
 *)
type 'a ref = { mutable contents: 'a }
type 'a option = None | Some of 'a
type string = char array

(************************************************************************
 * EXCEPTIONS                                                           *
 ************************************************************************)

(*
 * Raise has to use a built-in command.
 *)
external raise : exn -> 'a = "%raise"

exception Assert_failure of string * int * int
exception Invalid_argument of string
exception Failure of string
exception Not_found
exception Out_of_memory
exception Stack_overflow
exception Sys_error of string
exception End_of_file
exception Division_by_zero
exception Exit
exception Sys_blocked_io

let invalid_arg s =
   raise (Invalid_argument s)

let failwith s =
   raise (Failure s)

(************************************************************************
 * COMPARISONS                                                          *
 ************************************************************************)

external (=)     : 'a -> 'a -> bool   = "%cmp_equal"
external (<>)    : 'a -> 'a -> bool   = "%cmp_nequal"
external (<)     : 'a -> 'a -> bool   = "%cmp_lt"
external (>)     : 'a -> 'a -> bool   = "%cmp_gt"
external (<=)    : 'a -> 'a -> bool   = "%cmp_le"
external (>=)    : 'a -> 'a -> bool   = "%cmp_ge"
external compare : 'a -> 'a -> int    = "%cmp_compare"

let min x y =
   if x < y then
      x
   else
      y

let max x y =
   if x > y then
      x
   else
      y

external (==)    : 'a -> 'a -> bool   = "%cmp_eq"
external (!=)    : 'a -> 'a -> bool   = "%cmp_neq"

(************************************************************************
 * BOOLEAN OPERATIONS                                                   *
 ************************************************************************)

let not = function
   true -> false
 | false -> true

(* external (&&) : bool -> bool -> bool = "%band" (* These are actually language constructs *)
   external (||) : bool -> bool -> bool = "%bor" *)
let (&) a b = match a with
    true -> b
  | false -> a
let (or) a b = match a with
    true -> a
  | false -> b

external (~-) : int -> int = "%int_neg"
external succ : int -> int = "%int_succ"
external pred : int -> int = "%int_pred"
external (+)  : int -> int -> int = "%int_add"
external (-)  : int -> int -> int = "%int_sub"
external ( * ) : int -> int -> int = "%int_mul"
external (/)  : int -> int -> int = "%int_div"
external (mod) : int -> int -> int = "%int_mod"
external abs   : int -> int = "%int_abs"
external max_int : int = "%int_max"
external min_int : int = "%int_min"

(************************************************************************
 * BITWISE OPERATIONS                                                   *
 ************************************************************************)

external (land) : int -> int -> int = "%int_and"
external (lor)  : int -> int -> int = "%int_or"
external (lxor) : int -> int -> int = "%int_xor"
external lnot   : int -> int        = "%int_not"
external (lsl)  : int -> int -> int = "%int_shl"
external (lsr)  : int -> int -> int = "%int_shr"
external (asr)  : int -> int -> int = "%int_sar"

(************************************************************************
 * FLOATING POINT                                                       *
 ************************************************************************)

external (~-.)  : float -> float    = "%float_neg"
external (+.)   : float -> float -> float = "%float_add"
external (-.)   : float -> float -> float = "%float_sub"
external ( *. ) : float -> float -> float = "%float_mul"
external (/.)   : float -> float -> float = "%float_div"
external ( ** ) : float -> float -> float = "%float_power"

external sqrt   : float -> float = "%float_sqrt"
external exp    : float -> float = "%float_exp"
external log    : float -> float = "%float_log"
external log10  : float -> float = "%float_log10"
external cos    : float -> float = "%float_cos"
external sin    : float -> float = "%float_sin"
external tan    : float -> float = "%float_tan"
external acos   : float -> float = "%float_acos"
external asin   : float -> float = "%float_asin"
external atan   : float -> float = "%float_atan"
external atan2  : float -> float -> float = "%float_atan2"
external cosh   : float -> float = "%float_cosh"
external sinh   : float -> float = "%float_sinh"
external tanh   : float -> float = "%float_tanh"

external ceil   : float -> float = "%float_ceil"
external floor  : float -> float = "%float_floor"
external abs_float : float -> float = "%float_abs"
external mod_float : float -> float -> float = "%float_mod"
external frexp     : float -> float * int    = "%float_rexp"
external ldexp     : float -> int -> float   = "%float_ldexp"
external modf      : float -> float * float  = "%float_modf"

external truncate     : float -> int         = "%float_int"
external int_of_float : float -> int         = "%float_int"
external float     : int -> float            = "%int_float"
external float_of_int : int -> float         = "%int_float"

(************************************************************************
 * STRINGS                                                              *
 ************************************************************************)

external (^) : string -> string -> string = "%array_append" (* XXX *)

(************************************************************************
 * CHARACTERS                                                           *
 ************************************************************************)

external int_of_char : char -> int = "%char_int"
external int_to_char : int -> char = "%int_char"
let char_of_int c =
  match c with
      0 .. 255 -> int_to_char c (* This is NOT OCaml compliant, for whatever bizzare reasoning they have *)
    | _ -> raise (Invalid_argument "char_of_int")

(************************************************************************
 * UNIT                                                                 *
 ************************************************************************)

let ignore _ = ()

(************************************************************************
 * STRING CONVERSION                                                    *
 ************************************************************************)

let string_of_bool = function
   true -> "true"
 | false -> "false"

let bool_of_string = function
   "true" -> true
 | "false" -> false
 | _ -> raise (Invalid_argument "bool_of_string")

(*
 * Buffer for conversions.
 *)
external string_of_int : int -> string = "%int_string"
external int_of_string : string -> int = "%string_int"
external string_of_float : float -> string = "%float_string"
external float_of_string : string -> float = "%string_float"

(************************************************************************
 * PAIRS                                                                *
 ************************************************************************)

external fst : 'a * 'b -> 'a = "%fst"
external snd : 'a * 'b -> 'b = "%snd"

(************************************************************************
 * LISTS                                                                *
 ************************************************************************)

let rec append l1 l2 =
   match l1 with
      h :: t ->
         h :: append t l2
    | [] ->
         l2

let (@) = append

(************************************************************************
 * REFERENCES                                                           *
 ************************************************************************)

let ref x =
   { contents = x }

let (!) { contents = x } =
   x

let (:=) x y =
   x.contents <- y

let incr x =
   x := succ !x

let decr x =
   x := pred !x

(************************************************************************
 * PROGRAM TERMINATION                                                  *
 ************************************************************************)

external exit : int -> 'a = "%exit"

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
