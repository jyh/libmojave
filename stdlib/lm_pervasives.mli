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
val raise : exn -> 'a

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

val invalid_arg : string -> 'a
val failwith : string -> 'a

(************************************************************************
 * COMPARISONS                                                          *
 ************************************************************************)

val (=)     : 'a -> 'a -> bool  
val (<>)    : 'a -> 'a -> bool  
val (<)     : 'a -> 'a -> bool  
val (>)     : 'a -> 'a -> bool  
val (<=)    : 'a -> 'a -> bool  
val (>=)    : 'a -> 'a -> bool  
val compare : 'a -> 'a -> int   

val min : 'a -> 'a -> 'a
val max : 'a -> 'a -> 'a

val (==)    : 'a -> 'a -> bool  
val (!=)    : 'a -> 'a -> bool  

(************************************************************************
 * BOOLEAN OPERATIONS                                                   *
 ************************************************************************)

val not : bool -> bool

(* val (&&) : bool -> bool -> bool   (* These are actually language constructs now *)
   val (||) : bool -> bool -> bool *)
val (&)  : bool -> bool -> bool
val (or) : bool -> bool -> bool
val (~-) : int -> int
val succ : int -> int
val pred : int -> int
val (+)  : int -> int -> int
val (-)  : int -> int -> int
val ( * ) : int -> int -> int
val (/)  : int -> int -> int
val (mod) : int -> int -> int
val abs   : int -> int
val max_int : int
val min_int : int

(************************************************************************
 * BITWISE OPERATIONS                                                   *
 ************************************************************************)

val (land) : int -> int -> int
val (lor)  : int -> int -> int
val (lxor) : int -> int -> int
val lnot   : int -> int       
val (lsl)  : int -> int -> int
val (lsr)  : int -> int -> int
val (asr)  : int -> int -> int

(************************************************************************
 * FLOATING POINT                                                       *
 ************************************************************************)

val (~-.)  : float -> float   
val (+.)   : float -> float -> float
val (-.)   : float -> float -> float
val ( *. ) : float -> float -> float
val (/.)   : float -> float -> float
val ( ** ) : float -> float -> float

val sqrt   : float -> float
val exp    : float -> float
val log    : float -> float
val log10  : float -> float
val cos    : float -> float
val sin    : float -> float
val tan    : float -> float
val acos   : float -> float
val asin   : float -> float
val atan   : float -> float
val atan2  : float -> float
val cosh   : float -> float
val sinh   : float -> float
val tanh   : float -> float

val ceil   : float -> float
val floor  : float -> float
val abs_float : float -> float
val mod_float : float -> float -> float
val frexp     : float -> float * int   
val ldexp     : float -> int -> float  
val modf      : float -> float * float 
val float     : int -> float           
val float_of_int : int -> float        
val truncate     : float -> int        
val int_of_float : float -> int        

(************************************************************************
 * STRINGS                                                              *
 ************************************************************************)

val (^) : string -> string -> string

(************************************************************************
 * CHARACTERS                                                           *
 ************************************************************************)

val int_of_char : char -> int
val char_of_int : int -> char

(************************************************************************
 * UNIT                                                                 *
 ************************************************************************)

val ignore : 'a -> unit

(************************************************************************
 * STRING CONVERSION                                                    *
 ************************************************************************)

val string_of_bool : bool -> string
val bool_of_string : string -> bool

(*
 * Buffer for conversions.
 *)
val string_of_int : int -> string
val int_of_string : string -> int
val string_of_float : float -> string
val float_of_string : string -> float

(************************************************************************
 * PAIRS                                                                *
 ************************************************************************)

val fst : 'a * 'b -> 'a
val snd : 'a * 'b -> 'b

(************************************************************************
 * LISTS                                                                *
 ************************************************************************)

val (@) : 'a list -> 'a list -> 'a list

(************************************************************************
 * REFERENCES                                                           *
 ************************************************************************)

val ref : 'a -> 'a ref
val (!) : 'a ref -> 'a
val (:=) : 'a ref -> 'a -> unit
val incr : int ref -> unit
val decr : int ref -> unit

(************************************************************************
 * PROGRAM TERMINATION                                                  *
 ************************************************************************)

val exit : int -> 'a

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
