(*
 * Generic buffered IO channel.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)
open Lm_location

type channel
type t = channel

(*
 * The channel may be a file, pipe, or socket.
 *)
type kind =
   FileChannel
 | PipeChannel
 | SocketChannel

type mode =
   InChannel
 | OutChannel
 | InOutChannel

(* Creation *)
val create        : string -> kind -> mode -> bool -> Unix.file_descr option -> t
val name          : t -> string
val descr         : t -> Unix.file_descr
val close         : t -> unit
val info          : t -> int * kind * mode * bool

val set_id        : t -> int -> unit

val of_string     : string -> t
val of_substring  : string -> int -> int -> t
val of_loc_string : string -> int -> int -> string -> t
val of_fun        : (bytes -> int -> int -> int) -> (bytes -> int -> int -> int) -> t

(* Output to strings *)
val create_string     : unit -> t
val create_loc_string : string -> int -> int -> t
val to_string         : t -> string

(* Set the file and line number *)
val set_line      : t -> string -> int -> unit

(*
 * Set text vs binary mode.
 * No effect unless on Win32.
 *)
val set_binary_mode : t -> bool -> unit

(* The write function is arbitrary and can be replaced *)
val set_io_functions : t ->
   (bytes -> int -> int -> int) ->  (* Reader *)
   (bytes -> int -> int -> int) ->  (* Writer *)
   unit

(* Positioning *)
val tell            : t -> int
val seek            : t -> int -> Unix.seek_command -> int
val loc             : t -> loc

(* Check if there is already input in the buffer *)
val poll          : t -> bool

(* Buffered IO *)
val input_char    : t -> char
val input_byte    : t -> int
val input_buffer  : t -> bytes -> int -> int -> unit
val input_line    : t -> string
val input_entire_line : t -> string

(*
 * read is a partial read. It returns the bytes already in the channel's
 * buffer if there are any, and otherwise reads straight from the
 * descriptor into [s]. On a text-mode channel (Windows only) the
 * buffer holds translated text ("\r\n" read as "\n", filled by the
 * input functions above) while a read straight from the descriptor is
 * not translated, so read returns "\r\n" or "\n" depending on what was
 * read before it. Use the input functions to read text.
 *)
val read          : t -> bytes -> int -> int -> int

(* Flush data to the channel *)
val flush         : t -> unit

(* Buffered IO *)
val output_char   : t -> char -> unit
val output_byte   : t -> int -> unit
val output_buffer : t -> string -> int -> int -> unit
val output_string : t -> string -> unit

(*
 * write is a partial write, straight to the descriptor after flushing
 * the buffer: always binary, so "\n" is not written as "\r\n" on a
 * text-mode channel. The output functions above translate.
 *)
val write         : t -> bytes -> int -> int -> int

(*
 * Select. A negative timeout waits with no limit. On Windows a pipe is
 * polled every 50 ms, and a pipe writer that still holds buffered output
 * is flushed by an output thread; in a build without threads (the
 * default profile) that thread cannot start, so select on such a writer
 * raises Invalid_argument "Lm_thread_pool_null.create: threads are not
 * enabled". Flush the channel before selecting on it there. Measured on
 * the Windows CI lanes by test_channel_pipe.
 *)
val select        : t list -> t list -> t list -> float -> t list * t list * t list

(* Lex-mode operations *)
module LexerInput :
sig
   type t = channel

   val lex_start     : t -> int
   val lex_restart   : t -> int -> unit
   val lex_stop      : t -> int -> unit
   val lex_string    : t -> int -> string
   val lex_substring : t -> int -> int -> string
   val lex_next      : t -> int
   val lex_pos       : t -> int
   val lex_buffer    : t -> Buffer.t -> unit
   val lex_loc       : t -> int -> loc
   val bof           : int
   val eof           : int
end

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
