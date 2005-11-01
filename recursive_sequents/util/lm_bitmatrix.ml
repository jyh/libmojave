(*
 * Adjacency matrix.
 * Implement this with bitwise operations.
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
 * Raw bits.
 *)
type t = string array

(*
 * Characters are 8 bits wide.
 *)
let byte_shift = 3
let byte_size = 1 lsl byte_shift
let byte_mask = pred byte_size
let roundup i =
   (i + byte_mask) land (lnot byte_mask)

(*
 * Create a new matrix.
 *)
let create w h =
   let len = (roundup w) lsr byte_shift in
      Array.init h (fun _ -> String.make len '\000')

(*
 * Test if an edge exists.
 *)
let query m i j =
   let s = m.(i) in
   let j_byte = j lsr byte_shift in
   let j_bit = j land byte_mask in
   let c = Char.code s.[j_byte] in
      ((c lsr j_bit) land 1) = 1

(*
 * Add an edge.
 *)
let add m i j =
   let s = m.(i) in
   let j_byte = j lsr byte_shift in
   let j_bit = j land byte_mask in
   let c = Char.code s.[j_byte] in
   let c = c lor (1 lsl j_bit) in
      s.[j_byte] <- Char.chr c

(*
 * Remove an edge.
 *)
let remove m i j =
   let s = m.(i) in
   let j_byte = j lsr byte_shift in
   let j_bit = j land byte_mask in
   let c = Char.code s.[j_byte] in
   let c = c land (lnot (1 lsl j_bit)) in
      s.[j_byte] <- Char.chr c

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
