(*
 * Sigs for testing.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000-2005 Jason Hickey, Caltech
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

module type XSig =
sig
   type t1
   type t2
   val x : t1
end

module type YSig =
sig
   type t1
   val x : t1
end

module type ZSig = functor (X : XSig) -> YSig

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
