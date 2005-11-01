(*
 * A splay table is like a functional hash table.
 * This code is derived from the Splay_set code.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
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
 * Author: Alexey Nogin, Jason Hickey
 * nogin@cs.cornell.edu
 * jyh@cs.cornell.edu
 *)
open Lm_map_sig

(*
 * Create a table.
 *)
type ('elt, 'data) table

val create : ('elt, 'data, ('elt, 'data) table) table_create_type

(*
 * Build the table over an ordered type.
 *)
module MakeTable (Base : TableBaseSig)
: TableSig
  with type t = (Base.elt, Base.data) table
  with type elt = Base.elt
  with type data = Base.data

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
