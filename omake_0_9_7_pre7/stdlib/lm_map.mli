(*
 * Map module based on red-black trees
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 1999-2005 Jason Hickey, Caltech
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
open Lm_map_sig

module Make       (Ord : OrderedType) : (S         with type key = Ord.t)
module LmMake     (Ord : OrderedType) : (LmMap     with type key = Ord.t)
module LmMakeList (Ord : OrderedType) : (LmMapList with type key = Ord.t)

(*
 * This version includes a sharing constraint so that maps can
 * be used in recursive definitions.  This exposes the internal
 * representation, should you should avoid using it unless
 * absolutely necessary (like in a recursive type definition).
 *)
type ('key, 'value) tree

module LmMakeRec (Ord : OrderedType) : (LmMap     with type key = Ord.t with type 'a t = (Ord.t, 'a) tree)
