(*
 * Serialized map. Acts as a normal map, but the order in
 * which elements are inserted is retained, and all iterating
 * functions visit elements in that order.
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2002 Adam Granicz, Caltech
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
 * Author: Adam Granicz
 * granicz@cs.caltech.edu
 *)
open Lm_map_sig

(*
 * These are the functions provided by the table.
 *)
module type SerialMap =
sig
   type key
   type 'a tt

   val empty : 'a tt
   val is_empty : 'a tt -> bool
   val cardinal : 'a tt -> int
   val add : 'a tt -> key -> 'a -> 'a tt
   val find : 'a tt -> key -> 'a
   val remove : 'a tt -> key -> 'a tt
   val mem : 'a tt -> key -> bool

   val iter : (key -> 'a -> unit) -> 'a tt -> unit
   val fold : ('a -> key -> 'b -> 'a) -> 'a -> 'b tt -> 'a

   (* Both keys and data return a list ordered by insertion order *)
   val keys : 'a tt -> key list
   val data : 'a tt -> 'a list
end

(*
 * Make the map.
 *)
module SerialMapMake (Base : OrderedType)
: SerialMap with type key = Base.t

