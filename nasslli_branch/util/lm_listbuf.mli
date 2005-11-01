(*
   Simple list buffer utility (used for instruction buffers)
   Copyright (C) 2002,2001 Justin David Smith, Caltech

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *)


type 'a t

val empty : 'a t

val of_elt : 'a -> 'a t

val of_list : 'a list -> 'a t

val to_list : 'a t -> 'a list

val add : 'a t -> 'a -> 'a t

val add_list : 'a t -> 'a list -> 'a t

val add_rev_list : 'a t -> 'a list -> 'a t

val add_listbuf : 'a t -> 'a t -> 'a t

