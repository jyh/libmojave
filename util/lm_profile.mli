(*
   Profiling code
   Copyright (C) 2001 Justin David Smith, Caltech

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

val difference_of : Unix.process_times -> Unix.process_times -> float
val profile : string -> ('a -> 'b) -> 'a -> 'b
val profile_file_opened : bool ref
val profile_file : string ref
val profile_out : out_channel ref
