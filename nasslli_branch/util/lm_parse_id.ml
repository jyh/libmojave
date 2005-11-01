(*
   Parse a CVS ID string
   Copyright (C) 2002 Justin David Smith, Caltech

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


let parse_id id =
   let parts = Lm_string_util.split " " id in
      match parts with
         [prologue; name; rev; date; time; user; ty; epilogue] ->
            name, rev, date, time, user, ty
       | _ ->
            raise (Failure "parse_id:  Malformed ID string")


let parse_id_revision id =
   try
      let _, rev, _, _, _, _ = parse_id id in
         rev
   with
      Failure _ ->
         "unknown"
