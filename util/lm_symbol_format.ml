(*
 * Formatting interface to symbols.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
open Lm_symbol
open Lm_format

let pp_print_ext_symbol buf v =
   Lm_format.pp_print_string buf (string_of_ext_symbol v)

let pp_print_symbol buf v =
   pp_print_string buf (string_of_symbol v)

let rec pp_print_symbol_list buf vl =
   match vl with
      [v] ->
         pp_print_symbol buf v
    | v :: vl ->
         fprintf buf "%a, %a" pp_print_symbol v pp_print_symbol_list vl
    | [] ->
         ()

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
