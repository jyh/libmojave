(*
 * Utilities on arrays.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Mojave Group, Caltech
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
 * Sorts an array, than eliminates the duplicate elements
 * and moves the remaining elements into an initial segment
 * of the input array. Returns the # of distinct elements.
 *)
let distinct cmp = function
   [||] -> 0
 | array ->
      let l = Array.length array in
      let rec d_find i =
         let j = (succ i) in
            if j = l then
               j
            else if (cmp array.(i) array.(j)) = 0 then
               d_copy i (succ j)
            else
               d_find j
      and d_copy i j =
         if j = l then
            succ i
         else if (cmp array.(i) array.(j)) = 0 then
            d_copy i (succ j)
         else
            let i = succ i in
               array.(i) <- array.(j);
               d_copy i (succ j)
      in
         Array.fast_sort cmp array;
         d_find 0

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
