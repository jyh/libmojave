(*
   Simple imperative list buffer utility (used for instruction buffers)
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


(***  Type definitions  ***)

type 'a t =
   Empty
 | FwdList of 'a list
 | RevList of 'a list


(***  Functions  ***)

let empty = Empty

let of_elt elt = RevList [elt]

let of_list ls = FwdList ls

let to_list buf =
   match buf with
      Empty ->
         []
    | FwdList buf ->
         buf
    | RevList buf ->
         List.rev buf

let add buf elt = 
   match buf with
      Empty ->
         RevList [elt]
    | FwdList buf ->
         RevList (elt :: List.rev buf)
    | RevList buf ->
         RevList (elt :: buf)

let add_list buf ls =
   let rec add_list buf = function
      elt :: ls ->
         add_list (elt :: buf) ls
    | [] ->
         RevList buf
   in
      match buf with
         Empty ->
            FwdList ls
       | FwdList buf ->
            add_list (List.rev buf) ls
       | RevList buf ->
            add_list buf ls

let add_rev_list buf ls =
   let rec add_rev_list buf = function
      elt :: ls ->
         elt :: (add_rev_list buf ls)
    | [] ->
         buf
   in
      match buf with
         Empty ->
            RevList ls
       | FwdList buf ->
            RevList (add_rev_list (List.rev buf) ls)
       | RevList buf ->
            RevList (add_rev_list buf ls)

let add_listbuf buf ls =
   let rec add_listbuf_rev buf = function
      elt :: ls ->
         elt :: (add_listbuf_rev buf ls)
    | [] ->
         buf
   in
   let add_listbuf_rev buf ls = RevList (add_listbuf_rev buf ls) in
      match buf, ls with
         Empty, _ ->
            ls
       | _, Empty ->
            buf
       | _, FwdList ls ->
            add_list buf ls
       | FwdList buf, RevList ls ->
            add_listbuf_rev (List.rev buf) ls
       | RevList buf, RevList ls ->
            add_listbuf_rev buf ls
