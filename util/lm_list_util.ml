(*
 * Extra utilities for lists.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 1999 Jason Hickey, Caltech
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

(*
 * Iterate over two lists, but stop at the end of the
 * shorter one.
 *)
let rec short_iter2 f l1 l2 =
   match l1, l2 with
      h1 :: l1, h2 :: l2 ->
         f h1 h2;
         short_iter2 f l1 l2
    | _ ->
         ()

let rec iter3 f l1 l2 l3 =
   match l1, l2, l3 with
      h1 :: l1, h2 :: l2, h3 :: l3 ->
         f h1 h2 h3;
         iter3 f l1 l2 l3
    | [], [], [] ->
         ()
    | _ ->
         raise (Invalid_argument "iter3")

(*
 * Work left-to-right, but reverse the result.
 *)
let rec rev_map' f l = function
   h :: t ->
      rev_map' f (f h :: l) t
 | [] ->
      l

let rev_map f l =
   rev_map' f [] l

(*
 * Generalize fold_left over three lists.
 *)
let rec fold_left3 f arg l1 l2 l3 =
   match l1, l2, l3 with
      h1 :: t1, h2 :: t2, h3 :: t3 ->
         fold_left3 f (f arg h1 h2 h3) t1 t2 t3
    | [], [], [] ->
         arg
    | _ ->
         raise (Invalid_argument "fold_left3")

(*
 * Find the index of an element in a list.
 *)
let find_index eq x l =
   let rec search i = function
      h :: t ->
         if eq x h then
            i
         else
            search (succ i) t
    | [] ->
         raise Not_found
   in
      search 0 l

(*
 * Split the last element from the rest.
 *)
let split_last =
   let rec split l = function
      [h] ->
         List.rev l, h
    | h :: t ->
         split (h :: l) t
    | [] ->
         raise (Invalid_argument "split_last")
   in
      (fun l -> split [] l)

(*
 * Split based on an index.
 *)
let rec split i l =
   if i = 0 then
      [], l
   else
      match l with
         h :: t ->
            let l1, l2 = split (pred i) t in
               h :: l1, l2
       | [] ->
            raise (Invalid_argument "split")

(*
 * Get the last element.
 *)
let rec last = function
   [x] -> x
 | _ :: t -> last t
 | [] -> raise (Invalid_argument "last")

(*
 * Association list with an equality.
 *)
let rec assoc_eq eq x = function
   (x', y) :: t ->
      if eq x x' then
         y
      else
         assoc_eq eq x t
 | [] ->
      raise Not_found

(*
 * Return the first n elements of the list.
 *)
let rec firstn i l =
   if i = 0 then
      []
   else
      match l with
         h :: t ->
            h :: firstn (pred i) t
       | [] ->
            []

(*
 * Nth tail.
 *)
let rec nth_tl i l =
   if i = 0 then
      l
   else
      match l with
         _ :: l ->
            nth_tl (pred i) l
       | [] ->
            raise (Invalid_argument "nth_tl")

(*
 * Remove an element from a list.
 *)
let rec remove k = function
   h :: t ->
      if k = h then
         remove k t
      else
         h :: remove k t
 | [] ->
      []

(*
 * Replace the nth element of the list.
 *)
let rec replace_nth i x l =
   match l with
      [] ->
         raise (Invalid_argument "replace_nth")
    | h :: l ->
         if i = 0 then
            x :: l
         else
            h :: replace_nth (pred i) x l

(*
 * Apply a function to the key in an assoc list.
 *)
let rec apply_assoc v l f =
   match l with
      (v', k) as h :: t ->
         if v' = v then
            (v', f k) :: t
         else
            h :: apply_assoc v t f
    | [] ->
         raise Not_found

(*
 * Add to an association list.
 *)
let add_assoc k v l =
   let rec replace = function
      (k', _) as h :: l ->
         if k' = k then
            (k', v) :: l
         else
            h :: replace l
    | [] ->
         raise Not_found
   in
      try replace l with
         Not_found ->
            (k, v) :: l

(*
 * Subtraction.
 *)
let rec subtract_list l1 l2 =
   match l1 with
      h :: l1 ->
         if List.mem h l2 then
            subtract_list l1 l2
         else
            h :: subtract_list l1 l2
    | [] ->
         []

(*
 * map2to1
 *)
let rec map2to1 f l1 l2 =
  match l1, l2 with
      [], [] -> []
    | h1::l1, h2::l2 ->
	f h1 h2 :: map2to1 f l1 l2
    | _ -> raise (Invalid_argument "map2to1")

(*
 * fold_left + map = fold_map
 *)
let rec fold_map f i l =
  match l with
      [] -> i, []
    | hd :: tl ->
	let i, hd = f i hd in
	let i, l = fold_map f i tl in
	  i, hd :: l

let rec fold_map2to1 f i l1 l2 =
  match l1, l2 with
      [], [] -> i, []
    | h1 :: l1, h2 :: l2 ->
	let i, h = f i h1 h2 in
	let i, l = fold_map2to1 f i l1 l2 in
	  i, h :: l
    | _ -> raise (Invalid_argument "fold_map2to1")

let rec fold_map1to2 f i l =
  match l with
      [] -> i, [], []
    | hd :: tl ->
	let i, hd1, hd2 = f i hd in
	let i, l1, l2 = fold_map1to2 f i tl in
	  i, hd1 :: l1, hd2 :: l2

(*
 * fold + filter = fold_filter
 *)
let rec fold_filter f x l =
  match l with
      [] -> x, []
    | h :: t ->
        let x, b = f x h in
          if b then
            let x, t = fold_filter f x t in
              x, h :: t
          else
            fold_filter f x t

(*
 * check list lengths equal
 *)
let rec length_eq l1 l2 =
  match l1, l2 with
      [], [] -> true
    | _ :: l1, _ :: l2 -> length_eq l1 l2
    | _ -> false

(*
 * Set operations.
 *)
let rec union l1 l2 =
   match l1 with
      x :: l1 ->
         if List.mem x l2 then
            union l1 l2
         else
            x :: union l1 l2
    | [] ->
         l2

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
