(*
 * Right now the symbol table is just a representation of strings.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 1999-2002 Jason Hickey, Caltech
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
 *
 * ----------------------------------------------------------------
 * Revision History
 *
 *  2002  Dec  4  Michael Maire  Added SymbolIndex
 *                               Added sets, tables, indices for
 *                               symbol pairs and triples
 *)
open Format

open Lm_debug

let debug_symbol = ref false

(*
 * We use a hashtable to manage symbols we have seen.
 *)
type symbol = int * string

type t =
   { hash : (string, symbol) Hashtbl.t;
     mutable count : int
   }

(*
 * A new symbol table.
 *)
let table =
   { hash = Hashtbl.create 19;
     count = 100
   }

let new_number () =
   let index = succ table.count in
      table.count <- index;
      index

(*
 * Mangle a string so it uses printable characters.
 *)
let is_special s =
   let len = String.length s in
   let rec search i =
      if i = len then
         false
      else
         match s.[i] with
            'a'..'z'
          | 'A'..'Z'
          | '0'..'9'
          | '_'
          | '.'
          | '%' ->
               search (succ i)
          | _ ->
               true
   in
      search 0

let rec buffer_mangle buf s i len =
   if len <> 0 then
      let c = s.[i] in
      let _ =
         match c with
            'a'..'z'
          | 'A'..'Z'
          | '0'..'9'
          | '_' ->
               Buffer.add_char buf c
          | _ ->
               Buffer.add_char buf '.';
               Buffer.add_string buf (string_of_int (Char.code c))
      in
         buffer_mangle buf s (succ i) (pred len)

let mangle s =
   let len = String.length s in
   let buf = Buffer.create len in
      buffer_mangle buf s 0 len;
      Buffer.contents buf


(*
 * Add a symbol to the table.
 *)
let stop s =
   eprintf "Bogus symbol %s@." s;
   false

let add s =
   (* assert (if (is_special s) then stop s else true); *)
   try Hashtbl.find table.hash s with
      Not_found ->
         let index = succ table.count in
         let symbol = index, s in
            table.count <- index;
            Hashtbl.add table.hash s symbol;
            symbol

let add_mangle s =
   add (mangle s)

let reintern (_, s) =
   add s

(*
 * Add a debugging symbol.
 *)
let debug_add s =
   try
      let len = String.length s in
      let i = String.rindex s '_' in
      let j = int_of_string (String.sub s (i + 1) (len - i - 1)) in
      let s = String.sub s 0 i in
         j, s
   with
      Not_found
    | Failure _ ->
         add s

(*
 * Get the symbol name.
 *)
let to_string (_, s) =
   s

let to_int (i, _) =
   i

(*
 * Create a new symbol.
 * Don't add it to the table.
 *)
let new_symbol_string s =
   (* assert (if is_special s then stop s else true); *)
   let i = succ table.count in
      table.count <- i;
      i, s

let new_symbol (_, v) =
   new_symbol_string v

let new_symbol_pre pre (_, v) =
   let s =
      if debug debug_symbol then
         v ^ "/" ^ pre
      else
         v
   in
      new_symbol_string s

(*
 * Check if the symbol is in the table.
 *)
let is_interned (i, s) =
   try
      let i', _ = Hashtbl.find table.hash s in
         i' = i
   with
      Not_found ->
         false

(*
 * Printer.
 * If the symbol is not a defined symbol,
 * print the index.
 *)
let string_of_symbol (i, s) =
   try
      let i', _ = Hashtbl.find table.hash s in
         if i' = i then
            s
         else
            Printf.sprintf "%s_%05d" s i
   with
      Not_found ->
         Printf.sprintf "%s_%05d" s i

let pp_print_symbol buf v =
   Format.pp_print_string buf (string_of_symbol v)

(*
 * Print extended symbols. Used in FIR printing.
 *)
exception Has

let string_of_ext_symbol (i, s) =
   let has_special_char s =
      try
         for i = 0 to String.length s - 1 do
            let c = Char.lowercase (String.get s i) in
               if not ((Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z')
               || (Char.code c >= Char.code '0' && Char.code c <= Char.code '9')
               || c = '_') then
                  raise Has
         done;
         false
      with
         Has ->
            true
   in
   let s =
      try
         let i', _ = Hashtbl.find table.hash s in
            if i' = i then
               s
            else
               Printf.sprintf "%s_%05d" s i
      with
         Not_found ->
            Printf.sprintf "%s_%05d" s i
   in
      if has_special_char s then
         Printf.sprintf "`\"%s\"" s
      else
         s

let pp_print_ext_symbol buf v =
   Format.pp_print_string buf (string_of_ext_symbol v)

(*
 * Compare for equality.
 *)
let eq (i1, _) (i2, _) =
   i1 = i2

let compare (i1, _) (i2, _) =
   i1 - i2

(*
 * Compare pair of symbols for equality.
 *)
let compare_pair (s1, s2) (s1', s2') =
   let res = compare s1 s1' in
   if (res = 0) then
      compare s2 s2'
   else
      res

(*
 * Compare triple of symbols for equality.
 *)
let compare_triple (s1, s2, s3) (s1', s2', s3') =
   let res = compare_pair (s1, s2) (s1, s2') in
   if (res = 0) then
      compare s3 s3'
   else
      res

(*
 * Build sets, tables, indices where the keys are symbols,
 * ordered symbol pairs, or orderd symbol triples.
 *)
module Base =
struct
   type t = symbol
   let compare = compare
end

module PairBase =
struct
   type t = symbol * symbol
   let compare = compare_pair
end

module TripleBase =
struct
   type t = symbol * symbol * symbol
   let compare = compare_triple
end

module SymbolSet = Lm_set.LmMake (Base)
module SymbolTable = Lm_map.LmMake (Base)
module SymbolMTable = Lm_map.LmMakeList (Base)
module SymbolIndex = Lm_index.LmMake (Base)

module SymbolPairSet = Lm_set.LmMake (PairBase)
module SymbolPairTable = Lm_map.LmMake (PairBase)
module SymbolPairMTable = Lm_map.LmMakeList (PairBase)
module SymbolPairIndex = Lm_index.LmMake (PairBase)

module SymbolTripleSet = Lm_set.LmMake (TripleBase)
module SymbolTripleTable = Lm_map.LmMake (TripleBase)
module SymbolTripleMTable = Lm_map.LmMakeList (TripleBase)
module SymbolTripleIndex = Lm_index.LmMake (TripleBase)

(*
 * Symbol lists are also useful.
 *)
module SymbolListCompare =
struct
   type t = symbol list

   let rec compare l1 l2 =
      match l1, l2 with
         v1 :: l1, v2 :: l2 ->
            let cmp = Base.compare v1 v2 in
               if cmp = 0 then
                  compare l1 l2
               else
                  cmp
       | [], _ :: _ ->
            -1
       | _ :: _, [] ->
            1
       | [], [] ->
            0
end

module SymbolListSet = Lm_set.LmMake (SymbolListCompare)
module SymbolListTable = Lm_map.LmMake (SymbolListCompare)

(*
 * -*-
 * Local Variables:
 * Caml-master: "set"
 * End:
 * -*-
 *)
