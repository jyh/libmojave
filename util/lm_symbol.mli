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

(*
 * Representation of symbols.
 *)
type symbol
type var = symbol

(*
 * Debugging adds extra qualifiers to new symbols.
 *)
val debug_symbol : bool ref

(*
 * Add a symbol to the table.
 *)
val add : string -> symbol
val is_interned : symbol -> bool
val reintern : symbol -> symbol

(*
 * Adding for debugging.
 * Don't use this normally.
 *)
val debug_add : string -> symbol

(*
 * Make a new symbol.
 *)
val new_symbol : symbol -> symbol
val new_symbol_pre : string -> symbol -> symbol
val new_symbol_string : string -> symbol

(*
 * Find a symbol for which the predicate is false.
 *)
val new_name : symbol -> (symbol -> bool) -> symbol

(*
 * Get back the string.
 *)
val to_int : symbol -> int
val to_string : symbol -> string
val string_of_symbol : symbol -> string
val string_of_ext_symbol : symbol -> string

(*
 * Printer uses Format library.
 *)
val pp_print_symbol : Format.formatter -> symbol -> unit
val pp_print_symbol_list : Format.formatter -> symbol list -> unit
val pp_print_ext_symbol : Format.formatter -> symbol -> unit

(*
 * This printer uses printf.
 *)
val print_symbol : out_channel -> symbol -> unit
val print_symbol_list : out_channel -> symbol list -> unit

(*
 * Compare two symbols for equality.
 *)
val eq : symbol -> symbol -> bool

(*
 * Ordered comparisons of symbols.
 *)
val compare : symbol -> symbol -> int

(*
 * Ordered comparisons of symbol pairs.
 *)
val compare_pair : symbol * symbol -> symbol * symbol -> int

(*
 * Ordered comparisons of symbol triples.
 *)
val compare_triple : symbol * symbol * symbol -> symbol * symbol * symbol -> int

(*
 * We also provide a function to produce a unique integer.
 *)
val new_number : unit -> int

(*
 * This table provides associations between symbols
 * and values.
 *)
module SymbolSet : Lm_set.LmSet with type elt = symbol
module SymbolTable : Lm_map.LmMap with type key = symbol
module SymbolMTable : Lm_map.LmMapList with type key = symbol
module SymbolIndex : Lm_index.McIndex with type key = symbol

module SymbolPairSet : Lm_set.LmSet with type elt = symbol * symbol
module SymbolPairTable : Lm_map.LmMap with type key = symbol * symbol
module SymbolPairMTable : Lm_map.LmMapList with type key = symbol * symbol
module SymbolPairIndex : Lm_index.McIndex with type key = symbol * symbol

module SymbolTripleSet : Lm_set.LmSet with type elt = symbol * symbol * symbol
module SymbolTripleTable : Lm_map.LmMap with type key = symbol * symbol * symbol
module SymbolTripleMTable : Lm_map.LmMapList with type key = symbol * symbol * symbol
module SymbolTripleIndex : Lm_index.McIndex with type key = symbol * symbol * symbol

(*
 * Symbol lists.
 *)
module SymbolListSet : Lm_set.LmSet with type elt = symbol list
module SymbolListTable : Lm_map.LmMap with type key = symbol list

(*
 * -*-
 * Local Variables:
 * Caml-master: "set"
 * End:
 * -*-
 *)
