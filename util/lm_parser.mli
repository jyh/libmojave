(*
 * Parser generator.
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
open Lm_printf

val debug_parse    : bool ref
val debug_parsegen : bool ref

(*
 * Associativity.
 *)
type assoc =
   LeftAssoc
 | RightAssoc
 | NonAssoc

module type ParserArg =
sig
   (* Variable names: the names of terminals and nonterminals *)
   type symbol

   (* For debugging *)
   val to_string : symbol -> string
   val pp_print_symbol : out_channel -> symbol -> unit

   (* Sets and tables *)
   module SymbolSet : Lm_set_sig.LmSet with type elt = symbol;;
   module SymbolTable : Lm_map_sig.LmMap with type key = symbol;;

   (*
    * Semantic actions.
    * Values of action type *must* be comparable with =,
    * hopefully quickly.
    *
    * For example, functions are not allowed.
    * If you want a function, you should make an array of functions,
    * and use the index for the action name.
    *)
   type action

   (* For debugging *)
   val pp_print_action : out_channel -> action -> unit
end

module MakeParser (Arg : ParserArg) :
sig
   open Arg

   (* Grammar operations *)
   type t
   type precedence
   type ('a, 'b) lexer = 'a -> symbol * 'a * 'b
   type ('a, 'b) eval =
      'a ->                     (* The argument *)
      action ->                 (* The name of the action *)
      'b list ->                (* The arguments to the action *)
      'a * 'b                   (* The result of the semantic action *)

   (* The empty grammar accepts the empty language *)
   val empty          : t

   (*
    * Add a start symbol.  There can be more than one start symbol,
    * but parsing can only be performed for start variables.
    *)
   val add_start      : t -> symbol -> t

   (* Precedence control *)
   val prec_min       : precedence
   val prec_max       : precedence
   val create_prec_lt : t -> precedence -> assoc  -> t * precedence
   val create_prec_gt : t -> precedence -> assoc  -> t * precedence
   val add_prec       : t -> precedence -> symbol -> t
   val find_prec      : t -> symbol -> precedence

   (* Add a production *)
   val add_production :
      t ->                      (* The initial grammar *)
      action ->                 (* The name of the semantic action *)
      symbol ->                 (* The left-hand-side of the production *)
      symbol list ->            (* The right-hand-side of the production *)
      symbol option ->          (* Optional precedence is the same as that of the symbol *)
      t

   (* Delete a production based on the name of the semantic action *)
   val remove_production :
      t ->                      (* The initial grammar *)
      action ->                 (* The name of the semantic action *)
      t

   (* Now the actual machine *)
   val parse :
      t ->                      (* The machine *)
      symbol ->                 (* The start symbol *)
      ('a, 'b) lexer ->         (* The lexer *)
      ('a, 'b) eval ->          (* The semantic action evaluator *)
      'a ->                     (* The argument *)
      'a * 'b                   (* The result *)
end

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
