(*
 * Generic parser generator.
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
open Lm_debug
open Lm_printf
open Lm_location

let debug_parse =
   create_debug (**)
      { debug_name = "debug-parse";
        debug_description = "Debug the parseer";
        debug_value = false
      }

let debug_parsegen =
   create_debug (**)
      { debug_name = "debug-parsegen";
        debug_description = "Debug the parseer generator";
        debug_value = false
      }

(*
 * Helper modules.
 *)
module IntCompare =
struct
   type t = int
   let compare = (-)
end

module IntSet    = Lm_set.LmMake     (IntCompare);;
module IntTable  = Lm_map.LmMake     (IntCompare);;
module IntMTable = Lm_map.LmMakeList (IntCompare);;

(*
 * A precedence directive is left-associative, right-associative,
 * or nonassociative.
 *)
type assoc =
   LeftAssoc
 | RightAssoc
 | NonAssoc

let pp_print_assoc buf assoc =
   let s =
      match assoc with
         LeftAssoc ->
            "left "
       | RightAssoc ->
            "right"
       | NonAssoc ->
            "nona "
   in
      pp_print_string buf s

module type PrecedenceArg =
sig
   type t
   type precedence

   (* Precedence control *)
   val prec_min       : precedence
   val prec_max       : precedence

   (* Precedence tables *)
   val empty          : t
   val create_prec_lt : t -> precedence -> assoc  -> t * precedence
   val create_prec_gt : t -> precedence -> assoc  -> t * precedence

   (* Print a precedence *)
   val pp_print_prec  : t -> out_channel -> precedence -> unit

   (* Comparison *)
   val add_assoc      : t -> precedence -> assoc -> t
   val assoc          : t -> precedence -> assoc
   val compare        : t -> precedence -> precedence -> int
end

exception ParseError of loc

(*
 * The parser is parameterized over symbol and action names.
 *)
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

   (* Names of semantic actions *)
   type action

   (* For debugging *)
   val pp_print_action : out_channel -> action -> unit
end

module MakeParser (Arg : ParserArg) (Precedence : PrecedenceArg) =
struct
   open Arg
   open Precedence

   (************************************************************************
    * Types.
    *)

   (*
    * Type of lexing tokens.
    *)
   type ('a, 'b) lexer = 'a -> symbol * loc * 'a * 'b
   type ('a, 'b) eval =
      'a ->                     (* The argument *)
      action ->                 (* The name of the action *)
      loc ->                    (* Location of the production *)
      'b list ->                (* The arguments to the action *)
      'a * 'b                   (* The result of the semantic action *)

   (*
    * Internally, we represent variables with integers.
    *)
   type var = int

   module VarCompare =
   struct
      type t = var
      let compare = (-)
   end

   module VarSet    = IntSet;;
   module VarTable  = IntTable;;
   module VarMTable = IntMTable;;

   (*
    * EOF is a special var.
    * Have to fix this.
    *)
   let eof_var     = -1

   (************************************************
    * The grammar.
    *)

   (*
    * A production has:
    *    prod_action : the name of the production semantic action
    *    prod_name   : the nonterminal
    *    prod_rhs    : the right-hand-side
    *    prod_prec   : the precedence
    *)
   type prod =
      { prod_action  : action;
        prod_name    : var;
        prod_rhs     : var list;
        prod_prec    : precedence
      }

   (*
    * A grammar has a set of symbols, productions,
    * and precedences.
    *
    * The precedences have a level of indirection.
    *)
   type grammar =
     { gram_var_of_symbol : var SymbolTable.t;
       gram_symbol_of_var : symbol VarTable.t;
       gram_prod          : prod VarMTable.t;
       gram_prec          : precedence VarTable.t;
       gram_prec_table    : Precedence.t;
       gram_start_symbols : SymbolSet.t
     }

   (************************************************
    * The PDA.
    *)

   (*
    * An action is shift, reduce, or accept.
    *)
   type pda_action =
      ReduceAction of action * var * int   (* semantic action, production name, #args *)
    | ShiftAction  of int                  (* goto state *)
    | GotoAction   of int
    | AcceptAction

   (*
    * We may reduce states without lookahead,
    * and we may accept.
    *)
   type pda_reduce =
      ReduceNone
    | ReduceNow of action * var * int
    | ReduceAccept of action * var * int

   (*
    * The PDA transition table.
    *)
   type pda_state =
      { pda_delta   : pda_action VarTable.t;
        pda_reduce  : pda_reduce
      }

   type pda =
      { pda_start_states  : int SymbolTable.t;
        pda_var_of_symbol : var SymbolTable.t;
        pda_symbol_of_var : symbol VarTable.t;
        pda_states        : pda_state array
      }

   (*
    * Run time info.
    *)
   type ('a, 'b) run =
      { run_var_of_symbol : var SymbolTable.t;
        run_symbol_of_var : symbol VarTable.t;
        run_states        : pda_state array;
        run_lexer         : ('a, 'b) lexer;
        run_eval          : ('a, 'b) eval
      }

   (*
    * The actual machine has a grammar and an optional pda.
    *)
   type t =
      { parse_grammar     : grammar;
        mutable parse_pda : pda option
      }

   (************************************************
    * Building the PDA.
    *)

   (*
    * A production item is the production with position.
    * It does not include the lookahead.
    *
    * name ::= left . right
    *
    * We also keep the precedence of the production,
    * and its semantic action name.
    *)
   type prod_item =
      { prod_item_name   : var;
        prod_item_left   : var list;       (* Reverse order *)
        prod_item_right  : var list;
        prod_item_action : action;
        prod_item_prec   : precedence
      }

   module ProdItemCompare =
   struct
      type t = prod_item
      let compare = Pervasives.compare
   end

   module ProdItemSet   = Lm_set.LmMake (ProdItemCompare);;
   module ProdItemTable = Lm_map.LmMake (ProdItemCompare);;

   module ProdItemSetCompare =
   struct
      type t = ProdItemSet.t
      let compare = Pervasives.compare
   end

   module ProdItemSetTable = Lm_map.LmMake (ProdItemSetCompare);;

   (*
    * The lookahead is a set of variables.
    *)
   type lookahead = VarSet.t

   (*
    * A state element is a set of items, with lookaheads for each.
    *)
   and info_item =
      { info_item_index   : int;
        info_item_table   : lookahead ProdItemTable.t
      }

   (*
    * The info for constructing the PDA.
    *    info_gram     : the grammar
    *    info_nullable : the nonterminals that derive epsilon
    *    info_first    : the terminals that may start a production
    *)
   type info =
     { info_grammar             : grammar;
       info_nullable            : VarSet.t;
       info_first               : VarSet.t VarTable.t
     }

   (************************************************************************
    * Utilities.
    *)

   (*
    * Get the index of a symbol.
    * If the symbol is new, expand the tables.
    *)
   let var_of_symbol_exn gram sym =
      SymbolTable.find gram.gram_var_of_symbol sym

   let var_of_symbol gram v =
      let { gram_var_of_symbol = var_of_symbol;
            gram_symbol_of_var = symbol_of_var
          } = gram
      in
         try gram, SymbolTable.find var_of_symbol v with
            Not_found ->
               let index = SymbolTable.cardinal var_of_symbol in
               let gram =
                  { gram with gram_var_of_symbol = SymbolTable.add var_of_symbol v index;
                              gram_symbol_of_var = VarTable.add symbol_of_var index v
                  }
               in
                  gram, index

   let vars_of_symbols gram vl =
      let gram, vl =
         List.fold_left (fun (gram, vl) v ->
               let gram, v = var_of_symbol gram v in
                  gram, v :: vl) (gram, []) vl
      in
         gram, List.rev vl

   (*
    * Get the symbol for a var.
    *)
   let symbol_of_var gram v =
      VarTable.find gram.gram_symbol_of_var v

   (************************************************************************
    * Printing and errors.
    *)
   let pp_print_var gram buf v =
      if v = eof_var then
         pp_print_char buf '$'
      else
         pp_print_symbol buf (symbol_of_var gram v)

   let rec pp_print_vars gram buf vl =
      List.iter (fun v -> fprintf buf " %a" (pp_print_var gram) v) vl

   let pp_print_var_set gram buf s =
      VarSet.iter (fun v ->
            fprintf buf "@ %a" (pp_print_var gram) v) s

   let pp_print_var_table gram buf table =
      VarTable.iter (fun v s ->
            fprintf buf "@ @[<b 3>%a:%a@]" (**)
               (pp_print_var gram) v
               (pp_print_var_set gram) s) table

   let pp_print_prod gram buf prod =
      let { prod_action = action;
            prod_name   = name;
            prod_rhs    = rhs;
            prod_prec   = pre
          } = prod
      in
         fprintf buf "@[<hv 3>(production@ action: %a@ prec: %a@ target: %a@ sources:@ %a)@]" (**)
            pp_print_action action
            (Precedence.pp_print_prec gram.gram_prec_table) pre
            (pp_print_var gram) name
            (pp_print_vars gram) rhs

   let pp_print_grammar buf gram =
      let { gram_prod = prods;
            gram_prec = precs;
            gram_prec_table = prec_table;
            gram_start_symbols = starts
          } = gram
      in
         fprintf buf "@[<hv 3>Grammar:";
         VarTable.iter (fun v pre ->
               fprintf buf "@ prec %a = %a" (**)
                  (pp_print_var gram) v
                  (Precedence.pp_print_prec prec_table) pre) precs;
         SymbolSet.iter (fun v ->
               fprintf buf "@ start %a" pp_print_symbol v) starts;
         VarMTable.iter_all (fun _ prods ->
               List.iter (fun prod -> fprintf buf "@ %a" (pp_print_prod gram) prod) prods) prods;
         fprintf buf "@]"

   let pp_print_prod_item info buf item =
      let { prod_item_action = action;
            prod_item_prec   = pre;
            prod_item_name   = name;
            prod_item_left   = left;
            prod_item_right  = right
          } = item
      in
      let gram = info.info_grammar in
         fprintf buf "%a[%a]    %a ::=%a .%a" (**)
            pp_print_action action
            (Precedence.pp_print_prec gram.gram_prec_table) pre
            (pp_print_var gram) name
            (pp_print_vars gram) (List.rev left)
            (pp_print_vars gram) right

   let pp_print_pda_action buf action =
      match action with
         ReduceAction (action, _, _) ->
            fprintf buf "reduce %a" pp_print_action action
       | ShiftAction id ->
            fprintf buf "shift %d" id
       | GotoAction id ->
            fprintf buf "goto %d" id
       | AcceptAction  ->
            pp_print_string buf "accept"

   let pp_print_pda_actions info buf actions =
      let pp_print_var = pp_print_var info.info_grammar in
         IntTable.iter (fun v action ->
               fprintf buf "@ %a: %a" pp_print_var v pp_print_pda_action action) actions

   let pp_print_closure info buf closure =
      let gram = info.info_grammar in
         fprintf buf "@[<v 3>Closure:";
         ProdItemTable.iter (fun prod_item lookahead ->
               if VarSet.is_empty lookahead then
                  fprintf buf "@ @[<hv 3>%a@]" (pp_print_prod_item info) prod_item
               else
                  VarSet.iter (fun v ->
                        fprintf buf "@ @[<hv 3>%a # %a@]" (pp_print_prod_item info) prod_item (pp_print_var gram) v) lookahead) closure;
         fprintf buf "@]"

   let pp_print_info_item info buf info_item =
      let gram = info.info_grammar in
      let { info_item_index = index;
            info_item_table = table
          } = info_item
      in
         fprintf buf "@[<v 3>State %d:" index;
         ProdItemTable.iter (fun prod_item lookahead ->
               fprintf buf "@ %a @[<b 2>#%a@]" (pp_print_prod_item info) prod_item (pp_print_var_set gram) lookahead) table;
         fprintf buf "@]"

   let pp_print_info buf info =
      let { info_grammar = gram;
            info_nullable = nullable;
            info_first = first
          } = info
      in
         fprintf buf "@[<v 0>%a" pp_print_grammar gram;
         fprintf buf "@ @[<b 3>Nullable:%a@]" (pp_print_var_set gram) nullable;
         fprintf buf "@ @[<hv 3>First:%a@]" (pp_print_var_table gram) first;
         fprintf buf "@]"

   (*
    * Error messages.
    *)
   let shift_reduce_conflict info v id prod_item =
      let gram = info.info_grammar in
         eprintf "shift/reduce conflict on %a: shift %d, reduce %a@." (**)
            (pp_print_var gram) v
            id
            pp_print_action prod_item.prod_item_action

   let reduce_reduce_conflict info v action1 action2 =
      let gram = info.info_grammar in
         eprintf "reduce/reduce conflict on %a: reduce %a, reduce %a@." (**)
            (pp_print_var gram) v
            pp_print_action action1
            pp_print_action action2

   (************************************************************************
    * Grammar construction.
    *)

   (*
    * Empty grammar has the basic precedences.
    *)
   let empty_grammar =
      { gram_var_of_symbol = SymbolTable.empty;
        gram_symbol_of_var = VarTable.empty;
        gram_prod          = VarMTable.empty;
        gram_prec          = VarTable.empty;
        gram_prec_table    = Precedence.empty;
        gram_start_symbols = SymbolSet.empty
      }

   (*
    * Add a start symbol.
    *)
   let add_start gram sym =
      { gram with gram_start_symbols = SymbolSet.add gram.gram_start_symbols sym }

   (*
    * Add a symbol at a given precedence level.
    *)
   let add_prec gram pre sym =
      let gram, v = var_of_symbol gram sym in
         { gram with gram_prec = VarTable.add gram.gram_prec v pre }

   (*
    * Find the precedence level for a symbol.
    *)
   let find_prec gram sym =
      let v = var_of_symbol_exn gram sym in
         VarTable.find gram.gram_prec v

   (*
    * Add a production.
    * If the precedence is not specified, it is the precedence
    * of the rightmost variable that has a precedence.
    *)
   let add_production gram action v rhs pre =
      let gram, v   = var_of_symbol gram v in
      let gram, rhs = vars_of_symbols gram rhs in
      let pre =
         match pre with
            Some sym ->
               find_prec gram sym
          | None ->
               List.fold_left (fun pre v ->
                     try VarTable.find gram.gram_prec v with
                        Not_found ->
                           pre) prec_min rhs
      in
      let prod =
         { prod_action  = action;
           prod_name    = v;
           prod_rhs     = rhs;
           prod_prec    = pre
         }
      in
         { gram with gram_prod = VarMTable.add gram.gram_prod v prod }

   (*
    * Remove a production.
    * We don't index by production name, so this takes linear time
    * in the number of productions.
    *)
   let remove_production gram action =
      let table =
         VarMTable.mapi_all (fun _ prods ->
               List.filter (fun prod -> prod.prod_action <> action) prods) gram.gram_prod
      in
         { gram with gram_prod = table }

   (************************************************************************
    * Initial PDA construction.
    *)

   (*
    * A nonterminal is nullable if all variables on the rhs are nullable.
    *)
   let nullable gram =
      let step nullable prods =
         VarMTable.fold_all (fun nullable v prods ->
               if VarSet.mem nullable v
                  || List.exists (fun prod -> List.for_all (VarSet.mem nullable) prod.prod_rhs) prods
               then
                  nullable
               else
                  VarSet.add nullable v) nullable prods
      in
      let rec fixpoint nullable prods =
         let nullable' = step nullable prods in
            if VarSet.cardinal nullable' <> VarSet.cardinal nullable then
               fixpoint nullable' prods
            else
               nullable
      in
         fixpoint VarSet.empty gram.gram_prod

   (*
    * Find the sets of first symbols that can start productions.
    *)
   let rec first_rhs nullable first set rhs =
      match rhs with
         v :: rhs ->
            if VarSet.mem nullable v then
               first_rhs nullable first set rhs
            else
               VarSet.union set (VarTable.find first v)
       | [] ->
            set

   let first gram nullable =
      let step first prods =
         VarMTable.fold_all (fun (first, changed) v prods ->
               List.fold_left (fun (first, changed) prod ->
                     let { prod_name = x;
                           prod_rhs = rhs
                         } = prod
                     in
                     let set = VarTable.find first x in
                     let set' = first_rhs nullable first set rhs in
                     let set, changed =
                        if changed || VarSet.cardinal set' <> VarSet.cardinal set then
                           set', true
                        else
                           set, false
                     in
                     let first = VarTable.add first x set in
                        first, changed) (first, changed) prods) (first, false) prods
      in
      let rec fixpoint first prods =
         let first, changed = step first prods in
            if changed then
               fixpoint first prods
            else
               first
      in

      (* Initialize with the terminals *)
      let { gram_symbol_of_var = vars;
            gram_prod = prods
          } = gram
      in
      let first =
         VarTable.fold (fun first v _ ->
               if VarMTable.mem prods v then
                  VarTable.add first v VarSet.empty
               else
                  VarTable.add first v (VarSet.singleton v)) VarTable.empty vars
      in
         fixpoint first prods

   (*
    * Build the grammar.
    *)
   let info_of_grammar gram =
      let nullable = nullable gram in
      let first = first gram nullable in
         { info_grammar     = gram;
           info_nullable    = nullable;
           info_first       = first
         }

   (************************************************************************
    * PDA construction.
    *)

   (*
    * Build a prod_item from a production.
    *)
   let prod_item_of_prod prod =
      let { prod_action = action;
            prod_name   = name;
            prod_rhs    = rhs;
            prod_prec   = pre
          } = prod
      in
         { prod_item_action = action;
           prod_item_prec   = pre;
           prod_item_name   = name;
           prod_item_left   = [];
           prod_item_right  = rhs
         }

   (*
    * Get the set of first symbols that can being a list.
    *)
   let rec first_list nullable first rhs lookahead =
      match rhs with
         v :: rhs ->
            if VarSet.mem nullable v then
               first_list nullable first rhs lookahead
            else
               VarTable.find first v
       | [] ->
            lookahead

   (*
    * Take the union of a state and a closure.
    * This just means take the union of their lookahead sets.
    *)
   let info_item_union info_item closure =
      let table = info_item.info_item_table in
      let table, changed =
         ProdItemTable.fold (fun (table, changed) item lookahead1 ->
               let lookahead2 = VarSet.union lookahead1 (ProdItemTable.find closure item) in
                  if VarSet.cardinal lookahead1 = VarSet.cardinal lookahead2 then
                     table, changed
                  else
                     ProdItemTable.add table item lookahead2, true) (table, false) table
      in
         { info_item with info_item_table = table }, changed

   (*
    * Take the closure of a production.
    *)
   let closure info (table : lookahead ProdItemTable.t) =
      let { info_grammar  = { gram_prod = prods };
            info_nullable = nullable;
            info_first    = first
          } = info
      in
      let step closure =
         ProdItemTable.fold (fun (closure, changed) prod_item lookahead ->
               match prod_item with
                  { prod_item_right = v :: rest } ->
                     let prods =
                        try VarMTable.find_all prods v with
                           Not_found ->
                              []
                     in
                        List.fold_left (fun (closure, changed) prod ->
                              let { prod_name = name;
                                    prod_rhs = rhs
                                  } = prod
                              in
                              let lookahead = first_list nullable first rest lookahead in
                              let prod_item = prod_item_of_prod prod in
                              let lookahead, changed =
                                 try
                                    let lookahead1 = ProdItemTable.find closure prod_item in
                                    let lookahead2 = VarSet.union lookahead lookahead1 in
                                       if VarSet.cardinal lookahead1 = VarSet.cardinal lookahead2 then
                                          lookahead1, changed
                                       else
                                          lookahead2, true
                                 with
                                    Not_found ->
                                       lookahead, true
                              in
                              let closure = ProdItemTable.add closure prod_item lookahead in
                                 closure, changed) (closure, changed) prods
                | { prod_item_right = [] } ->
                     closure, changed) (closure, false) closure
      in
      let rec fixpoint closure =
         let closure', changed = step closure in
            if changed then
               fixpoint closure'
            else
               closure
      in
         fixpoint table

   (*
    * Add the state identified by the closure to the set
    * of know states.
    *
    * This is the LALR step: merge the entry with whatever entry
    * already exists.
    *)
   let add_state examined unexamined closure =
      let key =
         ProdItemTable.fold (fun key item _ ->
               ProdItemSet.add key item) ProdItemSet.empty closure
      in
         try
            let info_item = ProdItemSetTable.find examined key in
            let info_item, changed = info_item_union info_item closure in
               if changed then
                  info_item, ProdItemSetTable.remove examined key, ProdItemSetTable.add unexamined key info_item
               else
                  info_item, examined, unexamined
         with
            Not_found ->
               try
                  let info_item = ProdItemSetTable.find unexamined key in
                  let info_item, _ = info_item_union info_item closure in
                  let unexamined = ProdItemSetTable.add unexamined key info_item in
                     info_item, examined, unexamined
               with
                  Not_found ->
                     let info_item =
                        { info_item_index = ProdItemSetTable.cardinal examined + ProdItemSetTable.cardinal unexamined;
                          info_item_table = closure
                        }
                     in
                     let unexamined = ProdItemSetTable.add unexamined key info_item in
                        info_item, examined, unexamined

   (*
    * Figure out all the symbols on which we can make a transition
    * by shifting.
    *)
   let shift_symbols info_item =
      ProdItemTable.fold (fun syms prod_item _ ->
            match prod_item.prod_item_right with
               v :: _ ->
                  VarSet.add syms v
             | [] ->
                  syms) VarSet.empty info_item.info_item_table

   (*
    * Perform the shift by a symbol.
    *)
   let shift_info_item info_item v =
      ProdItemTable.fold (fun table prod_item lookahead ->
            let { prod_item_left = left;
                  prod_item_right = right
                } = prod_item
            in
               match right with
                  v' :: right when v' = v ->
                     let prod_item =
                        { prod_item with prod_item_left = v :: left;
                                         prod_item_right = right
                        }
                     in
                        ProdItemTable.add table prod_item lookahead
                | _ ->
                     table) ProdItemTable.empty info_item.info_item_table

   (*
    * Compute the transition table, only for shift operations.
    *)
   let rec shift_closure info shift_table examined unexamined =
      let { info_grammar = { gram_prod = prods } } = info in
         if ProdItemSetTable.is_empty unexamined then
            shift_table, examined
         else
            (* Move an item from unexamined to examined *)
            let key, info_item = ProdItemSetTable.choose unexamined in
            let examined = ProdItemSetTable.add examined key info_item in
            let unexamined = ProdItemSetTable.remove unexamined key in

            (* For each of the shift symbols, add a shift operation *)
            let syms = shift_symbols info_item in
            let shift, examined, unexamined =
               VarSet.fold (fun (shift, examined, unexamined) v ->
                     let table = shift_info_item info_item v in
                     let closure = closure info table in
                     let info_item, examined, unexamined = add_state examined unexamined closure in
                     let id = info_item.info_item_index in
                     let shift =
                        if VarMTable.mem prods v then
                           VarTable.add shift v (GotoAction id)
                        else
                           VarTable.add shift v (ShiftAction id)
                     in
                        shift, examined, unexamined) (VarTable.empty, examined, unexamined) syms
            in
            let id = info_item.info_item_index in
            let shift_table = IntTable.add shift_table id shift in
               shift_closure info shift_table examined unexamined

   (*
    * If a state has only one production,
    * and that is a reduce production, we can do
    * the reduce without lookahead.
    *)
   let reduce_early table =
      if ProdItemTable.cardinal table = 1 then
         match ProdItemTable.choose table with
            { prod_item_right = [];
              prod_item_action = action;
              prod_item_name = name;
              prod_item_left = left
            }, lookahead ->
               if VarSet.cardinal lookahead = 1 && VarSet.choose lookahead = eof_var then
                  ReduceAccept (action, name, List.length left)
               else
                  ReduceNow (action, name, List.length left)
          | _ ->
               ReduceNone
      else
         ReduceNone

   (*
    * Found a reduce action, resolve conflicts.
    *)
   let reduce_action info actions prod_item lookahead =
      let { info_grammar = { gram_prec = var_prec_table;
                             gram_prec_table = prec_table
                           }
          } = info
      in
      let { prod_item_name   = name;
            prod_item_action = action;
            prod_item_left   = left;
            prod_item_prec   = prec_name
          } = prod_item
      in
      let assoc = Precedence.assoc prec_table prec_name in
      let reduce = ReduceAction (action, name, List.length left) in
         VarSet.fold (fun actions v ->
               try
                  match VarTable.find actions v with
                     ShiftAction id
                   | GotoAction id ->
                        (* Shift/reduce conflict *)
                        let cmp =
                           try Precedence.compare prec_table prec_name (VarTable.find var_prec_table v) with
                              Not_found ->
                              0
                        in
                           if cmp < 0 then
                              actions
                           else if cmp = 0 then
                              match assoc with
                                 LeftAssoc ->
                                    VarTable.add actions v reduce
                               | RightAssoc ->
                                    actions
                               | NonAssoc ->
                                    shift_reduce_conflict info v id prod_item;
                                    actions
                           else
                              VarTable.add actions v reduce
                   | ReduceAction (action2, _, _) ->
                        reduce_reduce_conflict info v action action2;
                        actions
                   | AcceptAction ->
                        raise (Invalid_argument "reduce_action")
               with
                  Not_found ->
                     VarTable.add actions v reduce) actions lookahead

   (*
    * Compute the reduce actions.
    *)
   let reduce info trans_table states =
      ProdItemSetTable.fold (fun trans_table _ info_item ->
            let { info_item_index = index;
                  info_item_table = prod_items
                } = info_item
            in
            let actions = IntTable.find trans_table index in
            let actions =
               ProdItemTable.fold (fun actions prod_item lookahead ->
                     match prod_item.prod_item_right with
                        [] ->
                           reduce_action info actions prod_item lookahead
                      | _ ->
                           actions) actions prod_items
            in
               if !debug_parsegen then
                  eprintf "@[<v 0>%a@ @[<v 3>%a@]@ @]@." (**)
                     (pp_print_info_item info) info_item
                     (pp_print_pda_actions info) actions;
               IntTable.add trans_table index actions) trans_table states

   (*
    * Find the start state for a production.
    *)
   let create_start info start_table examined unexamined start =
      let gram = info.info_grammar in
      let prods =
         try
            let _, start = var_of_symbol gram start in
               VarMTable.find_all gram.gram_prod start
         with
            Not_found ->
               raise (Failure ("no such production: " ^ to_string start))
      in
      let lookahead = VarSet.singleton eof_var in
      let table =
         List.fold_left (fun table prod ->
               let prod_item = prod_item_of_prod prod in
                  ProdItemTable.add table prod_item lookahead) ProdItemTable.empty prods
      in
      let closure = closure info table in
      let info_item, examined, unexamined = add_state examined unexamined closure in
      let start_table = SymbolTable.add start_table start info_item.info_item_index in
         start_table, examined, unexamined

   let create gram =
      let info = info_of_grammar gram in
      let start_table, examined, unexamined =
         SymbolSet.fold (fun (table, examined, unexamined) start ->
               create_start info table examined unexamined start) (**)
            (SymbolTable.empty, ProdItemSetTable.empty, ProdItemSetTable.empty) gram.gram_start_symbols
      in
      let trans_table, states = shift_closure info IntTable.empty examined unexamined in
      let trans_table = reduce info trans_table states in

      (* Build the PDA states *)
      let null_state =
         { pda_delta = VarTable.empty;
           pda_reduce = ReduceNone
         }
      in
      let table = Array.create (ProdItemSetTable.cardinal states) null_state in
      let () =
         ProdItemSetTable.iter (fun _ info_item ->
               let  { info_item_index = index;
                      info_item_table = items
                    } = info_item
               in
               let state =
                  { pda_delta  = IntTable.find trans_table index;
                    pda_reduce = reduce_early items
                  }
               in
                  table.(index) <- state) states
      in
         { pda_start_states  = start_table;
           pda_var_of_symbol = gram.gram_var_of_symbol;
           pda_symbol_of_var = gram.gram_symbol_of_var;
           pda_states        = table
         }

   (*
    * Execute a semantic action.
    *)
   let loc_of_stack stack =
      match stack with
         (_, loc, _) :: _ ->
            loc
       | [] ->
            bogus_loc "null"

   let rec collect_args state args loc1 stack i =
      if i = 0 then
         state, loc1, args, stack
      else
         match stack with
            (state, loc2, arg) :: stack ->
               collect_args state (arg :: args) (union_loc loc1 loc2) stack (pred i)
          | [] ->
               raise (Invalid_argument "semantic_action: stack is empty")

   let semantic_action eval arg action stack state tokens =
      let loc = loc_of_stack stack in
      let state, loc, args, stack = collect_args 0 [] loc stack tokens in
      let () =
         if !debug_parse then
            eprintf "Calling action %a@." pp_print_action action
      in
      let arg, value = eval arg action loc args in
         state, arg, loc, value, stack

   (*
    * Execution.
    *
    * The stack contains (state * value) pairs, where the
    * state is the state of the machine when that token was pushed.
    *
    *)
   let fst3 (v, _, _) = v

   let rec pda_lookahead run arg stack state token =
      let { pda_delta = delta } = run.run_states.(state) in
      let v, loc, x = token in
      let action =
         try IntTable.find delta v with
            Not_found ->
               raise (ParseError loc)
      in
         match IntTable.find delta v with
            ShiftAction new_state
          | GotoAction new_state ->
               if !debug_parse then
                  eprintf "State %d: token %a: shift %d@." state pp_print_symbol (VarTable.find run.run_symbol_of_var v) new_state;
               pda_no_lookahead run arg ((state, loc, x) :: stack) new_state
          | ReduceAction (action, name, tokens) ->
               if !debug_parse then
                  eprintf "State %d: reduce %a@." state pp_print_action action;
               let state, arg, loc, x, stack = semantic_action run.run_eval arg action stack state tokens in
                  pda_goto_lookahead run arg loc stack state name x token
          | AcceptAction ->
               match stack with
                  [_, _, x] ->
                     arg, x
                | _ ->
                     raise (Invalid_argument "pda_lookahead")

   and pda_goto_lookahead run arg loc stack state name x token =
      if !debug_parse then
         eprintf "State %d: Goto lookahead: production %a@." (**)
            state pp_print_symbol (VarTable.find run.run_symbol_of_var name);
      let action =
         try IntTable.find run.run_states.(state).pda_delta name with
            Not_found ->
               raise (ParseError loc)
      in
         match action with
            ShiftAction new_state
          | GotoAction new_state ->
               if !debug_parse then
                  eprintf "State %d: production %a: goto %d (lookahead %a)@." (**)
                     state pp_print_symbol (VarTable.find run.run_symbol_of_var name)
                     new_state pp_print_symbol (VarTable.find run.run_symbol_of_var (fst3 token));
               let stack = (state, loc, x) :: stack in
                  pda_lookahead run arg stack new_state token
          | ReduceAction _
          | AcceptAction ->
               eprintf "pda_goto_no_lookahead: illegal action: %a@." pp_print_pda_action action;
               raise (Invalid_argument "pda_goto_lookahead: illegal action")

   and pda_no_lookahead run arg stack state =
      match run.run_states.(state).pda_reduce with
         ReduceNow (action, name, tokens) ->
            if !debug_parse then
               eprintf "State %d: ReduceNow: %a@." state pp_print_action action;
            let state, arg, loc, x, stack = semantic_action run.run_eval arg action stack state tokens in
               pda_goto_no_lookahead run arg loc stack state name x
       | ReduceAccept (action, name, tokens) ->
            if !debug_parse then
               eprintf "State %d: ReduceAccept: %a@." state pp_print_action action;
            let _, arg, _, x, _ = semantic_action run.run_eval arg action stack state tokens in
               arg, x
       | ReduceNone ->
            let sym, loc, arg, x = run.run_lexer arg in
            let () =
               if !debug_parse then
                  eprintf "State %d: Read token: %a@." state pp_print_symbol sym
            in
            let v =
               try SymbolTable.find run.run_var_of_symbol sym with
                  Not_found ->
                     raise (ParseError loc)
            in
               pda_lookahead run arg stack state (v, loc, x)

   and pda_goto_no_lookahead run arg loc stack state name x =
      let action =
         try IntTable.find run.run_states.(state).pda_delta name with
            Not_found ->
               raise (ParseError loc)
      in
         match action with
            ShiftAction new_state
          | GotoAction new_state ->
               if !debug_parse then
                  eprintf "State %d: production %a: goto %d (no lookahead)@." (**)
                     state pp_print_symbol (VarTable.find run.run_symbol_of_var name) new_state;
               let stack = (state, loc, x) :: stack in
                  pda_no_lookahead run arg stack new_state
          | ReduceAction _
          | AcceptAction ->
               eprintf "pda_goto_no_lookahead: illegal action: %a@." pp_print_pda_action action;
               raise (Invalid_argument "pda_goto_no_lookahead")

   let parse pda start lexer eval arg =
      let { pda_var_of_symbol = var_of_symbol;
            pda_symbol_of_var = symbol_of_var;
            pda_states        = states;
            pda_start_states  = start_states
          } = pda
      in
      let run =
         { run_var_of_symbol = var_of_symbol;
           run_symbol_of_var = symbol_of_var;
           run_states        = states;
           run_lexer         = lexer;
           run_eval          = eval
         }
      in
      let start =
         try SymbolTable.find start_states start with
            Not_found ->
               raise (Failure ("not a start symbol: " ^ to_string start))
      in
         try pda_no_lookahead run arg [] start with
            Not_found ->
               raise (Failure "syntax error")

   (************************************************************************
    * Wrappers.
    *)
   let empty =
      { parse_grammar = empty_grammar;
        parse_pda     = None
      }

   let add_start info sym =
      let gram = add_start info.parse_grammar sym in
         { parse_grammar = gram; parse_pda = None }

   let prec_min = Precedence.prec_min
   let prec_max = Precedence.prec_max

   let add_assoc info pre assoc =
      let { parse_grammar = gram } = info in
      let { gram_prec_table = prec_table } = gram in
      let prec_table = Precedence.add_assoc prec_table pre assoc in
      let gram = { gram with gram_prec_table = prec_table } in
      let info = { parse_grammar = gram; parse_pda = None } in
         info

   let create_prec_lt info pre assoc =
      let { parse_grammar = gram } = info in
      let { gram_prec_table = prec_table } = gram in
      let prec_table, pre = Precedence.create_prec_lt prec_table pre assoc in
      let gram = { gram with gram_prec_table = prec_table } in
      let info = { parse_grammar = gram; parse_pda = None } in
         info, pre

   let create_prec_gt info pre assoc =
      let { parse_grammar = gram } = info in
      let { gram_prec_table = prec_table } = gram in
      let prec_table, pre = Precedence.create_prec_gt prec_table pre assoc in
      let gram = { gram with gram_prec_table = prec_table } in
      let info = { parse_grammar = gram; parse_pda = None } in
         info, pre

   let add_prec info pre v =
      let gram = add_prec info.parse_grammar pre v in
         { parse_grammar = gram; parse_pda = None }

   let find_prec info v =
      find_prec info.parse_grammar v

   let add_production info action name rhs pre =
      let gram = add_production info.parse_grammar action name rhs pre in
         { parse_grammar = gram; parse_pda = None }

   let remove_production info action =
      let gram = remove_production info.parse_grammar action in
         { parse_grammar = gram; parse_pda = None }

   let parse info start lexer eval =
      let pda =
         match info.parse_pda with
            Some pda ->
               pda
          | None ->
               let pda = create info.parse_grammar in
                  info.parse_pda <- Some pda;
                  pda
      in
         parse pda start lexer eval

   let build info debug =
      let prev_debug = !debug_parse in
      let () = debug_parse := debug in
      let pda = create info.parse_grammar in
         debug_parse := prev_debug;
         info.parse_pda <- Some pda
end

(*
 * Default precedence module.
 *)
module ParserPrecedence : PrecedenceArg =
struct
   (*
    * A precedence has a name and associativity.
    * The integer gives the *name* of a precedence,
    * not the actual priority.
    *)
   type precedence = int

   module PrecTable = IntTable;;
   type t = (assoc * int) PrecTable.t

   (*
    * Degenerate precedences.
    *)
   let prec_min    = 0
   let prec_max    = 1

   let empty =
      let prec_table = PrecTable.empty in
      let prec_table = PrecTable.add prec_table prec_min (NonAssoc, 0) in
      let prec_table = PrecTable.add prec_table prec_max (NonAssoc, 1) in
         prec_table

   (*
    * Check that the associativity matches.
    *)
   let add_assoc table pre assoc =
      let () =
         try
            let assoc', _ = PrecTable.find table pre in
               if assoc' <> assoc then
                  raise (Failure "ParserPrecedence.add_assoc: associativities do not match")
         with
            Not_found ->
               raise (Failure "ParserPrecedence.add_assoc: precedence is not defined")
      in
         table

   (*
    * Shift all the precedence levels at least the given level
    * up by one.
    *)
   let prec_shift table prio =
      PrecTable.map (fun (assoc, prio2) ->
            let prio =
               if prio2 >= prio then
                  succ prio2
               else
                  prio2
            in
               assoc, prio) table

   (*
    * Create a new precedence level after the given one.
    *)
   let create_prec_lt table pre assoc =
      let index = PrecTable.cardinal table in
      let _, prio = PrecTable.find table pre in
      let table = prec_shift table prio in
      let table = PrecTable.add table index (assoc, prio) in
         table, index

   let create_prec_gt table pre assoc =
      let index = PrecTable.cardinal table in
      let _, prio = PrecTable.find table pre in
      let table = prec_shift table (succ prio) in
      let table = PrecTable.add table index (assoc, succ prio) in
         table, index

   (*
    * Get the associativity of a precedence operator.
    *)
   let assoc table pre =
      fst (PrecTable.find table pre)

   (*
    * Compare two precedences.
    *)
   let compare table pre1 pre2 =
      let _, prio1 = PrecTable.find table pre1 in
      let _, prio2 = PrecTable.find table pre2 in
         prio1 - prio2

   (*
    * Print the precedence.
    *)
   let pp_print_prec table buf pre =
      let assoc, prio = PrecTable.find table pre in
         fprintf buf "%a, %d" pp_print_assoc assoc prio
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
