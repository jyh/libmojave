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
open Lm_int_set

let debug_parse =
   create_debug (**)
      { debug_name = "parse";
        debug_description = "Debug the parseer";
        debug_value = false
      }

let debug_parsegen =
   create_debug (**)
      { debug_name = "parsegen";
        debug_description = "Debug the parser generator";
        debug_value = false
      }

let debug_parsetiming =
   create_debug (**)
      { debug_name = "parsetiming";
        debug_description = "Display timing statistics for the parser generator";
        debug_value = false
      }

let debug_parse_conflict_is_warning =
   create_debug (**)
      { debug_name = "parse_conflict_is_warning";
        debug_description = "Do not abort on grammar conflicts";
        debug_value = false
      }

(*
 * A precedence directive is left-associative, right-associative,
 * or nonassociative.
 *)
type assoc =
   LeftAssoc
 | RightAssoc
 | NonAssoc
 | NoneAssoc

let pp_print_assoc buf assoc =
   let s =
      match assoc with
         LeftAssoc ->
            "left"
       | RightAssoc ->
            "right"
       | NonAssoc ->
            "nona"
       | NoneAssoc ->
            "none"
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

   (* Tables and sets *)
   module PrecTable   : Lm_map_sig.LmMap with type key = precedence
end

exception ParseError of loc * string

(*
 * The parser is parameterized over symbol and action names.
 *)
module type ParserArg =
sig
   (* Variable names: the names of terminals and nonterminals *)
   type symbol

   (* A symbol to represent eof *)
   val eof : symbol

   (* For debugging *)
   val to_string : symbol -> string
   val pp_print_symbol : out_channel -> symbol -> unit

   (* Sets and tables *)
   module SymbolSet : Lm_set_sig.LmSet with type elt = symbol;;
   module SymbolTable : Lm_map_sig.LmMap with type key = symbol;;
   module SymbolMTable : Lm_map_sig.LmMapList with type key = symbol;;

   (* Names of semantic actions *)
   type action

   (* For debugging *)
   val pp_print_action : out_channel -> action -> unit

   (* Set of actions *)
   module ActionSet : Lm_set_sig.LmSet with type elt = action
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
    * JYH: it used to be the case that we represented vars with
    * integers.  However, this makes the union operation pretty hard,
    * so we just go ahead and use symbols directly.
    *)
   type var = symbol

   module VarSet    = SymbolSet;;
   module VarTable  = SymbolTable;;
   module VarMTable = SymbolMTable;;

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

   (* %%MAGICBEGIN%% *)
   type prod =
      { prod_action  : action;
        prod_name    : var;
        prod_rhs     : var list;
        prod_prec    : precedence
      }

   (*
    * A grammar has a set of symbols, productions,
    * and precedences.
    *)
   type grammar =
      { gram_prod          : prod VarMTable.t;
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
    | ErrorAction

   (*
    * We may reduce states without lookahead,
    * and we may accept.
    *)
   type pda_reduce =
      ReduceNone
    | ReduceNow    of action * var * int
    | ReduceAccept of action * var * int

   (*
    * The PDA transition table.
    * The pda_item is *purely* for debugging.
    *)
   type pda_item =
      { pda_item_left      : var list;  (* Reverse order *)
        pda_item_right     : var list
      }

   type pda_state_info =
      { pda_items     : pda_item list;
        pda_next      : VarSet.t
      }

   type pda_state =
      { pda_delta   : pda_action VarTable.t;
        pda_reduce  : pda_reduce;
        pda_info    : pda_state_info
      }

   type pda =
      { pda_start_states  : int SymbolTable.t;
        pda_states        : pda_state array
      }

   (*
    * The actual machine has a grammar and an optional pda.
    *)
   type t =
      { parse_grammar     : grammar;
        mutable parse_pda : pda option
      }
   (* %%MAGICEND%% *)

   (*
    * Run time info.
    *)
   type ('a, 'b) run =
      { run_states        : pda_state array;
        run_lexer         : ('a, 'b) lexer;
        run_eval          : ('a, 'b) eval
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
    *
    * The hash is just to make comparisons faster.
    *)
   type prod_item_core =
      { prod_item_name   : var;
        prod_item_left   : var list;       (* Reverse order *)
        prod_item_right  : var list;
        prod_item_action : action;
        prod_item_prec   : precedence
      }

   type prod_item =
      { prod_item_hash   : int;
        prod_item_core   : prod_item_core
      }

   module ProdItemCompare =
   struct
      type t = prod_item
      let compare = Pervasives.compare
   end

   module ProdItemSet   = Lm_set.LmMake (ProdItemCompare);;
   module ProdItemTable = Lm_map.LmMake (ProdItemCompare);;

   (*
    * An LR(0) item is a set of ProdItemSet
    *)
   module ProdItemSetCompare =
   struct
      type t = ProdItemSet.t
      let compare = ProdItemSet.compare
   end

   module ProdItemSetSet = Lm_set.LmMake (ProdItemSetCompare);;
   module ProdItemSetTable = Lm_map.LmMake (ProdItemSetCompare);;

   (*
    * The lookahead is a set of variables.
    *)
   type lookahead = VarSet.t

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

   (*
    * A state element is a set of items, with lookaheads for each.
    *)
   type info_item =
      { info_item_index   : int;
        info_item_table   : lookahead ProdItemTable.t
      }

   (*
    * Info for propagating lookaheads.
    *)
   type lookahead_item =
      LookAheadConst of VarSet.t
    | LookAheadProp  of VarSet.t

   (*
    * Info for propagating lookaheads.
    *)
   type prop_info =
      { prop_next_state   : int;
        prop_next_item    : int;
        prop_lookahead    : lookahead_item
      }

   type prop_entry =
      { prop_prod_item    : prod_item;
        mutable prop_info : prop_info list;
        mutable prop_vars : VarSet.t
      }

   (************************************************************************
    * Printing and errors.
    *)
   let pp_print_var gram buf v =
      pp_print_symbol buf v

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
         fprintf buf "@[<hv 3>(production@ action: %a@ prec: %a@ target: %a@ @[<hv 3>sources:@ %a@])@]" (**)
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

   let pp_print_prod_item_core info buf item =
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

   let pp_print_prod_item info buf item =
      pp_print_prod_item_core info buf item.prod_item_core

   let pp_print_pda_action buf action =
      match action with
         ReduceAction (action, _, _) ->
            fprintf buf "reduce %a" pp_print_action action
       | ShiftAction id ->
            fprintf buf "shift %d" id
       | GotoAction id ->
            fprintf buf "goto %d" id
       | ErrorAction ->
            pp_print_string buf "error"
       | AcceptAction  ->
            pp_print_string buf "accept"

   let pp_print_pda_actions info buf actions =
      let pp_print_var = pp_print_var info.info_grammar in
         VarTable.iter (fun v action ->
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
   let shift_reduce_conflict info info_item actions v id prod_item =
      let gram = info.info_grammar in
         eprintf "shift/reduce conflict on %a: shift %d, reduce %a@." (**)
            (pp_print_var gram) v
            id
            pp_print_action prod_item.prod_item_core.prod_item_action;
         if not !debug_parse_conflict_is_warning && not !debug_parsegen then
            eprintf "@[<v 0>%a@ @[<v 3>%a@]@ @]@." (**)
               (pp_print_info_item info) info_item
               (pp_print_pda_actions info) actions;
         if not !debug_parse_conflict_is_warning then
            raise (Invalid_argument "Lm_parser.shift_reduce_conflict\n\tset MP_DEBUG=parse_conflict_is_warning to ignore this error")

   let reduce_reduce_conflict info info_item actions v action1 action2 =
      let gram = info.info_grammar in
         eprintf "reduce/reduce conflict on %a: reduce %a, reduce %a@." (**)
            (pp_print_var gram) v
            pp_print_action action1
            pp_print_action action2;
         if not !debug_parse_conflict_is_warning && not !debug_parsegen then
            eprintf "@[<v 0>%a@ @[<v 3>%a@]@ @]@." (**)
               (pp_print_info_item info) info_item
               (pp_print_pda_actions info) actions;
         if not !debug_parse_conflict_is_warning then
            raise (Invalid_argument "Lm_parser.reduce_reduce_conflict:\n\tset MP_DEBUG=parse_conflict_is_warning to ignore this error")

   (************************************************************************
    * Grammar construction.
    *)

   (*
    * Empty grammar has the basic precedences.
    *)
   let empty_grammar =
      { gram_prod          = VarMTable.empty;
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
   let add_prec gram pre v =
      { gram with gram_prec = VarTable.add gram.gram_prec v pre }

   (*
    * Find the precedence level for a symbol.
    *)
   let find_prec gram v =
      VarTable.find gram.gram_prec v

   (*
    * Add a production.
    * If the precedence is not specified, it is the precedence
    * of the rightmost variable that has a precedence.
    *)
   let add_production gram action v rhs pre =
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

   (*
    * Precedence union is a little hard.
    * Suppose the second grammar contains some precedence
    * levels that do not occur in the first grammar.  We
    * have to insert some levels, and we have to figure out
    * where to put them.
    *
    * The basic idea is to build an inverse table for the
    * second grammar.  Then sort this grammar, and walk
    * through each level.  If it exists in the first grammar,
    * keep it.  Otherwise add a new level, and continue.
    *)
   let rec find_existing_prec precs vars =
      match vars with
         [] ->
            None
       | v :: vars ->
            try Some (VarTable.find precs v) with
               Not_found ->
                  find_existing_prec precs vars

   let add_precs precs vars pre =
      List.fold_left (fun precs v ->
            VarTable.add precs v pre) precs vars

   let union_prec prec1 table1 prec2 table2 =
      (* Build an inverse precedence table for grammar2 *)
      let inv_table =
         VarTable.fold (fun inv_table v pre ->
               PrecTable.filter_add inv_table pre (fun vars ->
                     let vars =
                        match vars with
                           Some vars ->
                              vars
                         | None ->
                              []
                     in
                        v :: vars)) PrecTable.empty prec2
      in

      (* Sort the precedences in grammar2 *)
      let prec_list =
         PrecTable.fold (fun prec_list pre _ ->
               pre :: prec_list) [] inv_table
      in
      let prec_list = List.sort (Precedence.compare table2) prec_list in

      (* Initial translation *)
      let translate = PrecTable.empty in
      let translate = PrecTable.add translate prec_min prec_min in
      let translate = PrecTable.add translate prec_max prec_max in

      (* Walk through each level, and create it if it doesn't already exist *)
      let translate, precs, table, _ =
         List.fold_left (fun (translate, precs, table, prev_prec) pre ->
               let vars = PrecTable.find inv_table pre in
               let table, current_prec =
                  match find_existing_prec precs vars with
                     Some current_prec ->
                        table, current_prec
                   | None ->
                        let assoc = Precedence.assoc table2 pre in
                           Precedence.create_prec_gt table prev_prec assoc
               in
               let translate = PrecTable.add translate pre current_prec in
               let precs = add_precs precs vars current_prec in
                  translate, precs, table, current_prec) (translate, prec1, table1, Precedence.prec_min) prec_list
      in
(*
         PrecTable.iter (fun pre1 pre2 ->
               eprintf "Translate %d -> %d@." (Obj.magic pre1) (Obj.magic pre2)) translate;
         eprintf "Pre: %d Table1: %d Table2: %d@." (**)
            (IntTable.cardinal (Obj.magic table))
            (IntTable.cardinal (Obj.magic table1))
            (IntTable.cardinal (Obj.magic table2));
         VarTable.iter (fun _ pre ->
               eprintf "Checking precedence for %d@." (Obj.magic pre);
               ignore (Precedence.assoc table pre)) precs;
 *)
         translate, precs, table

   (*
    * Union of two grammars.
    *)
   let union_grammar gram1 gram2 =
      let { gram_prod          = prod1;
            gram_prec          = prec1;
            gram_prec_table    = prec_table1;
            gram_start_symbols = start1
          } = gram1
      in
      let { gram_prod          = prod2;
            gram_prec          = prec2;
            gram_prec_table    = prec_table2;
            gram_start_symbols = start2
          } = gram2
      in

      (* Compute the new precedence table *)
      let prec_translate, precs, prec_table = union_prec prec1 prec_table1 prec2 prec_table2 in

      (* Get the complete set of actions for the first parser *)
      let actions =
         VarMTable.fold_all (fun actions _ prods ->
               List.fold_left (fun actions { prod_action = action } ->
                     ActionSet.add actions action) actions prods) ActionSet.empty prod1
      in

      (* Take the union of the productions *)
      let changed, prods =
         VarMTable.fold_all (fun (changed, prods) _ prodlist ->
               List.fold_left (fun (changed, prods) prod ->
                     let { prod_action = action;
                           prod_name   = name;
                           prod_prec   = pre
                         } = prod
                     in
                        if ActionSet.mem actions prod.prod_action then
                           changed, prods
                        else
                           let prod = { prod with prod_prec = PrecTable.find prec_translate pre } in
                              true, VarMTable.add prods name prod) (changed, prods) prodlist) (false, prod1) prod2
      in

      (* Union of the start symbols *)
      let start = SymbolSet.union start1 start2 in

      (* Has anything changed? *)
      let changed =
         changed
         || (VarTable.cardinal precs <> VarTable.cardinal prec1)
         || (SymbolSet.cardinal start <> SymbolSet.cardinal start1)
      in

      (* New grammar *)
      let gram =
         { gram_prod          = prods;
           gram_prec          = precs;
           gram_prec_table    = prec_table;
           gram_start_symbols = start
         }
      in
         changed, gram

   (*
    * Debugging version.
    *)
   let union_grammar gram1 gram2 =
      if !debug_parsegen then
         eprintf "@[<v 3>Grammar union:@ @[<hv 3>Grammar1:@ %a@]@ @[<hv 3>Grammar2:@ %a@]@]@." (**)
            pp_print_grammar gram1
            pp_print_grammar gram2;
      let changed, gram = union_grammar gram1 gram2 in
         if !debug_parsegen then
            eprintf "@[<v 3>Grammar union %b:@ %a@]@." (**)
               changed pp_print_grammar gram;
         changed, gram

   (************************************************************************
    * Initial PDA construction.
    *)

   (*
    * A nonterminal is nullable if all variables on the rhs are nullable.
    *)
   let nullable gram =
      let step nullable prods =
         VarMTable.fold_all (fun nullable v prods ->
               if VarSet.mem nullable v then
                  nullable
               else if List.exists (fun prod -> List.for_all (VarSet.mem nullable) prod.prod_rhs) prods then
                  VarSet.add nullable v
               else
                  nullable) nullable prods
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
            let set = VarSet.union set (VarTable.find first v) in
               if VarSet.mem nullable v then
                  first_rhs nullable first set rhs
               else
                  set
       | [] ->
            set

   let first gram nullable =
      let step first prods =
         VarMTable.fold_all (fun (first, changed) _ prods ->
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
      let { gram_prod = prods } = gram in
      let vars =
         VarMTable.fold_all (fun vars v prods ->
               let vars = VarSet.add vars v in
                  List.fold_left (fun vars prod ->
                        List.fold_left VarSet.add vars prod.prod_rhs) vars prods) VarSet.empty prods
      in
      let first =
         VarSet.fold (fun first v ->
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
    * LR(0) construction.
    *)

   (*
    * Build a prod_item from a production.
    *)
   let prod_item_of_core core =
      { prod_item_hash = Hashtbl.hash core;
        prod_item_core = core
      }

   let prod_item_of_prod prod =
      let { prod_action = action;
            prod_name   = name;
            prod_rhs    = rhs;
            prod_prec   = pre
          } = prod
      in
      let core =
         { prod_item_action = action;
           prod_item_prec   = pre;
           prod_item_name   = name;
           prod_item_left   = [];
           prod_item_right  = rhs
         }
      in
         prod_item_of_core core

   (*
    * Take the closure of a production.
    *)
   let closure info (set : ProdItemSet.t) =
      let { info_grammar  = { gram_prod = prods } } = info in
      let rec close examined unexamined closure =
         if VarSet.is_empty unexamined then
            closure
         else
            let examined, unexamined, closure =
               VarSet.fold (fun (examined, unexamined, closure) v ->
                     let unexamined = VarSet.remove unexamined v in
                        if VarSet.mem examined v then
                           examined, unexamined, closure
                        else
                           let examined = VarSet.add examined v in
                              try
                                 let prods = VarMTable.find_all prods v in
                                 let unexamined, closure =
                                    List.fold_left (fun (unexamined, closure) prod ->
                                          let unexamined =
                                             match prod.prod_rhs with
                                                v :: _ ->
                                                   VarSet.add unexamined v
                                              | [] ->
                                                   unexamined
                                          in
                                          let closure = ProdItemSet.add closure (prod_item_of_prod prod) in
                                             unexamined, closure) (unexamined, closure) prods
                                 in
                                    examined, unexamined, closure
                              with
                                 Not_found ->
                                    examined, unexamined, closure) (examined, unexamined, closure) unexamined
            in
               close examined unexamined closure
      in
      let unexamined =
         ProdItemSet.fold (fun unexamined prod_item ->
               match prod_item.prod_item_core.prod_item_right with
                  v :: _ ->
                     VarSet.add unexamined v
                | [] ->
                     unexamined) VarSet.empty set
      in
         close VarSet.empty unexamined set

   (*
    * Add the state identified by the closure to the set
    * of known LR(0) states.
    *)
   let add_state examined unexamined closure =
      try ProdItemSetTable.find examined closure, unexamined with
         Not_found ->
            try ProdItemSetTable.find unexamined closure, unexamined with
               Not_found ->
                  let index = ProdItemSetTable.cardinal examined + ProdItemSetTable.cardinal unexamined in
                  let unexamined = ProdItemSetTable.add unexamined closure index in
                     index, unexamined

   (*
    * Figure out all the symbols on which we can make a transition
    * by shifting.
    *)
   let shift_symbols item =
      ProdItemSet.fold (fun syms prod_item ->
            match prod_item.prod_item_core.prod_item_right with
               v :: _ ->
                  VarSet.add syms v
             | [] ->
                  syms) VarSet.empty item

   (*
    * Perform the shift by a symbol.
    *)
   let shift_item item v =
      ProdItemSet.fold (fun set prod_item ->
            let core = prod_item.prod_item_core in
            let { prod_item_left = left;
                  prod_item_right = right
                } = core
            in
               match right with
                  v' :: right when v' = v ->
                     let core =
                        { core with prod_item_left = v :: left;
                                    prod_item_right = right
                        }
                     in
                     let prod_item = prod_item_of_core core in
                        ProdItemSet.add set prod_item
                | _ ->
                     set) ProdItemSet.empty item

   (*
    * Compute the transition table, only for shift operations.
    *)
   let rec shift_closure info shift_table examined unexamined =
      let { info_grammar = { gram_prod = prods } } = info in
         if ProdItemSetTable.is_empty unexamined then
            shift_table, examined
         else
            (* Move an item from unexamined to examined *)
            let item, index = ProdItemSetTable.choose unexamined in
            let examined = ProdItemSetTable.add examined item index in
            let unexamined = ProdItemSetTable.remove unexamined item in

            (* Compute the goto states *)
            let syms = shift_symbols item in
            let goto_table, unexamined =
               VarSet.fold (fun (goto_table, unexamined) v ->
                     let item = shift_item item v in
                     let item = closure info item in
                     let next, unexamined = add_state examined unexamined item in
                     let goto_table = VarTable.add goto_table v next in
                        goto_table, unexamined) (VarTable.empty, unexamined) syms
            in
            let shift_table = IntTable.add shift_table index goto_table in
               shift_closure info shift_table examined unexamined

   (************************************************************************
    * LALR(1) construction.
    *
    * Once we have the set of LR(0) states, we need to propagate lookahead
    * sets.  For each item in a state, figure out what symbols are propagated
    * and which are spontaneously generated, then perform a fixpoint.
    *)

   (*
    * Get the set of first symbols that can being a list.
    *)
   let rec lookahead nullable first set rhs =
      match rhs with
         v :: rhs ->
            let set = VarSet.union (VarTable.find first v) set in
               if VarSet.mem nullable v then
                  lookahead nullable first set rhs
               else
                  LookAheadConst set
       | [] ->
            LookAheadProp set

   (*
    * Construct the lookahead for the next item from the
    * lookahead info and the lookahead for this item.
    *)
   let lookahead_vars lookahead vars =
      match lookahead with
         LookAheadConst set ->
            set
       | LookAheadProp set ->
            VarSet.union vars set

   (*
    * Give an index to each of the productions in a state.
    *)
   let build_item_indices states =
      ProdItemSetTable.mapi (fun item index ->
            let table, _ =
               ProdItemSet.fold (fun (table, index) prod_item ->
                     let table = ProdItemTable.add table prod_item index in
                        table, succ index) (ProdItemTable.empty, 0) item
            in
               table, index) states

   let build_state_index states =
      ProdItemSetTable.fold (fun table state (prods, index) ->
            IntTable.add table index prods) IntTable.empty states

   (*
    * Now construct a propagation network.
    * Each state is represented as an array of production indices,
    * each with a propagation entry to another item identified
    * by (state, index).
    *)
   let build_prop_table info shift_table state_table =
      let { info_nullable = nullable;
            info_first = first;
            info_grammar = { gram_prod = prods }
          } = info
      in
      let state_count = IntTable.cardinal state_table in
      let prop_table = Array.create state_count [||] in
         IntTable.iter (fun state_index prod_table ->
               let goto_table = IntTable.find shift_table state_index in
               let prod_count = ProdItemTable.cardinal prod_table in
               let prod_entry_list =
                  ProdItemTable.fold (fun prop_entry_list prod_item prod_index ->
                        let prod_item_core = prod_item.prod_item_core in
                        let { prod_item_left = left;
                              prod_item_right = right
                            } = prod_item_core
                        in
                        let prop_entry =
                           match right with
                              v :: right ->
                                 (* If v is a nonterminal, add the self-edges *)
                                 let lookahead = lookahead nullable first VarSet.empty right in
                                 let prop_infos =
                                    try
                                       let prods = VarMTable.find_all prods v in
                                          List.fold_left (fun prop_infos prod ->
                                                let prod_item = prod_item_of_prod prod in
                                                let item_index = ProdItemTable.find prod_table prod_item in
                                                let prop_info =
                                                   { prop_next_state = state_index;
                                                     prop_next_item = item_index;
                                                     prop_lookahead = lookahead
                                                   }
                                                in
                                                   prop_info :: prop_infos) [] prods
                                    with
                                       Not_found ->
                                          []
                                 in

                                 (* Get the goto state *)
                                 let next_state_index = VarTable.find goto_table v in
                                 let next_item_core =
                                    { prod_item_core with prod_item_left = v :: left;
                                                          prod_item_right = right
                                    }
                                 in
                                 let next_item = prod_item_of_core next_item_core in
                                 let next_table = IntTable.find state_table next_state_index in
                                 let next_item_index = ProdItemTable.find next_table next_item in
                                 let prop_info =
                                    { prop_next_state = next_state_index;
                                      prop_next_item  = next_item_index;
                                      prop_lookahead  = LookAheadProp VarSet.empty
                                    }
                                 in
                                    { prop_prod_item = prod_item;
                                      prop_info      = prop_info :: prop_infos;
                                      prop_vars      = VarSet.empty
                                    }
                            | [] ->
                                 { prop_prod_item = prod_item;
                                   prop_info      = [];
                                   prop_vars      = VarSet.empty
                                 }
                        in
                           prop_entry :: prop_entry_list) [] prod_table
               in
               let prod_entry_table = Array.of_list (List.rev prod_entry_list) in
                  prop_table.(state_index) <- Array.of_list (List.rev prod_entry_list)) state_table;
         prop_table

   (*
    * Add the eof symbol for the start states.
    *)
   let set_start_lookahead start_table prop_table =
      let eof_set = VarSet.singleton eof in
         SymbolTable.iter (fun _ state_index ->
               Array.iter (fun prop_entry ->
                     prop_entry.prop_vars <- eof_set) prop_table.(state_index)) start_table

   (*
    * The fixpoint is a forward-dataflow problem.
    * Try to order the states so that dependencies are in
    * order.  Use depth-first-search to find an approximate
    * order.
    *)
   let int_set_of_interval lower upper =
      let rec collect set i =
         if i = upper then
            set
         else
            collect (IntSet.add set i) (succ i)
      in
         collect IntSet.empty lower

   let propagate_order_core prop_table =
      let next_table =
         Array.map (fun prod_table ->
               Array.fold_left (fun next prop ->
                     List.fold_left (fun next info ->
                           IntSet.add next info.prop_next_state) next prop.prop_info) IntSet.empty prod_table) prop_table
      in
      let rec search order examined unexamined index =
         let unexamined = IntSet.remove unexamined index in
            if IntSet.mem examined index then
               order, examined, unexamined
            else
               let order = index :: order in
               let examined = IntSet.add examined index in
               let next = next_table.(index) in
                  IntSet.fold (fun (order, examined, unexamined) index ->
                        search order examined unexamined index) (order, examined, unexamined) next
      in
      let rec fixpoint order examined unexamined =
         if IntSet.is_empty unexamined then
            List.rev order
         else
            let index = IntSet.choose unexamined in
            let order, examined, unexamined = search order examined unexamined index in
               fixpoint order examined unexamined
      in
      let unexamined = int_set_of_interval 0 (Array.length prop_table) in
         fixpoint [] IntSet.empty unexamined

   let check_order order =
      let len  = List.length order in
      let set1 = List.fold_left IntSet.add IntSet.empty order in
      let set2 = int_set_of_interval 0 len in
         assert (IntSet.equal set1 set2)

   let propagate_order prop_table =
      if !debug_parsetiming then
         let start = Unix.gettimeofday () in
         let order = propagate_order_core prop_table in
            check_order order;
            eprintf "propagate_order: %g secs@." (Unix.gettimeofday () -. start);
            order
      else
         propagate_order_core prop_table

   (*
    * Now solve the lookahead fixpoint.
    *)
   let propagate_lookahead prop_table order =
      let step () =
         List.fold_left (fun changed index ->
               let prod_table = prop_table.(index) in
                  Array.fold_left (fun changed prop ->
                        let { prop_info = info;
                              prop_vars = vars
                            } = prop
                        in
                           List.fold_left (fun changed info ->
                                 let { prop_next_state = next_state_index;
                                       prop_next_item  = next_item_index;
                                       prop_lookahead  = lookahead
                                     } = info
                                 in
                                 let vars1 = lookahead_vars lookahead vars in
                                 let next_entry = prop_table.(next_state_index).(next_item_index) in
                                 let vars2 = next_entry.prop_vars in
                                 let vars = VarSet.union vars1 vars2 in
                                    if VarSet.cardinal vars <> VarSet.cardinal vars2 then
                                       begin
                                          next_entry.prop_vars <- vars;
                                          true
                                       end
                                    else
                                       changed) changed info) changed prod_table) false order
      in
      let rec fixpoint () =
         if step () then
            fixpoint ()
      in
         if !debug_parsetiming then
            let start = Unix.gettimeofday () in
               eprintf "Lm_parser: solving fixpoint@.";
               fixpoint ();
               eprintf "Fixpoint: %g secs@." (Unix.gettimeofday () -. start)
         else
            fixpoint ()

   (*
    * Rebuild the traditional table from the propagation network.
    * The traditional table is a (info_item IntTable.t).
    *)
   let rebuild_state_table prop_table =
      let prop_table_len = Array.length prop_table in
      let rec collect state_table state_index =
         if state_index = prop_table_len then
            state_table
         else
            let info_table =
               Array.fold_left (fun entries prop_entry ->
                     let { prop_prod_item = prod_item;
                           prop_vars = lookahead
                         } = prop_entry
                     in
                        ProdItemTable.add entries prod_item lookahead) ProdItemTable.empty prop_table.(state_index)
            in
            let info_item =
               { info_item_index = state_index;
                 info_item_table = info_table
               }
            in
            let state_table = IntTable.add state_table state_index info_item in
               collect state_table (succ state_index)
      in
         collect IntTable.empty 0

   (*
    * Rebuild the transition table.
    *)
   let rebuild_trans_table info shift_table =
      let { info_grammar = { gram_prod = prods } } = info in
         IntTable.map (fun goto_table ->
               VarTable.mapi (fun v index ->
                     if VarMTable.mem prods v then
                        GotoAction index
                     else
                        ShiftAction index) goto_table) shift_table


   (*
    * Construct the LALR(1) table from the LR(0) table.
    *)
   let build_lalr_table info start_table unexamined =
      let shift_table, states = shift_closure info IntTable.empty ProdItemSetTable.empty unexamined in
      let state_table = build_item_indices states in
      let state_table = build_state_index state_table in
      let prop_table = build_prop_table info shift_table state_table in
      let () = set_start_lookahead start_table prop_table in
      let order = propagate_order prop_table in
      let () = propagate_lookahead prop_table order in
      let states = rebuild_state_table prop_table in
      let trans_table = rebuild_trans_table info shift_table in
         trans_table, states

   (************************************************************************
    * Constructing the PDA.
    *)

   (*
    * If a state has only one production,
    * and that is a reduce production, we can do
    * the reduce without lookahead.
    *)
   let reduce_early table =
      if ProdItemTable.cardinal table = 1 then
         match ProdItemTable.choose table with
            { prod_item_core =
                 { prod_item_right = [];
                   prod_item_action = action;
                   prod_item_name = name;
                   prod_item_left = left
                 }
            }, lookahead ->
               if VarSet.cardinal lookahead = 1 && VarSet.choose lookahead = eof then
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
   let reduce_action info info_item actions prod_item lookahead =
      let { info_grammar = gram } = info in
      let { gram_prec = var_prec_table;
            gram_prec_table = prec_table
          } = gram
      in
      let { prod_item_name   = name;
            prod_item_action = action;
            prod_item_left   = left;
            prod_item_prec   = prec_name
          } = prod_item.prod_item_core
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
                                    VarTable.add actions v ErrorAction
                               | NoneAssoc ->
                                    shift_reduce_conflict info info_item actions v id prod_item;
                                    actions
                           else
                              VarTable.add actions v reduce
                   | ReduceAction (action2, _, _) ->
                        reduce_reduce_conflict info info_item actions v action action2;
                        actions
                   | ErrorAction
                   | AcceptAction ->
                        raise (Invalid_argument "reduce_action")
               with
                  Not_found ->
                     VarTable.add actions v reduce) actions lookahead

   (*
    * Compute the reduce actions.
    *)
   let reduce info trans_table states =
      IntTable.fold (fun trans_table _ info_item ->
            let { info_item_index = index;
                  info_item_table = prod_items
                } = info_item
            in
            let actions = IntTable.find trans_table index in
            let actions =
               ProdItemTable.fold (fun actions prod_item lookahead ->
                     match prod_item.prod_item_core.prod_item_right with
                        [] ->
                           reduce_action info info_item actions prod_item lookahead
                      | _ ->
                           actions) actions prod_items
            in
               if !debug_parsegen then
                  eprintf "@[<v 0>%a@ @[<v 3>%a@]@ @]@." (**)
                     (pp_print_info_item info) info_item
                     (pp_print_pda_actions info) actions;
               IntTable.add trans_table index actions) trans_table states

   (*
    * Flatten a production state to a pda description.
    *)
   let pda_info_of_items items =
      let items, next =
         ProdItemTable.fold (fun (info, next) item lookahead ->
               let { prod_item_core  = core } = item in
               let { prod_item_left  = left;
                     prod_item_right = right
                   } = core
               in
               let item =
                  { pda_item_left  = left;
                    pda_item_right = right
                  }
               in
               let info = item :: info in
               let next =
                  match right with
                     v :: _ ->
                        VarSet.add next v
                   | [] ->
                        VarSet.union next lookahead
               in
                  info, next) ([], VarSet.empty) items
      in
         { pda_items     = items;
           pda_next      = next
         }

   (*
    * Find the start state for a production.
    *)
   let create_start info start_table unexamined start =
      let gram = info.info_grammar in
      let prods =
         try VarMTable.find_all gram.gram_prod start with
            Not_found ->
               raise (Failure ("no such production: " ^ to_string start))
      in
      let set =
         List.fold_left (fun set prod ->
               let prod_item = prod_item_of_prod prod in
                  ProdItemSet.add set prod_item) ProdItemSet.empty prods
      in
      let closure = closure info set in
      let state_index, unexamined = add_state ProdItemSetTable.empty unexamined closure in
      let start_table = SymbolTable.add start_table start state_index in
         start_table, unexamined

   let create_core gram =
      let info = info_of_grammar gram in
      let () =
         if !debug_parsegen then
            eprintf "@[<hv 3>Grammar:@ %a@]@." pp_print_info info
      in
      let start_table, unexamined =
         SymbolSet.fold (fun (start_table, unexamined) start ->
               create_start info start_table unexamined start) (**)
            (SymbolTable.empty, ProdItemSetTable.empty) gram.gram_start_symbols
      in
      let trans_table, states = build_lalr_table info start_table unexamined in
      let trans_table = reduce info trans_table states in

      (* Build the PDA states *)
      let null_info =
         { pda_items     = [];
           pda_next      = VarSet.empty
         }
      in
      let null_state =
         { pda_delta  = VarTable.empty;
           pda_reduce = ReduceNone;
           pda_info   = null_info
         }
      in
      let table = Array.create (IntTable.cardinal states) null_state in
      let () =
         IntTable.iter (fun _ info_item ->
               let  { info_item_index = index;
                      info_item_table = items
                    } = info_item
               in
               let state =
                  { pda_delta  = IntTable.find trans_table index;
                    pda_reduce = reduce_early items;
                    pda_info   = pda_info_of_items items
                  }
               in
                  table.(index) <- state) states
      in
         { pda_start_states  = start_table;
           pda_states        = table
         }

   let create gram =
      if !debug_parsetiming then
         let start = Unix.gettimeofday () in
         let () = eprintf "Creating grammar@." in
         let pda = create_core gram in
            eprintf "Created grammar: %g secs@." (Unix.gettimeofday () -. start);
            pda
      else
         create_core gram

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
      let state, loc, args, stack = collect_args state [] loc stack tokens in
      let () =
         if !debug_parse then
            eprintf "Calling action %a@." pp_print_action action
      in
      let arg, value = eval arg action loc args in
      let () =
         if !debug_parse then
            eprintf "Called action %a@." pp_print_action action
      in
         state, arg, loc, value, stack

   (*
    * Exceptions.
    *)
   let parse_error loc gram run stack state (v : var) =
      let { pda_info = { pda_items = items; pda_next = next } } = run.run_states.(state) in
      let pp_print_var = pp_print_var gram in
      let buf = stdstr in
         fprintf buf "@[<v 0>Syntax error on token %a" pp_print_var v;
         fprintf buf "@ @[<v 3>Current state:";
         List.iter (fun item ->
               let { pda_item_left = left;
                     pda_item_right = right
                   } = item
               in
                  fprintf buf "@[<b 3>";
                  Lm_list_util.rev_iter (fun v -> fprintf buf "@ %a" pp_print_var v) left;
                  fprintf buf "@ .";
                  List.iter (fun v -> fprintf buf "@ %a" pp_print_var v) right;
                  fprintf buf "@]") items;
         fprintf buf "@]";
         fprintf buf "@ @[<b 3>The next possible tokens are:";
         VarSet.iter (fun v -> fprintf buf "@ %a" pp_print_var v) next;
         fprintf buf "@]@]";
         raise (ParseError (loc, flush_stdstr ()))

   (*
    * Execution.
    *
    * The stack contains (state * value) pairs, where the
    * state is the state of the machine when that token was pushed.
    *
    *)
   let fst3 (v, _, _) = v

   let rec pda_lookahead gram run arg stack state tok =
      let { pda_delta = delta } = run.run_states.(state) in
      let v, loc, x = tok in
         match
            (try VarTable.find delta v with
                Not_found ->
                   parse_error loc gram run stack state v)
         with
            ShiftAction new_state
          | GotoAction new_state ->
               if !debug_parse then
                  eprintf "State %d: token %a: shift %d@." state pp_print_symbol v new_state;
               pda_no_lookahead gram run arg ((state, loc, x) :: stack) new_state
          | ReduceAction (action, name, tokens) ->
               if !debug_parse then
                  eprintf "State %d: reduce %a@." state pp_print_action action;
               let state, arg, loc, x, stack = semantic_action run.run_eval arg action stack state tokens in
                  pda_goto_lookahead gram run arg loc stack state name x tok
          | ErrorAction ->
               parse_error loc gram run stack state v
          | AcceptAction ->
               match stack with
                  [_, _, x] ->
                     arg, x
                | _ ->
                     raise (Invalid_argument "pda_lookahead")

   and pda_goto_lookahead gram run arg loc stack state name x tok =
      if !debug_parse then
         eprintf "State %d: Goto lookahead: production %a@." (**)
            state pp_print_symbol name;
      let action =
         try VarTable.find run.run_states.(state).pda_delta name with
            Not_found ->
               parse_error loc gram run stack state name
      in
         match action with
            ShiftAction new_state
          | GotoAction new_state ->
               if !debug_parse then
                  eprintf "State %d: production %a: goto %d (lookahead %a)@." (**)
                     state pp_print_symbol name
                     new_state pp_print_symbol (fst3 tok);
               let stack = (state, loc, x) :: stack in
                  pda_lookahead gram run arg stack new_state tok
          | ErrorAction
          | ReduceAction _
          | AcceptAction ->
               eprintf "pda_goto_no_lookahead: illegal action: %a@." pp_print_pda_action action;
               raise (Invalid_argument "pda_goto_lookahead: illegal action")

   and pda_no_lookahead gram run arg stack state =
      match run.run_states.(state).pda_reduce with
         ReduceNow (action, name, tokens) ->
            if !debug_parse then
               eprintf "State %d: ReduceNow: %a@." state pp_print_action action;
            let state, arg, loc, x, stack = semantic_action run.run_eval arg action stack state tokens in
               pda_goto_no_lookahead gram run arg loc stack state name x
       | ReduceAccept (action, _, tokens) ->
            if !debug_parse then
               eprintf "State %d: ReduceAccept: %a@." state pp_print_action action;
            let _, arg, _, x, _ = semantic_action run.run_eval arg action stack state tokens in
               arg, x
       | ReduceNone ->
            let v, loc, arg, x = run.run_lexer arg in
            let () =
               if !debug_parse then
                  eprintf "State %d: Read token: %a@." state pp_print_symbol v
            in
               pda_lookahead gram run arg stack state (v, loc, x)

   and pda_goto_no_lookahead gram run arg loc stack state name x =
      let action =
         try VarTable.find run.run_states.(state).pda_delta name with
            Not_found ->
               parse_error loc gram run stack state name
      in
         match action with
            ShiftAction new_state
          | GotoAction new_state ->
               if !debug_parse then
                  eprintf "State %d: production %a: goto %d (no lookahead)@." (**)
                     state pp_print_symbol name new_state;
               let stack = (state, loc, x) :: stack in
                  pda_no_lookahead gram run arg stack new_state
          | ErrorAction
          | ReduceAction _
          | AcceptAction ->
               eprintf "pda_goto_no_lookahead: illegal action: %a@." pp_print_pda_action action;
               raise (Invalid_argument "pda_goto_no_lookahead")

   let parse gram pda start lexer eval arg =
      let { pda_states        = states;
            pda_start_states  = start_states
          } = pda
      in
      let run =
         { run_states        = states;
           run_lexer         = lexer;
           run_eval          = eval
         }
      in
      let start =
         try SymbolTable.find start_states start with
            Not_found ->
               raise (Failure ("not a start symbol: " ^ to_string start))
      in
         try pda_no_lookahead gram run arg [] start with
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

   let get_start info =
      SymbolSet.to_list info.parse_grammar.gram_start_symbols

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

   let union info1 info2 =
      let changed, gram = union_grammar info1.parse_grammar info2.parse_grammar in
         if changed then
            { parse_grammar = gram; parse_pda = None }
         else
            info1

   let pda_of_info info =
      match info.parse_pda with
         Some pda ->
            pda
       | None ->
            let pda = create info.parse_grammar in
               info.parse_pda <- Some pda;
               pda

   let parse info start lexer eval =
      parse info.parse_grammar (pda_of_info info) start lexer eval

   let compile info =
      ignore (pda_of_info info)

   let build info debug =
      let prev_debug = !debug_parse in
      let () = debug_parse := debug in
      let pda = create info.parse_grammar in
         debug_parse := prev_debug;
         info.parse_pda <- Some pda

   let pp_print_parser buf info =
      pp_print_grammar buf info.parse_grammar

   let hash info =
      Hashtbl.hash_param max_int max_int info.parse_grammar
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
      let prec_table = PrecTable.add prec_table prec_min (NoneAssoc, 0) in
      let prec_table = PrecTable.add prec_table prec_max (NoneAssoc, 1) in
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
