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

(************************************************************************
 * Tools for profiling.
 *)
let time_start () =
   Unix.gettimeofday(), Unix.times ()

let time_print debug t1 t2 =
   if !debug_parsetiming then
      let now1, t1 = t1 in
      let now2, t2 = t2 in
      let now3 = Unix.gettimeofday () in
      let t3 = Unix.times () in
      let total = now3 -. now1 in
      let utime = t3.Unix.tms_utime -. t1.Unix.tms_utime in
      let stime = t3.Unix.tms_stime -. t1.Unix.tms_stime in
      let diff_total = now3 -. now2 in
      let diff_utime = t3.Unix.tms_utime -. t2.Unix.tms_utime in
      let diff_stime = t3.Unix.tms_stime -. t2.Unix.tms_stime in
         eprintf "Time: %2.2f real %2.2f user %2.2f sys; %2.2f real %2.2f user %2.2f sys (%s)@." (**)
            diff_total diff_utime diff_stime total utime stime debug;
         now3, t3
   else
      t1

(************************************************************************
 * A generic hash module to make comparisons faster.
 *)

(*
 * The client needs to provide these functions.
 *)
module type HashArgSig =
sig
   type t

   (* For debugging *)
   val debug : string

   (* The client needs to provide hash and comparison functions *)
   val hash : t -> int
   val compare : t -> t -> int
end

(*
 * This is what we get.
 *)
module type HashSig =
sig
   type elt
   type t

   (* Creation *)
   val create : elt -> t
   val get : t -> elt

   (* Hash code *)
   val hash : t -> int

   (* Comparison *)
   val compare : t -> t -> int
end

(*
 * Make a hash item.
 *)
module MakeHash (Arg : HashArgSig)
: HashSig with type elt = Arg.t =
struct
   type elt = Arg.t

   type t = int * elt

   let create x =
      Arg.hash x, x

   let get (_, x) =
      x

   let hash (i, _) =
      i

   let compare ((i1 : int), x1) ((i2 : int), x2) =
      if i1 = i2 then
         Arg.compare x1 x2
      else if i1 < i2 then
         -1
      else
         1
end;;

(*
 * This provides hash-consing.
 *)
module MakeHashCons (Arg : HashArgSig)
: sig
   include HashSig

   (* Create from a hashed value *)
   val icreate : MakeHash(Arg).t -> t

   (* State management *)
   type state
   val state : state
   val set_state : state -> unit
  end
  with type elt = Arg.t =
struct
   type elt = Arg.t

   type t = int

   module Key = MakeHash (Arg);;
   module KeyTable = Lm_map.LmMake (Key);;

   (*
    * We need both directions.
    *)
   type state =
      { mutable key_table : int KeyTable.t;
        mutable int_table : Key.t IntTable.t
      }

   let state =
      { key_table = KeyTable.empty;
        int_table = IntTable.empty
      }

   let set_state state1 =
      state.key_table <- state1.key_table;
      state.int_table <- state1.int_table

   let icreate item =
      try KeyTable.find state.key_table item with
         Not_found ->
            let index = KeyTable.cardinal state.key_table in
               state.key_table <- KeyTable.add state.key_table item index;
               state.int_table <- IntTable.add state.int_table index item;
               index

   let create x =
      icreate (Key.create x)

   let get index =
      try Key.get (IntTable.find state.int_table index) with
         Not_found ->
            eprintf "MakeHash.get: %s: failed on index %d@." Arg.debug index;
            eprintf "\tTable has %d entries@." (KeyTable.cardinal state.key_table);
            raise (Invalid_argument "MakeHash.get")

   let hash index =
      index

   let compare index1 index2 =
      index1 - index2
end;;

(*
 * The default function for combinding hash values.
 * XXX: JYH: we should try using a smarter hash function.
 *)
let hash_combine i1 i2 =
    (i1 lsl 2) lxor (i1 lsr 2) lxor i2

(************************************************************************
 * Sorter.
 *)
module type SortArg =
sig
   type key
   type elt

   val name : elt -> key
   val next : elt -> key list

   val compare : key -> key -> int
end

module type SortSig =
sig
   type elt

   val sort : elt list -> elt list
end

module MakeSort (Arg : SortArg) : SortSig with type elt = Arg.elt =
struct
   type key = Arg.key
   type elt = Arg.elt

   module KeyCompare =
   struct
      type t = Arg.key
      let compare = Arg.compare
   end;;

   module KeySet = Lm_set.LmMake (KeyCompare);;
   module KeyTable = Lm_map.LmMake (KeyCompare);;

   (*
    * Build a graph from an input list.
    *)
   type node =
      { node_name : key;
        node_next : KeySet.t;
        node_item : elt
      }

   let build_graph nodes =
      let domain =
         List.fold_left (fun domain node ->
               KeySet.add domain (Arg.name node)) KeySet.empty nodes
      in
         List.fold_left (fun graph node ->
               let name = Arg.name node in
               let next =
                  List.fold_left (fun next node ->
                        if KeySet.mem domain node then
                           KeySet.add next node
                        else
                           next) KeySet.empty (Arg.next node)
               in
               let node =
                  { node_name = name;
                    node_next = next;
                    node_item = node
                  }
               in
                  KeyTable.add graph name node) KeyTable.empty nodes

   (*
    * Find the roots if there are any.
    * If there are none, just pick a node at random.
    *)
   let roots graph nodes =
      let roots =
         KeySet.fold (fun roots node ->
               let node = KeyTable.find graph node in
                  KeySet.diff roots node.node_next) nodes nodes
      in
         (* If the graph is cyclic, just choose the first node *)
         if KeySet.is_empty roots then
            KeySet.singleton (KeySet.choose nodes)
         else
            roots

   (*
    * Produce a sort in DFS order.
    *)
   let rec dfs_sort_node graph marked items next node =
      let next = KeySet.remove next node in
         if KeySet.mem marked node then
            marked, items, next
         else
            let marked = KeySet.add marked node in
            let node = KeyTable.find graph node in
            let marked, items, next = dfs_sort_nodes graph marked items next node.node_next in
               marked, node.node_item :: items, next

   and dfs_sort_nodes graph marked items next nodes =
      KeySet.fold (fun (marked, items, next) node ->
            dfs_sort_node graph marked items next node) (marked, items, next) nodes

   (*
    * The tree may have disconnected components,
    * so repeat until done.
    *)
   let rec dfs_sort graph marked items nodes =
      if KeySet.is_empty nodes then
         items
      else
         let roots = roots graph nodes in
         let marked, items, nodes = dfs_sort_nodes graph marked items nodes roots in
            dfs_sort graph marked items nodes

   (*
    * Main sort functions.
    *)
   let sort nodes =
      let graph = build_graph nodes in
      let nodes =
         List.fold_left (fun nodes node ->
               KeySet.add nodes (Arg.name node)) KeySet.empty nodes
      in
         dfs_sort graph KeySet.empty [] nodes
end

(************************************************************************
 * Precedences.
 *)
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
   val hash_symbol : symbol -> int
   val compare_symbol : symbol -> symbol -> int

   (* Names of semantic actions *)
   type action

   (* For debugging *)
   val pp_print_action : out_channel -> action -> unit

   (* For set and table building *)
   val hash_action : action -> int
   val compare_action : action -> action -> int
end

module MakeParser (Arg : ParserArg) (Precedence : PrecedenceArg) =
struct
   open Precedence

   (************************************************************************
    * Types.
    *)

   (*
    * Type of lexing tokens.
    *)
   type ('a, 'b) lexer = 'a -> Arg.symbol * loc * 'a * 'b
   type ('a, 'b) eval =
      'a ->                     (* The argument *)
      Arg.action ->             (* The name of the action *)
      loc ->                    (* Location of the production *)
      'b list ->                (* The arguments to the action *)
      'a * 'b                   (* The result of the semantic action *)

   (************************************************************************
    * Internal versions of types.
    *)
   module VarArg =
   struct
      type t = Arg.symbol

      let debug = "Var"
      let hash = Arg.hash_symbol
      let compare = Arg.compare_symbol
   end;;

   module Var = MakeHash (VarArg);;
   module VarSet    = Lm_set.LmMake (Var);;
   module VarTable  = Lm_map.LmMake (Var);;
   module VarMTable = Lm_map.LmMakeList (Var);;

   module IVar = MakeHashCons (VarArg);;
   module IVarSet = Lm_set.LmMake (IVar);;
   module IVarTable = Lm_map.LmMake (IVar);;
   module IVarMTable = Lm_map.LmMakeList (IVar);;

   type var = Var.t
   type ivar = IVar.t

   let eof = IVar.create Arg.eof

   (*
    * Also hash the actions.
    *)
   module ActionArg =
   struct
      type t = Arg.action

      let debug = "Action"
      let hash = Arg.hash_action
      let compare = Arg.compare_action
   end;;

   module Action = MakeHash (ActionArg);;
   module ActionSet = Lm_set.LmMake (Action);;

   module IAction = MakeHashCons (ActionArg);;
   module IActionSet = Lm_set.LmMake (IAction);;

   type action = Action.t
   type iaction = IAction.t

   (*
    * A production item is represents a production with
    * a current position.
    *)
   type prod_item_core =
      { prod_item_name   : ivar;
        prod_item_left   : ivar list;       (* Reverse order *)
        prod_item_right  : ivar list;
        prod_item_action : iaction;
        prod_item_prec   : precedence
      }

   (*
    * Hash utilities.
    *)
   let ivar_list_hash hash vars =
      List.fold_left (fun hash v ->
            hash_combine hash (IVar.hash v)) hash vars

   let rec ivar_list_compare vars1 vars2 =
      match vars1, vars2 with
         v1 :: vars1, v2 :: vars2 ->
            let cmp = IVar.compare v1 v2 in
               if cmp = 0 then
                  ivar_list_compare vars1 vars2
               else
                  cmp
       | [], [] ->
            0
       | _ :: _, [] ->
            1
       | [], _ :: _ ->
            -1

   module ProdItemArg =
   struct
      type t = prod_item_core

      let debug = "ProdItem"

      let hash item =
         let { prod_item_name   = name;
               prod_item_left   = left;
               prod_item_right  = right;
               prod_item_action = action
             } = item
         in
         let hash = hash_combine (IVar.hash name) (IAction.hash action) in
         let hash = ivar_list_hash hash left in
         let hash = ivar_list_hash hash right in
            hash

      let compare item1 item2 =
         let { prod_item_name   = name1;
               prod_item_left   = left1;
               prod_item_right  = right1;
               prod_item_action = action1;
               prod_item_prec   = prec1
             } = item1
         in
         let { prod_item_name   = name2;
               prod_item_left   = left2;
               prod_item_right  = right2;
               prod_item_action = action2;
               prod_item_prec   = prec2
             } = item2
         in
         let cmp = IVar.compare name1 name2 in
            if cmp = 0 then
               let cmp = IAction.compare action1 action2 in
                  if cmp = 0 then
                     let cmp = ivar_list_compare left1 left2 in
                        if cmp = 0 then
                           let cmp = ivar_list_compare right1 right2 in
                              if cmp = 0 then
                                 Pervasives.compare prec1 prec2
                              else
                                 cmp
                        else
                           cmp
                  else
                     cmp
            else
               cmp
   end

   module ProdItem      = MakeHashCons (ProdItemArg);;
   module ProdItemSet   = Lm_set.LmMake (ProdItem);;
   module ProdItemTable = Lm_map.LmMake (ProdItem);;

   type prod_item = ProdItem.t

   (*
    * An LR(0) state is a set of ProdItems, and
    * a closure, which is a set of nonterminals.
    *)
   type info_state =
      { info_state_items   : ProdItemSet.t;
        info_state_closure : IVarSet.t
      }

   module StateArg =
   struct
      type t = info_state

      let debug = "State"

      (*
       * We don't need to hash or compare the closure,
       * because it is uniquely determined by the items.
       *)
      let hash state =
         ProdItemSet.fold (fun hash item ->
               hash_combine hash (ProdItem.hash item)) 0 state.info_state_items

      let compare state1 state2 =
         ProdItemSet.compare state1.info_state_items state2.info_state_items
   end;;

   module State = MakeHashCons (StateArg);;
   module StateSet = Lm_set.LmMake (State);;
   module StateTable = Lm_map.LmMake (State);;

   (************************************************
    * The grammar.
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
   (* %%MAGICBEGIN%% *)

   (*
    * A single production.
    *)
   type prod =
      { prod_name   : var;
        prod_right  : var list;
        prod_action : action;
        prod_prec   : precedence
      }
   (*
    * A grammar has a set of symbols, productions,
    * and precedences.
    *)
   type grammar =
      { gram_prod          : prod VarMTable.t;
        gram_prec          : precedence VarTable.t;
        gram_prec_table    : Precedence.t;
        gram_start_symbols : VarSet.t
      }

   (************************************************
    * The PDA.
    *)

   (*
    * An action is shift, reduce, or accept.
    *)
   type pda_action =
      ReduceAction of iaction * ivar * int  (* semantic action, production name, #args *)
    | ShiftAction  of int                   (* goto state *)
    | GotoAction   of int
    | AcceptAction
    | ErrorAction

   (*
    * We may reduce states without lookahead,
    * and we may accept.
    *)
   type pda_reduce =
      ReduceNone
    | ReduceNow    of iaction * ivar * int
    | ReduceAccept of iaction * ivar * int

   (*
    * The PDA transition table.
    *
    * The pda_item is *purely* for debugging, so access
    * does not have to be fast.
    *)
   type pda_item =
      { pda_item_left      : ivar list;  (* Reverse order *)
        pda_item_right     : ivar list
      }

   type pda_state_info =
      { pda_items     : pda_item list;
        pda_next      : IVarSet.t
      }

   type pda_state =
      { pda_delta   : pda_action IVarTable.t;
        pda_reduce  : pda_reduce;
        pda_info    : pda_state_info
      }

   type pda =
      { pda_start_states    : int IVarTable.t;
        pda_states          : pda_state array;
        pda_ivar_state      : IVar.state;
        pda_iaction_state   : IAction.state;
        pda_prod_item_state : ProdItem.state;
        pda_state_state     : State.state
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
    * Lookahead expressions.
    * LookAheadConst vars: the vars are spontaneously generated
    * LoadAheadProp vars: the vars are spontaneously generated, and the item vars are propagated.
    *)
   type lookahead =
      LookAheadConst of IVarSet.t
    | LookAheadProp of IVarSet.t

   (*
    * A head summary is a summary of the
    * transition function and lookaheads for a production.
    *)
   type head_info =
      { head_delta     : ProdItemSet.t IVarTable.t;
        head_lookahead : lookahead IVarTable.t
      }

   (*
    * The info for constructing the PDA.
    *    info_gram     : the grammar
    *    info_nullable : the nonterminals that derive epsilon
    *    info_first    : the terminals that may start a production
    *)
   type info =
      { info_grammar             : grammar;
        info_prod                : ProdItem.t list IVarTable.t;
        info_start_symbols       : IVarSet.t;
        info_prec                : precedence IVarTable.t;
        info_nullable            : IVarSet.t;
        info_first               : IVarSet.t IVarTable.t;
        info_head_table          : head_info IVarTable.t
      }

   (*
    * A state element is a set of items, with lookaheads for each.
    *)
   type info_item =
      { info_item_index   : int;
        info_item_table   : IVarSet.t ProdItemTable.t
      }

   (*
    * A prop_edge is used to specify that we should
    * propagate from one item to another.
    *)
   type prop_edge =
      { prop_edge_src   : (int * int);          (* state, item *)
        prop_edge_dst   : (int * int) list      (* state, item *)
      }

   module PropEdgeSortArg =
   struct
      type key = int * int
      type elt = prop_edge

      let name edge =
         edge.prop_edge_src

      let next edge =
         edge.prop_edge_dst

      let compare ((i11 : int), (i12 : int)) ((i21 : int), (i22 : int)) =
         if i11 < i21 then
            -1
         else if i11 > i21 then
            1
         else if i12 < i22 then
            -1
         else if i12 > i22 then
            1
         else
            0
   end;;

   module PropEdgeSort = MakeSort (PropEdgeSortArg);;

   (*
    * The prop_entry is the lookahead we are computing.
    *)
   type prop_entry =
      { prop_prod_item       : prod_item;
        mutable prop_changed : bool;
        mutable prop_vars    : IVarSet.t
      }

   (************************************************************************
    * Printing and errors.
    *)
   let string_of_var v =
      Arg.to_string (Var.get v)

   let pp_print_var buf v =
      Arg.pp_print_symbol buf (Var.get v)

   let rec pp_print_vars buf vl =
      List.iter (fun v -> fprintf buf " %a" pp_print_var v) vl

   let pp_print_var_set buf s =
      VarSet.iter (fun v ->
            fprintf buf "@ %a" pp_print_var v) s

   let pp_print_var_table buf table =
      VarTable.iter (fun v s ->
            fprintf buf "@ @[<b 3>%a:%a@]" (**)
               pp_print_var v
               pp_print_var_set s) table

   let pp_print_action buf action =
      Arg.pp_print_action buf (Action.get action)

   let string_of_ivar v =
      Arg.to_string (IVar.get v)

   let pp_print_ivar buf v =
      Arg.pp_print_symbol buf (IVar.get v)

   let rec pp_print_ivars buf vl =
      List.iter (fun v -> fprintf buf " %a" pp_print_ivar v) vl

   let pp_print_ivar_set buf s =
      IVarSet.iter (fun v ->
            fprintf buf "@ %a" pp_print_ivar v) s

   let pp_print_ivar_table buf table =
      IVarTable.iter (fun v s ->
            fprintf buf "@ @[<b 3>%a:%a@]" (**)
               pp_print_ivar v
               pp_print_ivar_set s) table

   let pp_print_iaction buf action =
      Arg.pp_print_action buf (IAction.get action)

   let pp_print_prod gram buf item =
      let { prod_action = action;
            prod_prec   = pre;
            prod_name   = name;
            prod_right  = right
          } = item
      in
         fprintf buf "%a[%a]    %a ::= %a" (**)
            pp_print_action action
            (Precedence.pp_print_prec gram.gram_prec_table) pre
            pp_print_var name
            pp_print_vars right

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
                  pp_print_var v
                  (Precedence.pp_print_prec prec_table) pre) precs;
         VarSet.iter (fun v ->
               fprintf buf "@ start %a" pp_print_var v) starts;
         VarMTable.iter_all (fun _ prods ->
               List.iter (fun prod -> fprintf buf "@ %a" (pp_print_prod gram) prod) prods) prods;
         fprintf buf "@]"

   let pp_print_pda_action buf action =
      match action with
         ReduceAction (action, _, _) ->
            fprintf buf "reduce %a" pp_print_iaction action
       | ShiftAction id ->
            fprintf buf "shift %d" id
       | GotoAction id ->
            fprintf buf "goto %d" id
       | ErrorAction ->
            pp_print_string buf "error"
       | AcceptAction  ->
            pp_print_string buf "accept"

   let pp_print_pda_actions info buf actions =
      IVarTable.iter (fun v action ->
            fprintf buf "@ %a: %a" pp_print_ivar v pp_print_pda_action action) actions

   let pp_print_prod_item_core gram buf item =
      let { prod_item_action = action;
            prod_item_prec   = pre;
            prod_item_name   = name;
            prod_item_left   = left;
            prod_item_right  = right
          } = item
      in
         fprintf buf "%a[%a]    %a ::=%a .%a" (**)
            pp_print_iaction action
            (Precedence.pp_print_prec gram.gram_prec_table) pre
            pp_print_ivar name
            pp_print_ivars (List.rev left)
            pp_print_ivars right

   let pp_print_prod_item gram buf item =
      pp_print_prod_item_core gram buf (ProdItem.get item)

   let pp_print_closure info buf closure =
      let gram = info.info_grammar in
         fprintf buf "@[<v 3>Closure:";
         ProdItemTable.iter (fun prod_item lookahead ->
               if IVarSet.is_empty lookahead then
                  fprintf buf "@ @[<hv 3>%a@]" (pp_print_prod_item info.info_grammar) prod_item
               else
                  IVarSet.iter (fun v ->
                        fprintf buf "@ @[<hv 3>%a # %a@]" (**)
                           (pp_print_prod_item info.info_grammar) prod_item pp_print_ivar v) lookahead) closure;
         fprintf buf "@]"

   let pp_print_info_item info buf info_item =
      let gram = info.info_grammar in
      let { info_item_index = index;
            info_item_table = table
          } = info_item
      in
         fprintf buf "@[<v 3>State %d:" index;
         ProdItemTable.iter (fun prod_item lookahead ->
               fprintf buf "@ %a @[<b 2>#%a@]" (pp_print_prod_item info.info_grammar) prod_item pp_print_ivar_set lookahead) table;
         fprintf buf "@]"

   let pp_print_info buf info =
      let { info_grammar = gram;
            info_nullable = nullable;
            info_first = first
          } = info
      in
         fprintf buf "@[<v 0>%a" pp_print_grammar gram;
         fprintf buf "@ @[<b 3>Nullable:%a@]" pp_print_ivar_set nullable;
         fprintf buf "@ @[<hv 3>First:%a@]" pp_print_ivar_table first;
         fprintf buf "@]"

   let pp_print_lookahead gram buf look =
      match look with
         LookAheadConst set ->
            fprintf buf "@[<b 3>const%a@]" pp_print_ivar_set set
       | LookAheadProp set ->
            fprintf buf "@[<b 3>prop%a@]" pp_print_ivar_set set

   (*
    * Error messages.
    *)
   let shift_reduce_conflict info info_item actions v id prod_item =
      let gram = info.info_grammar in
         eprintf "shift/reduce conflict on %a: shift %d, reduce %a@." (**)
            pp_print_ivar v
            id
            pp_print_iaction (ProdItem.get prod_item).prod_item_action;
         if not !debug_parse_conflict_is_warning && not !debug_parsegen then
            eprintf "@[<v 0>%a@ @[<v 3>%a@]@ @]@." (**)
               (pp_print_info_item info) info_item
               (pp_print_pda_actions info) actions;
         if not !debug_parse_conflict_is_warning then
            raise (Invalid_argument "Lm_parser.shift_reduce_conflict\n\tset MP_DEBUG=parse_conflict_is_warning to ignore this error")

   let reduce_reduce_conflict info info_item actions v action1 action2 =
      let gram = info.info_grammar in
         eprintf "reduce/reduce conflict on %a: reduce %a, reduce %a@." (**)
            pp_print_ivar v
            pp_print_iaction action1
            pp_print_iaction action2;
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
        gram_start_symbols = VarSet.empty
      }

   (*
    * Add a start symbol.
    *)
   let add_start gram sym =
      { gram with gram_start_symbols = VarSet.add gram.gram_start_symbols (Var.create sym) }

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
           prod_right   = rhs;
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
               List.fold_left (fun actions prod ->
                     let action = prod.prod_action in
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
                        if ActionSet.mem actions action then
                           changed, prods
                        else
                           let prod = { prod with prod_prec = PrecTable.find prec_translate pre } in
                              true, VarMTable.add prods name prod) (changed, prods) prodlist) (false, prod1) prod2
      in

      (* Union of the start symbols *)
      let start = VarSet.union start1 start2 in

      (* Has anything changed? *)
      let changed =
         changed
         || (VarTable.cardinal precs <> VarTable.cardinal prec1)
         || (VarSet.cardinal start <> VarSet.cardinal start1)
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
   let nullable prods =
      let step nullable prods =
         IVarTable.fold (fun nullable v prods ->
               if IVarSet.mem nullable v then
                  nullable
               else if List.exists (fun prod -> List.for_all (IVarSet.mem nullable) (ProdItem.get prod).prod_item_right) prods then
                  IVarSet.add nullable v
               else
                  nullable) nullable prods
      in
      let rec fixpoint nullable prods =
         let nullable' = step nullable prods in
            if IVarSet.cardinal nullable' <> IVarSet.cardinal nullable then
               fixpoint nullable' prods
            else
               nullable
      in
         fixpoint IVarSet.empty prods

   (*
    * Find the sets of first symbols that can start productions.
    *)
   let rec first_rhs nullable first set rhs =
      match rhs with
         v :: rhs ->
            let set = IVarSet.union set (IVarTable.find first v) in
               if IVarSet.mem nullable v then
                  first_rhs nullable first set rhs
               else
                  set
       | [] ->
            set

   let first prods nullable =
      let step first prods =
         IVarTable.fold (fun (first, changed) _ prods ->
               List.fold_left (fun (first, changed) prod ->
                     let { prod_item_name = x;
                           prod_item_right = rhs
                         } = ProdItem.get prod
                     in
                     let set = IVarTable.find first x in
                     let set' = first_rhs nullable first set rhs in
                     let set, changed =
                        if changed || IVarSet.cardinal set' <> IVarSet.cardinal set then
                           set', true
                        else
                           set, false
                     in
                     let first = IVarTable.add first x set in
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
      let vars =
         IVarTable.fold (fun vars v prods ->
               let vars = IVarSet.add vars v in
                  List.fold_left (fun vars prod ->
                        List.fold_left IVarSet.add vars (ProdItem.get prod).prod_item_right) vars prods) IVarSet.empty prods
      in
      let first =
         IVarSet.fold (fun first v ->
               if IVarTable.mem prods v then
                  IVarTable.add first v IVarSet.empty
               else
                  IVarTable.add first v (IVarSet.singleton v)) IVarTable.empty vars
      in
         fixpoint first prods

   (************************************************************************
    * LR(0) construction.
    *)

   (*
    * Get the set of first symbols that can begin a list.
    *)
   let lookahead info rhs =
      let { info_first = first;
            info_nullable = nullable
          } = info
      in
      let rec search set rhs =
         match rhs with
            v :: rhs ->
               let set = IVarSet.union (IVarTable.find first v) set in
                  if IVarSet.mem nullable v then
                     search set rhs
                  else
                     LookAheadConst set
          | [] ->
               LookAheadProp set
      in
         search IVarSet.empty rhs

   (*
    * Concatenate lookahead sets.
    *)
   let lookahead_concat look1 look2 =
      match look1, look2 with
         LookAheadConst _, _ ->
            look1
       | LookAheadProp set1, LookAheadConst set2 ->
            LookAheadConst (IVarSet.union set1 set2)
       | LookAheadProp set1, LookAheadProp set2 ->
            LookAheadProp (IVarSet.union set1 set2)

   (*
    * Two different paths for lookahead.
    *)
   let lookahead_union look1 look2 =
      match look1, look2 with
         LookAheadConst set1, LookAheadConst set2 ->
            LookAheadConst (IVarSet.union set1 set2)
       | LookAheadProp set1, LookAheadConst set2
       | LookAheadConst set1, LookAheadProp set2
       | LookAheadProp set1, LookAheadProp set2 ->
            LookAheadProp (IVarSet.union set1 set2)

   (*
    * Comparison.
    *)
   let lookahead_equal look1 look2 =
      match look1, look2 with
         LookAheadConst set1, LookAheadConst set2
       | LookAheadProp set1, LookAheadProp set2 ->
            IVarSet.equal set1 set2
       | LookAheadConst _, LookAheadProp _
       | LookAheadProp _, LookAheadConst _ ->
            false

   (*
    * Split into a pair.
    *)
   let lookahead_pair look =
      match look with
         LookAheadConst set ->
            false, set
       | LookAheadProp set ->
            true, set

   (************************************************
    * Produce the derivation table for items where
    * the dot is at the head.
    *
    * We want a transition table, as well as lookaheads.
    *
    * The transition table gives a set of transitions
    * for nonterminal (symbol -> symbol -> ProdItemSet.t),
    * where and entry (v1 -> v2 -> items) states that:
    *    if looking at v1,
    *    you can goto on v2,
    *    with the resulting items.
    *
    * The lookahead gives a similar table.  It provides
    * the lookahead if looking at v1, and hence v2, because
    * v1 has an item that begins with v2.
    *)

   (*
    * Compute the transition function and lookahead table
    * for an item.
    *)
   let build_head_item info delta lookaheads item =
      let core = ProdItem.get item in
         match core.prod_item_right with
            v :: right ->
               let core =
                  { core with prod_item_left = [v];
                              prod_item_right = right
                  }
               in
               let item = ProdItem.create core in
               let delta =
                  IVarTable.filter_add delta v (fun items ->
                        match items with
                           Some items ->
                              ProdItemSet.add items item
                         | None ->
                              ProdItemSet.singleton item)
               in
               let look1 = lookahead info right in
               let lookaheads =
                  if IVarTable.mem info.info_prod v then
                     IVarTable.filter_add lookaheads v (fun look2 ->
                           match look2 with
                              Some look2 ->
                                 lookahead_union look1 look2
                            | None ->
                                 look1)
                  else
                     lookaheads
               in
                  delta, lookaheads
          | [] ->
               delta, lookaheads

   let build_head_items info items =
      List.fold_left (fun (delta, lookaheads) item ->
            build_head_item info delta lookaheads item) (IVarTable.empty, IVarTable.empty) items

   (*
    * Solve the lookahead functions.
    * This is a fixpoint, but it should be pretty cheap.
    *)
   let build_lookahead_item info table v =
      (* Fixpoint *)
      let step venv =
         IVarTable.fold (fun (venv, changed) v e1 ->
               let next = IVarTable.find table v in
                  IVarTable.fold (fun (venv, changed) v e2 ->
                        let e = lookahead_concat e2 e1 in
                           try
                              let e_old = IVarTable.find venv v in
                              let e_new = lookahead_union e_old e in
                                 if lookahead_equal e_old e_new then
                                    venv, changed
                                 else
                                    IVarTable.add venv v e_new, true
                           with
                              Not_found ->
                                 IVarTable.add venv v e, true) (venv, changed) next) (venv, false) venv
      in
      let rec fixpoint venv =
         let venv, changed = step venv in
            if changed then
               fixpoint venv
            else
               venv
      in
         fixpoint (IVarTable.add IVarTable.empty v (LookAheadProp IVarSet.empty))

   let build_lookaheads info table =
      IVarTable.mapi (fun v _ ->
            build_lookahead_item info table v) table

   (*
    * Main function.
    *)
   let build_head_table info start now =
      let delta_table, look_table =
         IVarTable.fold (fun (delta_table, look_table) v items ->
               let delta, lookaheads = build_head_items info items in
               let delta_table = IVarTable.add delta_table v delta in
               let look_table = IVarTable.add look_table v lookaheads in
                  delta_table, look_table) (IVarTable.empty, IVarTable.empty) info.info_prod
      in
      let now = time_print "Head items" start now in
      let lookaheads = build_lookaheads info look_table in
      let now = time_print "Lookaheads" start now in
      let table =
         IVarTable.mapi (fun v delta ->
               { head_delta = delta;
                 head_lookahead = IVarTable.find lookaheads v
               }) delta_table
      in
         if !debug_parsegen then begin
            eprintf "@[<v 3>Head table:";
            IVarTable.iter (fun v head ->
                  let { head_delta = delta;
                        head_lookahead = look
                      } = head
                  in
                     eprintf "@ @ @[<v 3>%a =" pp_print_ivar v;
                     IVarTable.iter (fun v items ->
                           eprintf "@ @[<hv 3>%a ->" pp_print_ivar v;
                           ProdItemSet.iter (fun item ->
                                 eprintf "@ %a" (pp_print_prod_item info.info_grammar) item) items;
                           eprintf "@]") delta;
                     eprintf "@]@ @[<v 3>Lookahead:";
                     IVarTable.iter (fun v look ->
                           eprintf "@ @[<b 3>%a = %a@]" pp_print_ivar v (pp_print_lookahead info.info_grammar) look) look;
                     eprintf "@]") table;
            eprintf "@]@."
         end;
         now, table

   (************************************************
    * Producing the state table.
    *)

   (*
    * Produce a transition table by shifting.
    * We take a set of items, and produce a IVarTable
    * containing the next states.
    *)
   let shift_items items =
      ProdItemSet.fold (fun delta prod_item ->
            let core = ProdItem.get prod_item in
            let { prod_item_left = left;
                  prod_item_right = right
                } = core
            in
               match right with
                  v :: right ->
                     let core =
                        { core with prod_item_left = v :: left;
                                    prod_item_right = right
                        }
                     in
                     let item = ProdItem.create core in
                        IVarTable.filter_add delta v (fun items ->
                              match items with
                                 Some items ->
                                    ProdItemSet.add items item
                               | None ->
                                    ProdItemSet.singleton item)
                | [] ->
                     delta) IVarTable.empty items

   (*
    * Shift a closure, given the current state.
    * This produces a IVarTable that defines the next
    * state for each symbol.
    *)
   let shift_state info state =
      let core = State.get state in
      let { info_state_items = items;
            info_state_closure = next
          } = core
      in
      let head_table = info.info_head_table in
      let delta = shift_items items in
         IVarSet.fold (fun delta v ->
               let head_delta = (IVarTable.find head_table v).head_delta in
                  IVarTable.fold (fun delta v items ->
                        IVarTable.filter_add delta v (fun current_items ->
                              match current_items with
                                 Some current_items ->
                                    ProdItemSet.union current_items items
                               | None ->
                                    items)) delta head_delta) delta next

   (*
    * A closure is represented by its kernel (all the
    * items where the dot is not at the front), plus
    * the names of all the productions where the dot
    * is at the front.
    *)
   let closure info items =
      let head_table = info.info_head_table in
      let closure =
         ProdItemSet.fold (fun closure item ->
               let core = ProdItem.get item in
                  match core.prod_item_right with
                     v :: _ when IVarTable.mem head_table v ->
                        let look = (IVarTable.find head_table v).head_lookahead in
                           IVarTable.fold (fun closure v _ ->
                                 IVarSet.add closure v) (IVarSet.add closure v) look
                   | _ ->
                        closure) IVarSet.empty items
      in
      let state =
         { info_state_items = items;
           info_state_closure = closure
         }
      in
         State.create state

   (*
    * Add the state to the set of known LR(0) states.
    *)
   let add_state examined unexamined closure =
      try StateTable.find examined closure, unexamined with
         Not_found ->
            try StateTable.find unexamined closure, unexamined with
               Not_found ->
                  let index = StateTable.cardinal examined + StateTable.cardinal unexamined in
                  let unexamined = StateTable.add unexamined closure index in
                     index, unexamined

   (*
    * Compute the transition table, only for shift operations.
    *)
   let shift_closure_count = ref 0

   let build_delta info unexamined =
      let prods = info.info_grammar.gram_prod in

      (* Perform the closure *)
      let rec build shift_table examined unexamined =
         incr shift_closure_count;
         if StateTable.is_empty unexamined then
            shift_table, examined
         else
            (* Move an item from unexamined to examined *)
            let state, index = StateTable.choose unexamined in
            let examined = StateTable.add examined state index in
            let unexamined = StateTable.remove unexamined state in

            (* Compute the goto states *)
            let delta = shift_state info state in
            let goto_table, unexamined =
               IVarTable.fold (fun (goto_table, unexamined) v items ->
                     let state = closure info items in
                     let index, unexamined = add_state examined unexamined state in
                     let goto_table = IVarTable.add goto_table v index in
                        goto_table, unexamined) (IVarTable.empty, unexamined) delta
            in
            let shift_table = IntTable.add shift_table index goto_table in
               build shift_table examined unexamined
      in
         build IntTable.empty StateTable.empty unexamined

   let build_start_state info start_table unexamined start =
      let prods =
         try IVarTable.find info.info_prod start with
            Not_found ->
               raise (Failure ("no such production: " ^ string_of_ivar start))
      in
      let set = List.fold_left ProdItemSet.add ProdItemSet.empty prods in
      let closure = closure info set in
      let state_index, unexamined = add_state StateTable.empty unexamined closure in
      let start_table = IVarTable.add start_table start state_index in
         start_table, unexamined

   let build_state_table info =
      let () =
         if !debug_parsegen then
            eprintf "@[<hv 3>Grammar:@ %a@]@." pp_print_info info
      in
      let start_table, unexamined =
         IVarSet.fold (fun (start_table, unexamined) start ->
               build_start_state info start_table unexamined start) (**)
            (IVarTable.empty, StateTable.empty) info.info_start_symbols
      in
      let shift_table, states = build_delta info unexamined in
         start_table, shift_table, states

   (************************************************************************
    * LALR(1) construction.
    *
    * Once we have the set of LR(0) states, we need to propagate lookahead
    * sets.  For each item in a state, figure out what symbols are propagated
    * and which are spontaneously generated, then perform a fixpoint.
    *)

   (*
    * Give an index to each of the productions in a state.
    *)
   let build_item_indices states =
      StateTable.mapi (fun item index ->
            let core = State.get item in
            let table, _ =
               ProdItemSet.fold (fun (table, index) prod_item ->
                     let table = ProdItemTable.add table prod_item index in
                        table, succ index) (ProdItemTable.empty, 0) core.info_state_items
            in
               table, index) states

   let build_state_index states =
      StateTable.fold (fun table state (prods, index) ->
            IntTable.add table index prods) IntTable.empty states

   (*
    * Build the empty state table.
    *)
   let build_prop_empty info state_table =
      let state_count = IntTable.cardinal state_table in
      let prop_table : prop_entry array array = Array.create state_count [||] in
         IntTable.iter (fun state_index prod_table ->
               let prod_count = ProdItemTable.cardinal prod_table in
               let prop_entry_list =
                  ProdItemTable.fold (fun prop_entry_list prod_item _ ->
                        let prop_entry =
                           { prop_prod_item = prod_item;
                             prop_changed   = true;
                             prop_vars      = IVarSet.empty
                           }
                        in
                           prop_entry :: prop_entry_list) [] prod_table
               in
               let prop_entry_array = Array.of_list (List.rev prop_entry_list) in
                  prop_table.(state_index) <- prop_entry_array) state_table;
         prop_table

   (*
    * Add the propagation info for the initial items.
    *
    * We are looking at an item.
    *     item = left . v1 right
    *
    * Suppose v is a nonterminal, and we have some derivation
    * with head nonterminal v2.
    *
    *     v1 --> . v2 right_1
    *
    * Suppose v2 has the following production.
    *
    *     v2 = X right_2
    *
    * Then the goto(X) state contains an item:
    *
    *     X . right_2
    *
    * We need to propagate lookaheads to this item.
    *)
   let build_prop_head info prop_table state_table goto_table state_index prod_index v1 right1 =
      (* Look up the derivations for v *)
      let head_info = IVarTable.find info.info_head_table v1 in
      let { head_delta = head_delta;
            head_lookahead = look_table
          } = head_info
      in
      let look2 = lookahead info right1 in
         IVarTable.fold (fun prop_items v2 items ->
               ProdItemSet.fold (fun prop_items next_item ->
                     (* Get the lookahead for the item *)
                     let core = ProdItem.get next_item in
                     let look1 = IVarTable.find look_table core.prod_item_name in
                     let look = lookahead_concat look1 look2 in
                     let prop, vars = lookahead_pair look in

                     (* Add the edge *)
                     let next_state_index = IVarTable.find goto_table v2 in
                     let next_table = IntTable.find state_table next_state_index in
                     let next_item_index = ProdItemTable.find next_table next_item in

                     (* Add the edge if we need to propagate *)
                     let prop_items =
                        if prop then
                           (next_state_index, next_item_index) :: prop_items
                        else
                           prop_items
                     in

                     (* Initial propagation *)
                     let prop_entry = prop_table.(next_state_index).(next_item_index) in
                        prop_entry.prop_vars <- IVarSet.union prop_entry.prop_vars vars;
                        prop_items) prop_items items) [] head_delta

   (*
    * Add the propagation info for all the items in a state.
    *
    * Propagate initial items.
    * In addition, if we have an item
    *
    *   item = left . v right
    *
    * then goto(v) contains the item
    *
    *   left v . right
    *
    * Propagate lookaheads to this item.
    *)
   let build_prop_state info prop_table state_table shift_table prop_edges state_index prod_table =
      let goto_table = IntTable.find shift_table state_index in
      let prods = info.info_prod in
         ProdItemTable.fold (fun prop_edges prod_item prod_index ->
               let prod_item_core = ProdItem.get prod_item in
               let { prod_item_left = left;
                     prod_item_right = right
                   } = prod_item_core
               in
                  match right with
                     v :: right ->
                        (* If v is a nonterminal, then also propagate to initial items *)
                        let prop_items =
                           if IVarTable.mem prods v then
                              build_prop_head info prop_table state_table goto_table state_index prod_index v right
                           else
                              []
                        in

                        (* Propagate directly to the next state *)
                        let next_state_index = IVarTable.find goto_table v in
                        let next_item_core =
                           { prod_item_core with prod_item_left = v :: left;
                                                 prod_item_right = right
                           }
                        in
                        let next_item = ProdItem.create next_item_core in
                        let next_table = IntTable.find state_table next_state_index in
                        let next_item_index = ProdItemTable.find next_table next_item in

                        (* Add the edges *)
                        let prop_edge =
                           { prop_edge_src = (state_index, prod_index);
                             prop_edge_dst = (next_state_index, next_item_index) :: prop_items
                           }
                        in
                           prop_edge :: prop_edges
                   | [] ->
                        prop_edges) prop_edges prod_table

   (*
    * Now construct a propagation network.
    * Each state is represented as an array of production indices,
    * each with a propagation entry to another item identified
    * by (state, index).
    *)
   let build_prop_table info shift_table state_table =
      let prop_table = build_prop_empty info state_table in
      let prop_edges = IntTable.fold (build_prop_state info prop_table state_table shift_table) [] state_table in
         prop_table, prop_edges

   (*
    * Add the eof symbol for the start states.
    *)
   let set_start_lookahead start_table prop_table =
      let eof_set = IVarSet.singleton eof in
         IVarTable.iter (fun _ state_index ->
               Array.iter (fun prop_entry ->
                     prop_entry.prop_vars <- IVarSet.union prop_entry.prop_vars eof_set) prop_table.(state_index)) start_table

   (*
    * The fixpoint is a forward-dataflow problem.
    * Try to order the states so that dependencies are in
    * order.  Use depth-first-search to find an approximate
    * order.
    *)
   let propagate_order prop_edges =
      PropEdgeSort.sort prop_edges

   (*
    * Now solve the lookahead fixpoint.
    *)
   let step_count = ref 0
   let fixpoint_count = ref 0

   let propagate_lookahead prop_table prop_edges =
      let step () =
         incr step_count;
         List.fold_left (fun changed prop_edge ->
               let { prop_edge_src = (state_index, item_index);
                     prop_edge_dst = dst
                   } = prop_edge
               in
               let item1 = prop_table.(state_index).(item_index) in
                  if item1.prop_changed then
                     let _ = item1.prop_changed <- false in
                     let vars = item1.prop_vars in
                        List.fold_left (fun changed (next_state, next_item) ->
                              let item2 = prop_table.(next_state).(next_item) in
                              let vars2 = item2.prop_vars in
                              let vars2' = IVarSet.union vars2 item1.prop_vars in
                                 if IVarSet.cardinal vars2' = IVarSet.cardinal vars2 then
                                    changed
                                 else begin
                                    item2.prop_changed <- true;
                                    item2.prop_vars <- vars2';
                                    true
                                 end) changed dst
                  else
                     changed) false prop_edges
      in
      let rec fixpoint () =
         incr fixpoint_count;
         if step () then
            fixpoint ()
      in
         fixpoint ()

   (*
    * Rebuild the traditional table from the propagation network.
    * The traditional table is an (info_item IntTable.t).
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
      let prods = info.info_prod in
         IntTable.map (fun goto_table ->
               IVarTable.mapi (fun v index ->
                     if IVarTable.mem prods v then
                        GotoAction index
                     else
                        ShiftAction index) goto_table) shift_table

   (*
    * Construct the LALR(1) table from the LR(0) table.
    *)
   let build_lalr_table info start now =
      let start_table, shift_table, states = build_state_table info in
      let now = time_print "State table" start now in
      let state_table = build_item_indices states in
      let state_table = build_state_index state_table in
      let now = time_print "Indexes" start now in
      let prop_table, prop_edges = build_prop_table info shift_table state_table in
      let now = time_print "Propagation table" start now in
      let () = set_start_lookahead start_table prop_table in
      let prop_edges = propagate_order prop_edges in
      let now = time_print "Propagation ordering" start now in

      (* Take the fixpoint *)
      let () = propagate_lookahead prop_table prop_edges in
      let now = time_print "Fixpoint" start now in
      let () =
         if !debug_parsetiming then
            eprintf "Fixpoint in %d iterations, %d steps@." !fixpoint_count !step_count
      in

      (* Reconstruct the tables *)
      let states = rebuild_state_table prop_table in
      let trans_table = rebuild_trans_table info shift_table in
      let now = time_print "LALR reconstruction" start now in
         now, start_table, trans_table, states

   (************************************************************************
    * The info needed to build the grammar.
    *)
   let ivar_of_var = IVar.icreate

   let ivar_list_of_var_list vars =
      List.map ivar_of_var vars

   let iaction_of_action = IAction.icreate

   let prod_item_of_prod prod =
      let { prod_name   = name;
            prod_action = action;
            prod_right  = right;
            prod_prec   = prec
          } = prod
      in
      let core =
         { prod_item_name = ivar_of_var name;
           prod_item_left = [];
           prod_item_right = ivar_list_of_var_list right;
           prod_item_action = iaction_of_action action;
           prod_item_prec = prec
         }
      in
         ProdItem.create core

   let info_of_grammar gram start now =
      (* First and nullable *)
      let prods =
         VarMTable.fold_all (fun prods v items ->
               let v = ivar_of_var v in
               let items = List.map prod_item_of_prod items in
                  IVarTable.add prods v items) IVarTable.empty gram.gram_prod
      in
      let nullable = nullable prods in
      let first = first prods nullable in
      let now = time_print "First and nullable sets" start now in

      (* Initial info *)
      let start_symbols =
         VarSet.fold (fun vars v ->
               IVarSet.add vars (ivar_of_var v)) IVarSet.empty gram.gram_start_symbols
      in
      let prec_table =
         VarTable.fold (fun precs v pre ->
            IVarTable.add precs (ivar_of_var v) pre) IVarTable.empty gram.gram_prec
      in
      let info =
         { info_grammar       = gram;
           info_prod          = prods;
           info_start_symbols = start_symbols;
           info_prec          = prec_table;
           info_nullable      = nullable;
           info_first         = first;
           info_head_table    = IVarTable.empty
         }
      in
      let now, head_table = build_head_table info start now in
      let now = time_print "Head table" start now in
      let info =
         { info with info_nullable   = nullable;
                     info_first      = first;
                     info_head_table = head_table
         }
      in
         now, info

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
         let item, lookahead = ProdItemTable.choose table in
            match ProdItem.get item with
               { prod_item_right = [];
                 prod_item_action = action;
                 prod_item_name = name;
                 prod_item_left = left
               } ->
                  if IVarSet.cardinal lookahead = 1 && IVarSet.choose lookahead = eof then
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
      let { info_grammar = gram;
            info_prec = var_prec_table
          } = info
      in
      let { gram_prec_table = prec_table } = gram in
      let { prod_item_name   = name;
            prod_item_action = action;
            prod_item_left   = left;
            prod_item_prec   = prec_name
          } = ProdItem.get prod_item
      in
      let assoc = Precedence.assoc prec_table prec_name in
      let reduce = ReduceAction (action, name, List.length left) in
         IVarSet.fold (fun actions v ->
               try
                  match IVarTable.find actions v with
                     ShiftAction id
                   | GotoAction id ->
                        (* Shift/reduce conflict *)
                        let cmp =
                           try Precedence.compare prec_table prec_name (IVarTable.find var_prec_table v) with
                              Not_found ->
                              0
                        in
                           if cmp < 0 then
                              actions
                           else if cmp = 0 then
                              match assoc with
                                 LeftAssoc ->
                                    IVarTable.add actions v reduce
                               | RightAssoc ->
                                    actions
                               | NonAssoc ->
                                    IVarTable.add actions v ErrorAction
                               | NoneAssoc ->
                                    shift_reduce_conflict info info_item actions v id prod_item;
                                    actions
                           else
                              IVarTable.add actions v reduce
                   | ReduceAction (action2, _, _) ->
                        reduce_reduce_conflict info info_item actions v action action2;
                        actions
                   | ErrorAction
                   | AcceptAction ->
                        raise (Invalid_argument "reduce_action")
               with
                  Not_found ->
                     IVarTable.add actions v reduce) actions lookahead

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
                     match (ProdItem.get prod_item).prod_item_right with
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
               let core = ProdItem.get item in
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
                        IVarSet.add next v
                   | [] ->
                        IVarSet.union next lookahead
               in
                  info, next) ([], IVarSet.empty) items
      in
         { pda_items     = items;
           pda_next      = next
         }

   (*
    * Find the start state for a production.
    *)
   let create_core gram =
      let start = time_start () in
      let now = start in
      let now, info = info_of_grammar gram start now in
      let now, start_table, trans_table, states = build_lalr_table info start now in
      let trans_table = reduce info trans_table states in
      let now = time_print "Shift/reduce table" start now in

      (* Build the PDA states *)
      let null_info =
         { pda_items     = [];
           pda_next      = IVarSet.empty
         }
      in
      let null_state =
         { pda_delta  = IVarTable.empty;
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
         { pda_start_states    = start_table;
           pda_states          = table;
           pda_ivar_state      = IVar.state;
           pda_iaction_state   = IAction.state;
           pda_prod_item_state = ProdItem.state;
           pda_state_state     = State.state
         }

   let create gram =
      let start = time_start () in
      let pda = create_core gram in
      let _ = time_print "Grammar total" start start in
         pda

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
            eprintf "Calling action %a@." pp_print_iaction action
      in
      let arg, value = eval arg (IAction.get action) loc args in
      let () =
         if !debug_parse then
            eprintf "Called action %a@." pp_print_iaction action
      in
         state, arg, loc, value, stack

   (*
    * Exceptions.
    *)
   let parse_error loc gram run stack state (v : ivar) =
      let { pda_info = { pda_items = items; pda_next = next } } = run.run_states.(state) in
      let buf = stdstr in
         fprintf buf "@[<v 0>Syntax error on token %a" pp_print_ivar v;
         fprintf buf "@ @[<v 3>Current state:";
         List.iter (fun item ->
               let { pda_item_left = left;
                     pda_item_right = right
                   } = item
               in
                  fprintf buf "@ @[<b 3>";
                  Lm_list_util.rev_iter (fun v -> fprintf buf "@ %a" pp_print_ivar v) left;
                  fprintf buf "@ .";
                  List.iter (fun v -> fprintf buf "@ %a" pp_print_ivar v) right;
                  fprintf buf "@]") items;
         fprintf buf "@ @[<b 3>The next possible tokens are:";
         IVarSet.iter (fun v -> fprintf buf "@ %a" pp_print_ivar v) next;
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
            (try IVarTable.find delta v with
                Not_found ->
                   parse_error loc gram run stack state v)
         with
            ShiftAction new_state
          | GotoAction new_state ->
               if !debug_parse then
                  eprintf "State %d: token %a: shift %d@." state pp_print_ivar v new_state;
               pda_no_lookahead gram run arg ((state, loc, x) :: stack) new_state
          | ReduceAction (action, name, tokens) ->
               if !debug_parse then
                  eprintf "State %d: reduce %a@." state pp_print_iaction action;
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
            state pp_print_ivar name;
      let action =
         try IVarTable.find run.run_states.(state).pda_delta name with
            Not_found ->
               parse_error loc gram run stack state name
      in
         match action with
            ShiftAction new_state
          | GotoAction new_state ->
               if !debug_parse then
                  eprintf "State %d: production %a: goto %d (lookahead %a)@." (**)
                     state pp_print_ivar name
                     new_state pp_print_ivar (fst3 tok);
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
               eprintf "State %d: ReduceNow: %a@." state pp_print_iaction action;
            let state, arg, loc, x, stack = semantic_action run.run_eval arg action stack state tokens in
               pda_goto_no_lookahead gram run arg loc stack state name x
       | ReduceAccept (action, _, tokens) ->
            if !debug_parse then
               eprintf "State %d: ReduceAccept: %a@." state pp_print_iaction action;
            let _, arg, _, x, _ = semantic_action run.run_eval arg action stack state tokens in
               arg, x
       | ReduceNone ->
            let v, loc, arg, x = run.run_lexer arg in
            let v = IVar.create v in
            let () =
               if !debug_parse then
                  eprintf "State %d: Read token: %a@." state pp_print_ivar v
            in
               pda_lookahead gram run arg stack state (v, loc, x)

   and pda_goto_no_lookahead gram run arg loc stack state name x =
      let action =
         try IVarTable.find run.run_states.(state).pda_delta name with
            Not_found ->
               parse_error loc gram run stack state name
      in
         match action with
            ShiftAction new_state
          | GotoAction new_state ->
               if !debug_parse then
                  eprintf "State %d: production %a: goto %d (no lookahead)@." (**)
                     state pp_print_ivar name new_state;
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
         try IVarTable.find start_states start with
            Not_found ->
               raise (Failure ("not a start symbol: " ^ string_of_ivar start))
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
      VarSet.fold (fun vars v ->
            Var.get v :: vars) [] (info.parse_grammar.gram_start_symbols)

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
      let gram = add_prec info.parse_grammar pre (Var.create v) in
         { parse_grammar = gram; parse_pda = None }

   let find_prec info v =
      find_prec info.parse_grammar (Var.create v)

   let add_production info action name rhs pre =
      let action = Action.create action in
      let name = Var.create name in
      let rhs = List.map Var.create rhs in
      let pre =
         match pre with
            Some v ->
               Some (Var.create v)
          | None ->
               None
      in
      let gram = add_production info.parse_grammar action name rhs pre in
         { parse_grammar = gram; parse_pda = None }

   let remove_production info action =
      let action = Action.create action in
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
      let start = IVar.create start in
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
