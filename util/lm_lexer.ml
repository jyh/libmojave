(*
 * Construction of lexical analyzers.
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

let debug_lex =
   create_debug (**)
      { debug_name = "lex";
        debug_description = "Debug the lexer";
        debug_value = false
      }

let debug_lexgen =
   create_debug (**)
      { debug_name = "lexgen";
        debug_description = "Debug the lexer generator";
        debug_value = false
      }

(************************************************************************
 * Modules.
 *)

(*
 * We simulate the NFA using the normal subset construction.
 * That is, the state of the DFA is a set of states of the NFA.
 *
 * In addition, we have counters for the r{n,m} regular expressions.
 *
 * So, we model a state of the NFA as a state and the list of
 * counters.  A state of the DFA is a sorted list of NFA states.
 *)
module NfaState =
struct
   type t = int * int list

   (*
    * Reset one of the counters.
    *)
   let rec reset_counter counters i =
      match counters with
         counter :: counters ->
            if i = 0 then
               0 :: counters
            else
               counter :: reset_counter counters (pred i)
       | [] ->
            raise (Invalid_argument "reset_counter: illegal counter")

   (*
    * Increment one of the counters.
    * Return the new counter value too.
    *)
   let rec incr_counter counters i min final max start =
      let rec incr counters i =
         match counters with
            counter :: counters ->
               if i = 0 then
                  let counter = succ counter in
                     counter, counter :: counters
               else
                  let i, counters = incr counters (pred i) in
                     i, counter :: counters
          | [] ->
               raise (Invalid_argument "incr_counter: illegal counter")
      in
      let counter, counters' = incr counters i in
         if counter < min then
            [start, counters']
         else if counter = min then
            [final, counters'; start, counters']
         else if max >= min && counter > max then
            []
         else if max < min then (* This is an optimization: no need to increment the counter *)
            [final, counters; start, counters]
         else
            [final, counters'; start, counters']
end

module DfaState =
struct
   type t = NfaState.t list           (* Sorted *)

   (*
    * Empty set.
    *)
   let empty = ([] : t)

   (*
    * Membership.
    *)
   let mem (set : t) (state : NfaState.t) =
      List.mem state set

   (*
    * Add an element to the set.
    *)
   let rec add (set : t) (state : NfaState.t) =
      match set with
         j :: s ->
            if j > state then
               state :: set
            else if j = state then
               set
            else
               j :: add s state
       | [] ->
            [state]

   (*
    * Union of two sets.
    *)
   let rec union (s1 : t) (s2 : t) =
      match s1, s2 with
         i1 :: l1, i2 :: l2 ->
            if i1 = i2 then
               i1 :: union l1 l2
            else if i1 < i2 then
               i1 :: union l1 s2
            else
               i2 :: union s1 l2
       | _, [] ->
            s1
       | [], _ ->
            s2
end

module DfaStateCompare =
struct
   type t = DfaState.t
   let compare = Pervasives.compare
end

module DfaStateTable = Lm_map.LmMake (DfaStateCompare);;

module IntCompare =
struct
   type t = int
   let compare = (-)
end

module IntSet   = Lm_set.LmMake (IntCompare);;
module IntTable = Lm_map.LmMake (IntCompare);;

(*
 * A TransTable represents the transition function for a DFA.
 * We represent this as a sorted array of entries.
 *)

(*
 * Binary search returns the smallest element
 * that is no smaller than the key.
 *)
let rec binary_search table (key : int) i j =
   if i < j - 1 then
      let k = (i + j) lsr 1 in
      let key', _ = table.(k) in
         if key' < key then
            binary_search table key k j
         else
            binary_search table key i k
   else
      j

module TransTable =
struct
   type 'a t = (int * 'a) array

   let empty = [||]

   (*
    * Find an entry in the table,
    * returning the default value if not found.
    *)
   let find table key default =
      let len = Array.length table in
      let i = binary_search table key (-1) len in
         if i = len then
            default
         else
            let key', value = table.(i) in
               if key' = key then
                  value
               else
                  default

   (*
    * Add an entry to the table.
    * Assumes the entry does not already exist.
    *)
   let add table key value =
      let len = Array.length table in
         if len = 0 then
            [|key, value|]
         else
            let i = binary_search table key (-1) len in
            let new_array = Array.create (len + 1) (key, value) in
               Array.blit table 0 new_array 0 i;
               Array.blit table i new_array (i + 1) (len - i);
               new_array
end

(************************************************************************
 * Lexer construction.
 *)

(*
 * Argument types.
 *)
module type LexerInput =
sig
   (*
    * Input channel is a stream of integers.
    * Usually these are just the ASCII codes for characters.
    *)
   type t

   (*
    * The channel has two special characters.
    *    bof: the beginning of file
    *    eof: the end of file.
    *)
   val bof : int
   val eof : int

   (*
    * The next function returns the next character in the input stream.
    *)
   val lex_next : t -> int

   (*
    * The pos function returns the current position of
    * the input buffer within the lexeme
    * (used for collecting \( ... \) arguments).
    *)
   val lex_pos : t -> int

   (*
    * The lexer will call start when it begins lexing.
    * The integer should be the *previous* character in the
    * input channel, or bof if at the beginning.
    *)
   val lex_start : t -> int

   (*
    * In some cases, the lexer may want to restart scanning
    * from a previous point.  If so, it will call this function
    * to reset the start point.
    *)
   val lex_restart : t -> int -> unit

   (*
    * When the lexer is done, it calls lex_stop with
    * the number of characters in the final lexeme.  Note
    * that this can cause data to be pushed back onto the input stream.
    *)
   val lex_stop : t -> int -> unit

   (*
    * Before calling lex_stop, the lexer may ask for the
    * lexeme as a string.  The integer is the number of
    * characters in the lexeme, the same as the argument
    * to lex_stop.
    *)
   val lex_string    : t -> int -> string
   val lex_substring : t -> int -> int -> string
   val lex_loc       : t -> int -> loc
end

module type LexerAction =
sig
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

   (* For creating sets and tables *)
   val hash : action -> int
   val compare : action -> action -> int

   (*
    * You can use the function to decide which clauses take
    * precedence for a match of equal length.  The function
    * gets two clause numbers.  If you use the min function,
    * then you get the first clause that matched.  If you
    * use the max function, you get the second clause that
    * matched.
    *)
   val choose : int -> int -> int
end

module MakeLexer (Input : LexerInput) (Action : LexerAction) =
struct
   open Action

   (*
    * For now, just create a default action set.
    *)
   module ActionCompare =
   struct
      type t = action
      let compare = Action.compare
   end

   module ActionSet = Lm_set.LmMake (ActionCompare);;

   (************************************************************************
    * Types.
    *)

   (*
    * A simplified regular expression.
    *)

   (* %%MAGICBEGIN%% *)
   type regex =
      RegexAnySymbol
    | RegexSymbol       of int list
    | RegexExceptSymbol of int list
    | RegexLimitPrev    of int list
    | RegexLimitNext    of int list
    | RegexChoice       of regex list
    | RegexSequence     of regex list
    | RegexStar         of regex
    | RegexPlus         of regex
    | RegexInterval     of regex * int * int       (* regex, min, max *)
    | RegexArg          of regex

   (*
    * Termination symbols.
    *)
   type regex_term =
      RegexTermEof
    | RegexTermRightParen of int
    | RegexTermRightArg   of int
    | RegexTermPipe       of int

   (*
    * An expression is nearly an NFA,
    * but designed to be built incrementally.
    * The id is an arbitrary int, but all clauses
    * must have unique ids.
    *)
   type exp =
      { exp_clauses : (action * int * regex) list;
        exp_id      : int
      }

   (*
    * An action specifies:
    *    ActionEpsilon state : epsilon transition to the given states
    *    ActionArgStart i    : start collecting the arguments for the rules
    *    ActionArgStop i     : stop collecting the arguments for the rules
    *    ActionStop i        : rule i is finished
    *    ActionSymbol table  : transition function
    *    ActionLimit syms    : normally an epsilon transition, but limited to syms
    *                          (this is to handle \< and \> symbols)
    *)
   type nfa_action =
      NfaActionEpsilon      of int list           (* next state list *)
    | NfaActionArgStart     of int * int          (* arg id, next state *)
    | NfaActionArgStop      of int * int          (* arg id, next state *)
    | NfaActionStop         of int                (* clause id, next state *)
    | NfaActionSymbol       of int list * int     (* symbols, next state *)
    | NfaActionAnySymbol    of int                (* next state *)
    | NfaActionExceptSymbol of int list *int      (* symbols, next state *)
    | NfaActionLimitPrev    of int list * int     (* symbols, next state *)
    | NfaActionLimitNext    of int list * int     (* symbols, next state *)
    | NfaActionNone
    | NfaActionResetCounter of int * int list              (* counter, next state list *)
    | NfaActionIncrCounter  of int * int * int * int * int (* counter, min, final, max, restart *)

   (*
    * This is the argument info we pass to the DFA.
    *)
   type arg =
      { arg_index     : int;
        arg_depends   : IntSet.t;
        arg_clause    : int;
        arg_number    : int
      }

   (*
    * This is the info we accumulate during compilation.
    *    nfa_index       : the index of the next state to be allocated
    *    nfa_counter     : the total number of interval expression we have seen
    *    nfa_arg_index   : the identifier of the next argument
    *    nfa_arg_depends : what other arguments this one depends on
    *)
   type nfa_accum =
      { nfa_index       : int;
        nfa_counter     : int;
        nfa_arg_index   : int;
        nfa_arg_table   : arg IntTable.t
      }

   (*
    * This is the info we pass left-to-right during compilation.
    *    nfa_clause       : the index of the current clause being compiled
    *    nfa_arg_number   : the index of the next argument
    *    nfa_arg_depends  : what other arguments the next one depends on
    *)
   type nfa_info =
      { nfa_clause        : int;
        nfa_arg_number    : int;
        nfa_arg_depends   : IntSet.t;
      }

   (*
    * A state in the machine.
    *)
   type nfa_state =
      { nfa_state_index   : int;
        nfa_state_action  : nfa_action
      }

   (*
    * The NFA has a start state,
    * and an array of states.
    *)
   type nfa =
      { nfa_actions     : action IntTable.t;
        nfa_start       : NfaState.t;
        nfa_table       : nfa_state array;
        nfa_args        : IntSet.t IntTable.t;
        nfa_arg_info    : arg IntTable.t
      }

   (*
    * DFA actions.
    *)
   type dfa_actions =
      { dfa_action_stop          : int option;                (* clause id *)
        dfa_action_arg_start_prev: IntSet.t;                  (* arg ids *)
        dfa_action_arg_stop_prev : IntSet.t;                  (* arg ids *)
        dfa_action_arg_stop_cur  : IntSet.t                   (* arg ids *)
      }

   (*
    * A transition may specify a new state and some actions.
    * or it may not exist,
    * or it may be unknown.
    *)
   type dfa_transition =
      DfaTransition of int * dfa_actions
    | DfaNoTransition
    | DfaUnknownTransition

   (*
    * A DFA state has an index,
    * the subset of states for the NFA,
    * and a lazy transition function.
    *)
   type dfa_state =
      { dfa_state_index         : int;
        dfa_state_set           : DfaState.t;
        mutable dfa_state_delta : dfa_transition TransTable.t
      }

   (*
    * The DFA has:
    *)
   type dfa =
     { mutable dfa_states : dfa_state array;      (* May be partially filled *)
       mutable dfa_length : int;                  (* Index of the largest valid state *)
       mutable dfa_map    : int DfaStateTable.t;  (* Map from NFA state subsets to DFA states *)
       dfa_table          : nfa_state array;      (* The NFA *)
       dfa_arg_depends    : IntSet.t IntTable.t;  (* The map from clause id to valid arguments *)
       dfa_arg_table      : arg IntTable.t;       (* The info about each arg *)
       dfa_action_table   : action IntTable.t     (* The map from clause id to actions *)
     }

   (*
    * The actual type.
    *)
   type t =
      { lex_exp         : exp;
        mutable lex_dfa : dfa option
      }
   (* %%MAGICEND%% *)

   (*
    * When we are collecting an arg, it may be in several states.
    *)
   type dfa_arg =
      ArgStarted of int
    | ArgComplete of int * int
    | ArgFinished of int * int

   (*
    * When we are scanning, we also have state.
    *)
   type dfa_info =
     { mutable dfa_stop_clause    : int;                  (* Clause id of the last match, or 0 if none *)
       mutable dfa_stop_pos       : int;                  (* Position of the last match *)
       mutable dfa_start_pos      : int;                  (* Starting position *)

       (*
        * Scanned arguments.
        * The index is the argument id number.
        * The value is (start, stop).
        *)
       mutable dfa_args           : dfa_arg IntTable.t;

       (*
        * The channel we are scanning from.
        *)
       dfa_channel                : Input.t
     }

   (************************************************************************
    * Characters and classes.
    *)

   let bof = Input.bof
   let eof = Input.eof

   let zero_char = Char.code '0'
   let at_char   = Char.code '@'
   let alert_char     = Char.code 'G' - at_char
   let backspace_char = Char.code 'H' - at_char
   let formfeed_char  = Char.code 'L' - at_char
   let newline_char   = Char.code '\n'
   let cr_char        = Char.code '\r'
   let tab_char       = Char.code '\t'
   let vertical_tab_char = Char.code 'K' - at_char
   let hex_a_char        = Char.code 'a' - 10
   let hex_A_char        = Char.code 'A' - 10

   (*
    * Character sets.
    *)
   external omake_alnum : unit -> string = "omake_alnum"
   external omake_alpha : unit -> string = "omake_alpha"
   external omake_graph : unit -> string = "omake_graph"
   external omake_lower : unit -> string = "omake_lower"
   external omake_upper : unit -> string = "omake_upper"
   external omake_punct : unit -> string = "omake_punct"
   external omake_space : unit -> string = "omake_space"

   let explode_chars s =
      let len = String.length s in
      let rec collect chars i =
         if i = len then
            chars
         else
            collect (Char.code s.[i] :: chars) (succ i)
      in
         collect [] 0

   let alnum_chars = explode_chars (omake_alnum ())
   let alpha_chars = explode_chars (omake_alpha ())
   let graph_chars = explode_chars (omake_graph ())
   let lower_chars = explode_chars (omake_lower ())
   let upper_chars = explode_chars (omake_upper ())
   let punct_chars = explode_chars (omake_punct ())
   let space_chars = explode_chars (omake_space ())

   let ascii_chars =
      let rec collect chars i =
         if i != 128 then
            collect (i :: chars) (succ i)
         else
            chars
      in
         collect [] 0

   let blank_chars = [Char.code ' '; Char.code '\t']

   let cntrl_chars =
      let rec collect chars i =
         if i != 32 then
            collect (i :: chars) (succ i)
         else
            chars
      in
         collect [] 0

   let digit_chars = List.map Char.code ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']

   let print_chars = Char.code ' ' :: graph_chars

   let xdigit_chars = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F']
   let xdigit_chars = digit_chars @ List.map Char.code xdigit_chars

   let white_or_bof_chars = bof :: space_chars
   let white_or_eof_chars = eof :: space_chars
   let word_chars = Char.code '_' :: alnum_chars

   (************************************************************************
    * Regular expressions.
    *)

   (*
    * Printer.
    *)
   let pp_print_char buf c =
      if c = bof then
         pp_print_string buf "\\bof"
      else if c = eof then
         pp_print_string buf "\\eof"
      else if c < 32 || c >= 127 then
         fprintf buf "\\%03d" c
      else
         pp_print_char buf (Char.chr c)

   let pp_print_chars buf cl =
      List.iter (pp_print_char buf) cl

   let rec pp_print_regex buf regex =
      match regex with
         RegexAnySymbol ->
            pp_print_string buf "."
       | RegexSymbol [] ->
            fprintf buf "(symbol)"
       | RegexExceptSymbol [] ->
            fprintf buf "(^symbol)"
       | RegexSymbol cl ->
            fprintf buf "(symbol ";
            List.iter (fun c -> pp_print_char buf c) cl;
            fprintf buf ")"
       | RegexExceptSymbol cl ->
            fprintf buf "(^symbol ";
            List.iter (fun c -> pp_print_char buf c) cl;
            fprintf buf ")"
       | RegexLimitPrev [] ->
            fprintf buf "(prev-symbol)"
       | RegexLimitNext [] ->
            fprintf buf "(next-symbol)"
       | RegexLimitPrev cl ->
            fprintf buf "(prev-symbol ";
            List.iter (fun c -> pp_print_char buf c) cl;
            fprintf buf ")"
       | RegexLimitNext cl ->
            fprintf buf "(next-symbol ";
            List.iter (fun c -> pp_print_char buf c) cl;
            fprintf buf ")"
       | RegexChoice el ->
            fprintf buf "@[<hv 3>(choice";
            List.iter (fun e -> fprintf buf "@ %a" pp_print_regex e) el;
            fprintf buf ")@]"
       | RegexSequence el ->
            fprintf buf "@[<hv 3>(sequence";
            List.iter (fun e -> fprintf buf "@ %a" pp_print_regex e) el;
            fprintf buf ")@]"
       | RegexStar e ->
            fprintf buf "(star %a)" pp_print_regex e
       | RegexPlus e ->
            fprintf buf "(plus %a)" pp_print_regex e
       | RegexInterval (e, min, max) ->
            fprintf buf "@[<hv 3>(interval{%d,%d}@ %a)@]" min max pp_print_regex e
       | RegexArg e ->
            fprintf buf "@[<hv 1>\\(%a\\)@]" pp_print_regex e

   (*
    * Standard regular expressions.
    *)
   let left_word_delimiter   = RegexSequence  [RegexLimitPrev white_or_bof_chars; RegexLimitNext word_chars]
   let right_word_delimiter  = RegexSequence  [RegexLimitPrev word_chars; RegexLimitNext white_or_eof_chars]
   let word_delimiter        = RegexChoice    [left_word_delimiter; right_word_delimiter]
   let inside_word_delimiter = RegexSequence  [RegexLimitPrev word_chars; RegexLimitNext word_chars]

   let left_line_delimiter   = RegexLimitPrev [bof; Char.code '\r'; Char.code '\n']
   let right_line_delimiter  = RegexChoice    [RegexSymbol [Char.code '\r'; Char.code '\n'; eof];
                                               RegexSequence [RegexSymbol [Char.code '\r'];
                                                              RegexSymbol [Char.code '\n']]]
   let bof_delimiter         = RegexLimitPrev [bof]
   let eof_delimiter         = RegexSymbol [eof]

   (*
    * Reduce a choice list.
    *)
   let regex_reduce_choices stack =
      match stack with
         [] ->
            RegexSequence []
       | [regex] ->
            regex
       | _ ->
            RegexChoice (List.rev stack)
   (*
    * Reduce the stack, its just a sequence.
    *)
   let regex_reduce_sequence stack =
      match stack with
         [regex] ->
            regex
       | _ ->
            RegexSequence (List.rev stack)

   (*
    * Just saw a +
    *)
   let regex_reduce_plus stack =
      match stack with
         elem :: stack ->
            RegexPlus elem :: stack
       | [] ->
            [RegexSymbol [Char.code '+']]

   let regex_reduce_star stack =
      match stack with
         elem :: stack ->
            RegexStar elem :: stack
       | [] ->
            [RegexSymbol [Char.code '*']]

   let regex_reduce_opt stack =
      match stack with
         elem :: stack ->
            RegexChoice [elem; RegexSequence []] :: stack
       | [] ->
            [RegexSymbol [Char.code '?']]

   let regex_reduce_interval stack n m =
      match stack with
         elem :: stack ->
            RegexInterval (elem, n, m) :: stack
       | [] ->
            []

   (*
    * Interval expressions.
    *)
   let rec regex_interval n s i len =
      if i = len then
         raise (Failure "interval expression is not terminated");
      let j = succ i in
      let c = s.[i] in
         match c with
            '0'..'9' ->
               regex_interval (n * 10 + (Char.code c - Char.code '0')) s j len
          | ',' ->
               regex_interval_bound n 0 s j len
          | '}' ->
               n, n, j
          | _ ->
               raise (Failure "interval expression is not terminated")

   and regex_interval_bound n m s i len =
      if i = len then
         raise (Failure "interval expression is not terminated");
      let j = succ i in
      let c = s.[i] in
         match c with
            '0'..'9' ->
               regex_interval_bound n (m * 10 + (Char.code c - Char.code '0')) s j len
          | '}' ->
               n, m, j
          | _ ->
               raise (Failure "interval expression is not terminated")

   (*
    * Character constants.
    *)
   let rec regex_hex_const c s i len =
      if i = len then
         c, i
      else
         let c' = s.[i] in
         let j = succ i in
            match c' with
               '0'..'9' ->
                  regex_hex_const (c * 16 + Char.code c' - zero_char) s j len
             | 'a'..'f' ->
                  regex_hex_const (c * 16 + Char.code c' - hex_a_char) s j len
             | 'A'..'F' ->
                  regex_hex_const (c * 16 + Char.code c' - hex_A_char) s j len
             | _ ->
                  c, i

   let rec regex_octal_const c s i len =
      if i = len then
         c, i
      else
         let c' = s.[i] in
            match c' with
               '0'..'7' ->
                  regex_octal_const (c * 8 + Char.code c' - zero_char) s (succ i) len
             | _ ->
                  c, i

   (*
    * Literal characters [...]
    *)
   let rec regex_chars s i len =
      if i = len then
         raise (Failure "character sequence is not terminated");
      let j = succ i in
         match s.[i] with
            '^' ->
               let chars, j = regex_chars_head s j len in
                  RegexExceptSymbol chars, j
          | _ ->
               let chars, j = regex_chars_head s i len in
                  RegexSymbol chars, j

   (*
    * At the head, allow a literal ]
    *)
   and regex_chars_head s i len =
      if i = len then
         raise (Failure "character sequence is not terminated");
      match s.[i] with
         ']' ->
            regex_chars_rest [Char.code ']'] s (succ i) len
       | _ ->
            regex_chars_rest [] s i len

   (*
    * Normal scanning.
    * Have to look for [:...:] sequences and ]
    *)
   and regex_chars_rest chars s i len =
      if i = len then
         raise (Failure "character sequence is not terminated");
      let j = succ i in
         match s.[i] with
            '[' ->
               regex_chars_possible_class chars s j len
          | ']' ->
               chars, j
          | '\\' ->
               regex_chars_escape chars s j len
          | c ->
               regex_chars_possible_range chars (Char.code c) s j len

   (*
    * Just saw a backslash.
    *)
   and regex_chars_escape chars s i len =
      if i = len then
         raise (Failure "character sequence is not terminated");
      let j = succ i in
      let c, j =
         match s.[i] with
            'a' ->
               alert_char, j
          | 'b' ->
               backspace_char, j
          | 'f' ->
               formfeed_char, j
          | 'n' ->
               newline_char, j
          | 'r' ->
               cr_char, j
          | 't' ->
               tab_char, j
          | 'v' ->
               vertical_tab_char, j
          | 'x' ->
               regex_hex_const 0 s j len
          | '0'..'9' ->
               regex_octal_const 0 s i (min (i + 3) len)
          | c ->
               Char.code c, j
      in
         regex_chars_rest (c :: chars) s j len

   (*
    * Just seen a character, look for a character range c-c
    *)
   and regex_chars_possible_range chars c1 s i len =
      if i = len then
         raise (Failure "character sequence not terminated");
      let j = succ i in
         match s.[i] with
            '-' ->
               regex_chars_range chars c1 s j len
          | '[' ->
               regex_chars_possible_class (c1 :: chars) s j len
          | ']' ->
               c1 :: chars, j
          | '\\' ->
               regex_chars_escape (c1 :: chars) s j len
          | c ->
               regex_chars_possible_range (c1 :: chars) (Char.code c) s j len

   (*
    * Just seen a c-, get the remain char.
    *)
   and regex_chars_range chars c1 s i len =
      if i = len then
         raise (Failure "character sequence not terminated");
      let j = succ i in
      let c2 = Char.code s.[i] in
      let rec collect chars i =
         if i > c2 then
            chars
         else
            collect (i :: chars) (succ i)
      in
      let chars = collect chars c1 in
         regex_chars_rest chars s j len

   (*
    * Just saw a [, look for the :
    *)
   and regex_chars_possible_class chars s i len =
      if i = len then
         raise (Failure "character sequence is not terminated");
      let j = succ i in
         match s.[i] with
            ':' ->
               regex_chars_class chars s j len
          | '[' ->
               regex_chars_possible_class (Char.code '[' :: chars) s j len
          | '\\' ->
               regex_chars_escape (Char.code '[' :: chars) s j len
          | c ->
               regex_chars_rest (Char.code c :: Char.code '[' :: chars) s j len

   (*
    * Get the character class specified by a sequence [:name:]
    *)
   and regex_chars_class chars s i len =
      let start = i in
      let rec get_name i =
         if i + 1 >= len then
            raise (Failure "character class is not terminated");
         let c = s.[i] in
         let j = succ i in
            match c with
               'a'..'z'
             | 'A'..'Z' ->
                  get_name j
             | ':' ->
                  if s.[j] = ']' then
                     regex_chars_get_class chars (String.sub s start (i - start)) s (succ j) len
                  else
                     raise (Failure "character class: syntax error")
             | _ ->
                  raise (Failure "character class: syntax error")
      in
         get_name i

   (*
    * These are the standard classes.
    *)
   and regex_chars_get_class chars name s i len =
      let charclass =
         match String.lowercase name with
            "alnum" ->
               alnum_chars
          | "alpha" ->
               alpha_chars
          | "blank" ->
               blank_chars
          | "cntrl" ->
               cntrl_chars
          | "digit" ->
               digit_chars
          | "graph" ->
               graph_chars
          | "lower" ->
               lower_chars
          | "print" ->
               print_chars
          | "punct" ->
               punct_chars
          | "space" ->
               space_chars
          | "upper" ->
               upper_chars
          | "xdigit" ->
               xdigit_chars
          | name ->
               raise (Failure ("unknown character class: " ^ name))
      in
         regex_chars_rest (charclass @ chars) s i len

   (*
    * Parse an expression block.
    *)
   let rec regex_choices choices s i len =
      let regex, term = regex_of_string [] s i len in
      let choices = regex :: choices in
         match term with
            RegexTermEof ->
               regex_reduce_choices choices, term
          | RegexTermRightParen _
          | RegexTermRightArg _ ->
               regex_reduce_choices choices, term
          | RegexTermPipe i ->
               regex_choices choices s i len

   and regex_left_paren s i len =
      let regex, term = regex_choices [] s i len in
         match term with
            RegexTermRightParen i ->
               regex, i
          | RegexTermRightArg _
          | RegexTermEof ->
               raise (Failure "mismatched parenthesis")
          | RegexTermPipe _ ->
               raise (Invalid_argument "regex_left_paren")

   and regex_left_arg s i len =
      let regex, term = regex_choices [] s i len in
         match term with
            RegexTermRightArg i ->
               regex, i
          | RegexTermRightParen _
          | RegexTermEof ->
               raise (Failure "mismatched parenthesis")
          | RegexTermPipe _ ->
               raise (Invalid_argument "regex_left_arg")

   (*
    * Parse the regular expression string.
    *)
   and regex_of_string stack s i len =
      if i = len then
         regex_reduce_sequence stack, RegexTermEof
      else
         let j = succ i in
            match s.[i] with
               '\\' ->
                  regex_of_escape stack s j len
             | '.' ->
                  let stack = RegexAnySymbol :: stack in
                     regex_of_string stack s j len
             | '^' ->
                  let stack = left_line_delimiter :: stack in
                     regex_of_string stack s j len
             | '$' ->
                  let stack = right_line_delimiter :: stack in
                     regex_of_string stack s j len
             | '[' ->
                  let regex, j = regex_chars s j len in
                  let stack = regex :: stack in
                     regex_of_string stack s j len
             | '+' ->
                  let stack = regex_reduce_plus stack in
                     regex_of_string stack s j len
             | '*' ->
                  let stack = regex_reduce_star stack in
                     regex_of_string stack s j len
             | '?' ->
                  let stack = regex_reduce_opt stack in
                     regex_of_string stack s j len
             | '(' ->
                  let regex, j = regex_left_paren s j len in
                     regex_of_string (regex :: stack) s j len
             | ')' ->
                  regex_reduce_sequence stack, RegexTermRightParen j
             | '|' ->
                  regex_reduce_sequence stack, RegexTermPipe j
             | '{' ->
                  let min, max, j = regex_interval 0 s j len in
                  let stack = regex_reduce_interval stack min max in
                     regex_of_string stack s j len
             | c ->
                  let stack = RegexSymbol [Char.code c] :: stack in
                     regex_of_string stack s j len

   (*
    * Escaped char.
    *)
   and regex_of_escape stack s i len =
      if i = len then
         raise (Failure "illegal backslash at end of string");

      let j = succ i in
         match s.[i] with
            '(' ->
               let regex, j = regex_left_arg s j len in
               let stack = RegexArg regex :: stack in
                  regex_of_string stack s j len
          | ')' ->
               regex_reduce_sequence stack, RegexTermRightArg j
          | '<' ->
               let stack = left_word_delimiter :: stack in
                  regex_of_string stack s j len
          | '>' ->
               let stack = right_word_delimiter :: stack in
                  regex_of_string stack s j len
          | 'y' ->
               let stack = word_delimiter :: stack in
                  regex_of_string stack s j len
          | 'B' ->
               let stack = inside_word_delimiter :: stack in
                  regex_of_string stack s j len
          | 'w' ->
               let stack = RegexSymbol word_chars :: stack in
                  regex_of_string stack s j len
          | 'W' ->
               let stack = RegexExceptSymbol word_chars :: stack in
                  regex_of_string stack s j len
          | '`' ->
               let stack = bof_delimiter :: stack in
                  regex_of_string stack s j len
          | '\'' ->
               let stack = eof_delimiter :: stack in
                  regex_of_string stack s j len
          | 'a' ->
               let stack = RegexSymbol [alert_char] :: stack in
                  regex_of_string stack s j len
          | 'b' ->
               let stack = RegexSymbol [backspace_char] :: stack in
                  regex_of_string stack s j len
          | 'f' ->
               let stack = RegexSymbol [formfeed_char] :: stack in
                  regex_of_string stack s j len
          | 'n' ->
               let stack = RegexSymbol [newline_char] :: stack in
                  regex_of_string stack s j len
          | 'r' ->
               let stack = RegexSymbol [cr_char] :: stack in
                  regex_of_string stack s j len
          | 't' ->
               let stack = RegexSymbol [tab_char] :: stack in
                  regex_of_string stack s j len
          | 'v' ->
               let stack = RegexSymbol [vertical_tab_char] :: stack in
                  regex_of_string stack s j len
          | 'x' ->
               let c, j = regex_hex_const 0 s j len in
               let stack = RegexSymbol [c] :: stack in
                  regex_of_string stack s j len
          | '0'..'9' ->
               let c, j = regex_octal_const 0 s i (min (i + 3) len) in
               let stack = RegexSymbol [c] :: stack in
                  regex_of_string stack s j len
          | c ->
               let stack = RegexSymbol [Char.code c] :: stack in
                  regex_of_string stack s j len

   (*
    * The toplevel function.
    *)
   let regex_of_string s =
      let regex, term = regex_choices [] s 0 (String.length s) in
      let regex =
         match term with
            RegexTermEof ->
               regex
          | RegexTermRightParen _
          | RegexTermRightArg _ ->
               raise (Failure "mismatched parenthesis")
          | RegexTermPipe _ ->
               raise (Invalid_argument "regex_of_string")
      in
         if !debug_lexgen then
            eprintf "@[<v 3>Regex:@ @[<hv 3>string: \"%s\"@]@ @[<hv 3>regex:@ %a@]@]@." (**)
               s pp_print_regex regex;
         regex

   (************************************************************************
    * Expressions.
    *)

   let pp_print_exp buf exp =
      let { exp_clauses = clauses;
            exp_id = id
          } = exp
      in
         fprintf buf "Id: %d" id;
         List.iter (fun (action, id, regex) ->
               fprintf buf "@ @[<hv 3>Clause:@ id = %d@ action = %a@ @[<hv 3>regex =@ %a@]@]" (**)
                  id pp_print_action action pp_print_regex regex) clauses

   (*
    * An expression is a set of clauses.
    *)
   let empty_exp =
      { exp_clauses = [];
        exp_id      = 0
      }

   (*
    * Compute the number of arguments in the regex.
    *)
   let rec regex_arg_count count e =
      match e with
         RegexAnySymbol
       | RegexSymbol _
       | RegexExceptSymbol _
       | RegexLimitPrev _
       | RegexLimitNext _
       | RegexChoice [] ->
            count
       | RegexChoice (e :: _)
       | RegexStar e
       | RegexPlus e
       | RegexInterval (e, _, _) ->
            regex_arg_count count e
       | RegexSequence el ->
            List.fold_left regex_arg_count count el
       | RegexArg e ->
            regex_arg_count (succ count) e

   (*
    * Add a clause to the pre-NFA.
    *)
   let add_clause_exp exp action s =
      let regex = regex_of_string s in
      let arity = regex_arg_count 0 regex in
      let { exp_clauses = clauses;
            exp_id      = id
          } = exp
      in
      let exp =
         { exp_clauses = (action, id, regex) :: clauses;
           exp_id      = succ id
         }
      in
         arity, exp

   (*
    * Remove a clause.
    *)
   let remove_clause_exp exp action =
      let clauses =
         List.filter (fun (action', _, _) ->
               action' <> action) exp.exp_clauses
      in
         { exp with exp_clauses = clauses }

   (*
    * Take the union of two expression lists.
    *)
   let union_exp exp1 exp2 =
      let { exp_clauses = clauses1;
            exp_id = id1
          } = exp1
      in
      let { exp_clauses = clauses2 } = exp2 in
      let actions =
         List.fold_left (fun actions (action, _, _) ->
               ActionSet.add actions action) ActionSet.empty clauses1
      in
      let rec collect id clauses1 clauses2 =
         match clauses2 with
            (action, _, regex) :: clauses2 ->
               if ActionSet.mem actions action then
                  collect id clauses1 clauses2
               else
                  collect (succ id) ((action, id, regex) :: clauses1) clauses2
          | [] ->
               id, clauses1
      in
      let id, clauses1 = collect id1 clauses1 clauses2 in
         if id = id1 then
            false, exp1
         else
            true, { exp_clauses = clauses1; exp_id = id }

   (************************************************************************
    * NFA.
    *)

   let pp_print_nfa_id buf nid =
      match nid with
         (nid, []) ->
            pp_print_int buf nid
       | (nid, counters) ->
            fprintf buf "<%d" nid;
            List.iter (fun counter -> fprintf buf " %d" counter) counters;
            fprintf buf ">"

   let pp_print_choices buf choices =
      IntSet.iter (fun i -> fprintf buf " %d" i) choices

   let pp_print_nfa_action buf action =
      match action with
         NfaActionEpsilon next ->
            fprintf buf "@[<hv 3>(epsilon goto";
            List.iter (fun i -> fprintf buf "@ %d" i) next;
            fprintf buf ")@]"
       | NfaActionArgStart (id, next) ->
            fprintf buf "(arg-start %d goto %d)" id next
       | NfaActionArgStop (id, next) ->
            fprintf buf "(arg-stop %d goto %d)" id next
       | NfaActionStop clause ->
            fprintf buf "(stop [%d])" clause
       | NfaActionSymbol (syms, next) ->
            fprintf buf "@[<hv 3>(symbols [%a]@ goto %d)@]" pp_print_chars syms next
       | NfaActionExceptSymbol (syms, next) ->
            fprintf buf "@[<hv 3>(^symbols [%a]@ goto %d)@]" pp_print_chars syms next
       | NfaActionAnySymbol next ->
            fprintf buf "(. goto %d)" next
       | NfaActionLimitPrev (syms, next) ->
            fprintf buf "@[<hv 3>(limit-prev [%a]@ goto %d)@]" pp_print_chars syms next
       | NfaActionLimitNext (syms, next) ->
            fprintf buf "@[<hv 3>(limit-next [%a]@ goto %d)@]" pp_print_chars syms next
       | NfaActionNone ->
            fprintf buf "(final)"
       | NfaActionResetCounter (counter, next) ->
            fprintf buf "@[<hv 3>(reset@ counter = %d@ goto" counter;
            List.iter (fun i -> fprintf buf "@ %d" i) next;
            fprintf buf ")@]"
       | NfaActionIncrCounter (counter, min, final, max, start) ->
            fprintf buf "@[<hv 3>(increment@ counter = %d@ if count >= %d then goto %d@ if count <= %d then goto %d)@]" (**)
               counter min final max start

   let pp_print_nfa_state buf nfa_state =
      let { nfa_state_index = index;
            nfa_state_action = action
          } = nfa_state
      in
         fprintf buf "@[<hv 3>NFA state %d:@ action %a@]" index pp_print_nfa_action action

   let pp_print_nfa buf nfa =
      let { nfa_start = start;
            nfa_table = table
          } = nfa
      in
         fprintf buf "@[<hv 3>NFA: start = %a" pp_print_nfa_id start;
         Array.iter (fun state ->
               fprintf buf "@ %a" pp_print_nfa_state state) table;
         fprintf buf "@]"

   (************************************************
    * Construct a new state.
    *)
   let nfa_state accum action =
      let { nfa_index = index } = accum in
      let state =
         { nfa_state_index = index;
           nfa_state_action = action
         }
      in
         { accum with nfa_index = succ index }, state

   (*
    * Set the action.
    *)
   let set_action state action =
     { state with nfa_state_action = action }

   (*
    * Compile the NFA from a regex.
    * We are given a start and a final state,
    * and the task to to connect them according to the regex.
    *
    * Invariant: the provided start state is current NfaActionNone.
    * There is no such guarantee for the provided final state, and
    * it should not be modified.
    *
    * The states list contains all the states that are not the
    * start and final states.
    *)
   let rec compile (accum : nfa_accum) (info : nfa_info) start final states regex =
      match regex with
         (* Sequence *)
         RegexSequence regexl ->
            compile_sequence accum info start final states regexl

         (* Choice *)
       | RegexChoice [] ->
            let start = set_action start (NfaActionEpsilon [final.nfa_state_index]) in
               accum, info, start, states
       | RegexChoice [regex] ->
            compile accum info start final states regex
       | RegexChoice (regex :: regexl) ->
            let accum, info, starts, finals, states =
               compile_choice accum info states regex regexl
            in
            let start = set_action start (NfaActionEpsilon (List.map (fun state -> state.nfa_state_index) starts)) in
            let action = NfaActionEpsilon [final.nfa_state_index] in
            let finals = List.map (fun state -> set_action state action) finals in
               accum, info, start, starts @ finals @ states

         (* Symbols *)
       | RegexSymbol syms ->
            let start = set_action start (NfaActionSymbol (syms, final.nfa_state_index)) in
               accum, info, start, states
       | RegexExceptSymbol syms ->
            let start = set_action start (NfaActionExceptSymbol (syms, final.nfa_state_index)) in
               accum, info, start, states
       | RegexAnySymbol ->
            let start = set_action start (NfaActionAnySymbol final.nfa_state_index) in
               accum, info, start, states
       | RegexLimitPrev syms ->
            let start = set_action start (NfaActionLimitPrev (syms, final.nfa_state_index)) in
               accum, info, start, states
       | RegexLimitNext syms ->
            let start = set_action start (NfaActionLimitNext (syms, final.nfa_state_index)) in
               accum, info, start, states

         (* Kleene closure *)
       | RegexStar regex ->
            let accum, start1 = nfa_state accum NfaActionNone in
            let accum, final1 = nfa_state accum (NfaActionEpsilon [start1.nfa_state_index; final.nfa_state_index]) in
            let start = set_action start (NfaActionEpsilon [start1.nfa_state_index; final.nfa_state_index]) in
            let accum, info, start1, states =
               compile accum info start1 final1 states regex
            in
               accum, info, start, start1 :: final1 :: states
       | RegexPlus regex ->
            let accum, start1 = nfa_state accum NfaActionNone in
            let accum, final1 = nfa_state accum (NfaActionEpsilon [start1.nfa_state_index; final.nfa_state_index]) in
            let start = set_action start (NfaActionEpsilon [start1.nfa_state_index]) in
            let accum, info, start1, states =
               compile accum info start1 final1 states regex
            in
               accum, info, start, start1 :: final1 :: states
       | RegexInterval (regex, min, max) ->
            let { nfa_counter = counter } = accum in
            let accum, start1 = nfa_state accum NfaActionNone in
            let accum, final1 =
               nfa_state accum (NfaActionIncrCounter (counter, min, final.nfa_state_index, max, start1.nfa_state_index))
            in
            let start =
               let states = [start1.nfa_state_index] in
               let states =
                  if min = 0 then
                     final.nfa_state_index :: states
                  else
                     states
               in
                  set_action start (NfaActionResetCounter (counter, states))
            in
            let accum = { accum with nfa_counter = succ counter } in
            let accum, info, start1, states =
               compile accum info start1 final1 states regex
            in
               accum, info, start, start1 :: final1 :: states

         (* Arguments *)
       | RegexArg regex ->
            let { nfa_arg_index = argindex;
                  nfa_arg_table = argtable
                } = accum
            in
            let accum, final1 = nfa_state accum (NfaActionArgStop (argindex, final.nfa_state_index)) in
            let accum, start1 = nfa_state accum NfaActionNone in
            let start = set_action start (NfaActionArgStart (argindex, start1.nfa_state_index)) in
            let { nfa_clause = clause;
                  nfa_arg_number = argnumber;
                  nfa_arg_depends = depends
                } = info
            in
            let arg =
               { arg_index   = argindex;
                 arg_depends = depends;
                 arg_clause  = clause;
                 arg_number  = argnumber
               }
            in
            let accum =
               { accum with nfa_arg_index = succ argindex;
                            nfa_arg_table = IntTable.add argtable argindex arg
               }
            in
            let info =
               { info with nfa_arg_number  = succ argnumber;
                           nfa_arg_depends = IntSet.singleton argindex
               }
            in
            let accum, info, start1, states =
               compile accum info start1 final1 states regex
            in
               accum, info, start, start1 :: final1 :: states

   (*
    * Choice.
    * Map over all the choices; make sure the argument counts match.
    *)
   and compile_choice accum info_orig states regex regexl =
      let accum, start = nfa_state accum NfaActionNone in
      let accum, final = nfa_state accum NfaActionNone in
      let accum, info1, start, states =
         compile accum info_orig start final states regex
      in
      let accum, info, starts, finals, states =
         List.fold_left (fun (accum, info1, starts, finals, states) regex ->
               let accum, start = nfa_state accum NfaActionNone in
               let accum, final = nfa_state accum NfaActionNone in
               let accum, info2, start, states =
                  compile accum info_orig start final states regex
               in
               let info =
                  if info1.nfa_arg_number <> info2.nfa_arg_number then
                     raise (Failure "Regular expression has mismatched argument counts");
                  { info1 with nfa_arg_depends = IntSet.union info1.nfa_arg_depends info2.nfa_arg_depends }
               in
                  accum, info, start :: starts, final :: finals, states) (**)
            (accum, info1, [start], [final], states) regexl
      in
         accum, info, starts, finals, states

   (*
    * Sequence.
    * Chain together the expressions.
    *)
   and compile_sequence accum info start final states regexl =
      match regexl with
         [] ->
            let start = set_action start (NfaActionEpsilon [final.nfa_state_index]) in
               accum, info, start, states
       | [regex] ->
            compile accum info start final states regex
       | regex :: regexl ->
            let accum, middle = nfa_state accum NfaActionNone in
            let accum, info, start, states =
               compile accum info start middle states regex
            in
            let accum, info, middle, states =
               compile_sequence accum info middle final states regexl
            in
               accum, info, start, middle :: states

   (*
    * Compile a clause.
    *)
   let compile_clause accum info states regex =
      let accum, final = nfa_state accum (NfaActionStop info.nfa_clause) in
      let accum, start = nfa_state accum NfaActionNone in
      let accum, info, start, states =
         compile accum info start final states regex
      in
         accum, info, start, final :: states

   (*
    * Create an actual NFA from the regular expression.
    * When lexing, we always start with the previous character
    * in the input (so we can handle \< expressions).
    * Add a new start state with a full set of transitions.
    *)
   let create_nfa exp =
      (* Initial accumulator and info *)
      let accum =
         { nfa_index     = 0;
           nfa_counter   = 0;
           nfa_arg_index = 0;
           nfa_arg_table = IntTable.empty
         }
      in

      (* Compile the expressions *)
      let accum, depends, actions, starts, states =
         List.fold_left (fun (accum, depends, actions, starts, states) (action, id, regex) ->
               let info =
                  { nfa_clause      = id;
                    nfa_arg_number  = 0;
                    nfa_arg_depends = IntSet.empty
                  }
               in
               let accum, info, start, states = compile_clause accum info states regex in
               let actions = IntTable.add actions id action in
               let starts = start.nfa_state_index :: starts in
               let states = start :: states in
               let depends = IntTable.add depends id info.nfa_arg_depends in
                  accum, depends, actions, starts, states) (**)
            (accum, IntTable.empty, IntTable.empty, [], []) exp.exp_clauses
      in

      (* Add the start state *)
      let accum, choice = nfa_state accum (NfaActionEpsilon starts) in
      let accum, start  = nfa_state accum (NfaActionAnySymbol choice.nfa_state_index) in
      let states = start :: choice :: states in
      let length = List.length states in
      let table = Array.create length start in
      let counters =
         let rec collect l i =
            if i = 0 then
               l
            else
               collect (0 :: l) (pred i)
         in
            collect [] accum.nfa_counter
      in
         (* Add all the states to the table *)
         List.iter (fun state ->
               table.(state.nfa_state_index) <- state) states;

         (* Check that the states had unique indexes *)
         Array.iteri (fun i state ->
               assert (state.nfa_state_index = i)) table;

         { nfa_actions     = actions;
           nfa_start       = start.nfa_state_index, counters;
           nfa_table       = table;
           nfa_args        = depends;
           nfa_arg_info    = accum.nfa_arg_table
         }

   (************************************************************************
    * DFA
    *
    * The DFA is computed lazily from the NFA.
    *)

   (************************************************
    * Printing.
    *)

   let pp_print_dfa_set buf closure =
      fprintf buf "@[<b 3>(set";
      List.iter (fun i -> fprintf buf "@ %a" pp_print_nfa_id i) closure;
      fprintf buf ")@]"

   let pp_print_dfa_actions buf action =
      let { dfa_action_stop = stop;
            dfa_action_arg_start_prev = starts_prev;
            dfa_action_arg_stop_prev  = stops_prev;
            dfa_action_arg_stop_cur   = stops_cur
          } = action
      in
      let () =
         fprintf buf "@[<hv 3>(action"
      in
      let () =
         match stop with
            Some stop ->
               fprintf buf "@ stop [%d]" stop
          | None ->
               ()
      in
         IntSet.iter (fun id -> fprintf buf "@ start arg %d prev" id) starts_prev;
         IntSet.iter (fun id -> fprintf buf "@ stop arg %d prev" id) stops_prev;
         IntSet.iter (fun id -> fprintf buf "@ stop arg %d cur" id) stops_cur;
         fprintf buf ")@]"

   let pp_print_dfa_transition buf trans =
      match trans with
         DfaTransition (i, _) ->
            fprintf buf "goto %d" i
       | DfaNoTransition ->
            fprintf buf "error"
       | DfaUnknownTransition ->
            fprintf buf "unknown"

   let pp_print_trans_table buf table =
      fprintf buf "@[<hv 3>(trans";
      Array.iter (fun (key, trans) ->
            fprintf buf "@ %d -> %a" key pp_print_dfa_transition trans) table;
      fprintf buf ")@]"

   (************************************************
    * DFA.
    *)

   (*
    * Action operations.
    *)
   let dfa_action_empty =
     { dfa_action_stop           = None;
       dfa_action_arg_start_prev = IntSet.empty;
       dfa_action_arg_stop_prev  = IntSet.empty;
       dfa_action_arg_stop_cur   = IntSet.empty
     }

   let dfa_action_is_empty action =
      action = dfa_action_empty

   let dfa_action_add_stop action clause =
      match action.dfa_action_stop with
         Some clause' ->
            { action with dfa_action_stop = Some (Action.choose clause clause') }
       | None ->
            { action with dfa_action_stop = Some clause }

   let dfa_action_add_arg_start_prev actions id =
      { actions with dfa_action_arg_start_prev = IntSet.add actions.dfa_action_arg_start_prev id }

   let dfa_action_add_arg_stop_prev pending actions id =
      let { dfa_action_arg_start_prev = starts;
            dfa_action_arg_stop_prev  = stops
          } = actions
      in
      let starts =
         if IntSet.mem pending.dfa_action_arg_start_prev id then
            IntSet.add starts id
         else
            starts
      in
      let stops = IntSet.add stops id in
         { actions with dfa_action_arg_start_prev = starts;
                        dfa_action_arg_stop_prev  = stops
         }

   let dfa_action_add_arg_stop_cur actions id =
      { actions with dfa_action_arg_stop_cur = IntSet.add actions.dfa_action_arg_stop_cur id }

   let dfa_action_union action1 action2 =
      let { dfa_action_stop = stop1;
            dfa_action_arg_start_prev = starts1_prev;
            dfa_action_arg_stop_prev  = stops1_prev;
            dfa_action_arg_stop_cur   = stops1_cur;
          } = action1
      in
      let { dfa_action_stop = stop2;
            dfa_action_arg_start_prev = starts2_prev;
            dfa_action_arg_stop_prev = stops2_prev;
            dfa_action_arg_stop_cur = stops2_cur;
          } = action2
      in
      let stop =
         match stop1, stop2 with
            Some stop1, Some stop2 ->
               Some (Action.choose stop1 stop2)
          | None, _ ->
               stop2
          | _, None ->
               stop1
      in
         { dfa_action_stop = stop;
           dfa_action_arg_start_prev = IntSet.union starts1_prev starts2_prev;
           dfa_action_arg_stop_prev  = IntSet.union stops1_prev stops2_prev;
           dfa_action_arg_stop_cur   = IntSet.union stops1_cur stops2_cur
         }

   (*
    * Evaluate the DFA actions.
    *)
   let dfa_eval_action_stop info action =
      match action.dfa_action_stop with
         Some clause ->
            info.dfa_stop_clause <- clause;
            info.dfa_stop_pos <- Input.lex_pos info.dfa_channel
       | None ->
            ()

   let dfa_eval_action_arg info action =
      let { dfa_args = args;
            dfa_channel = channel
          } = info
      in
      let { dfa_action_arg_start_prev = starts_prev;
            dfa_action_arg_stop_prev = stops_prev;
            dfa_action_arg_stop_cur = stops_cur
          } = action
      in
      let pos = Input.lex_pos channel in
      let add_start_prev args id =
         IntTable.filter_add args id (fun item ->
               let pos = pred pos in
                  match item with
                     None
                   | Some (ArgStarted _) ->
                        ArgStarted pos
                   | Some item ->
                        item)
      in
      let add_stop pos args id =
         IntTable.filter_add args id (fun item ->
               match item with
                  Some (ArgStarted start) ->
                     ArgComplete (start, pos)
                | Some (ArgComplete (start, stop)) ->
                     if pos = stop + 1 then
                        ArgComplete (start, pos)
                     else
                        ArgFinished (start, stop)
                | Some item ->
                     item
                | None ->
                     raise (Invalid_argument "add_stop"))
      in
      let args = IntSet.fold add_start_prev args starts_prev in
      let args = IntSet.fold (add_stop (pos - 1)) args stops_prev in
      let args = IntSet.fold (add_stop pos) args stops_cur in
         info.dfa_args <- args

   let dfa_eval_actions info action =
      if not (action == dfa_action_empty) then
         begin
            dfa_eval_action_stop info action;
            dfa_eval_action_arg info action
         end

   (*
    * We just scanned a symbol c
    * in NFA state nid.  Compute the forward epsilon closure,
    * and all the actions we should take.  We return only the
    * frontier (the nodes where we can't make progress) as the next state.
    *)
   let rec close_prev table nid c closure frontier actions =
      if DfaState.mem closure nid then
         closure, frontier, actions
      else
         let index, counters = nid in
         let closure = DfaState.add closure nid in
         let action = table.(index).nfa_state_action in
            if !debug_lexgen then
               eprintf "@[<v 3>close_prev:@ NFA state: %a@ Symbol: %a@ @[<hv 3>Closure:@ %a@]@ @[<hv 3>Frontier:@ %a@]@ @[<hv 3>NFA Action:@ %a@]@ @[<hv 3>Actions: %a@]@]@." (**)
                  pp_print_nfa_id nid
                  pp_print_char c
                  pp_print_dfa_set closure
                  pp_print_dfa_set frontier
                  pp_print_nfa_action action
                  pp_print_dfa_actions actions;
            match action with
               NfaActionEpsilon nids ->
                  let nids = List.map (fun index -> index, counters) nids in
                     close_prev_list table nids c closure frontier actions

               (* Reached a final state *)
             | NfaActionStop id ->
                  let actions = dfa_action_add_stop actions id in
                     closure, frontier, actions

               (* Counter operations *)
             | NfaActionResetCounter (i, nids) ->
                  let counters = NfaState.reset_counter counters i in
                  let nids = List.map (fun index -> index, counters) nids in
                     close_prev_list table nids c closure frontier actions

             | NfaActionIncrCounter (i, min, final, max, start) ->
                  let nids = NfaState.incr_counter counters i min final max start in
                     close_prev_list table nids c closure frontier actions

               (* Can only make progress if the current symbol is allowed *)
             | NfaActionLimitPrev (syms, nid) ->
                  if List.mem c syms then
                     close_prev table (nid, counters) c closure frontier actions
                  else
                     closure, frontier, actions

               (* Handle argument termination eagerly *)
             | NfaActionArgStop (id, nid) ->
                  let actions = dfa_action_add_arg_stop_cur actions id in
                     close_prev table (nid, counters) c closure frontier actions

              (* Reached the frontier *)
             | NfaActionArgStart _
             | NfaActionSymbol _
             | NfaActionAnySymbol _
             | NfaActionExceptSymbol _
             | NfaActionLimitNext _ ->
                  closure, DfaState.add frontier nid, actions

               (* Minor optimization, because we know that we can't make progress *)
             | NfaActionNone ->
                  closure, frontier, actions

   and close_prev_list table nids c closure frontier actions =
      List.fold_left (fun (closure, frontier, actions) nid ->
            close_prev table nid c closure frontier actions) (closure, frontier, actions) nids

   (*
    * We are now processing symbol c in NFA state nid.
    * Process the symbol, then close_prev.
    *)
   let rec close_next table nid c closure frontier pending committed =
      if DfaState.mem closure nid then
         closure, frontier, committed
      else
         let closure = DfaState.add closure nid in
         let index, counters = nid in
         let action = table.(index).nfa_state_action in
            if !debug_lexgen then
               eprintf "@[<v 3>close_next:@ NFA state: %a@ Symbol: %a@ @[<hv 3>Closure:@ %a@]@ @[<hv 3>Frontier:@ %a@]@ @[<hv 3>NFA Action:@ %a@]@ @[<hv 3>Committed:@ %a@]@]@." (**)
                  pp_print_nfa_id nid
                  pp_print_char c
                  pp_print_dfa_set closure
                  pp_print_dfa_set frontier
                  pp_print_nfa_action action
                  pp_print_dfa_actions committed;
            match action with
               NfaActionEpsilon nids ->
                  let nids = List.map (fun index -> index, counters) nids in
                     close_next_list table nids c closure frontier pending committed

             | NfaActionStop id ->
                  let committed = dfa_action_add_stop committed id in
                     closure, frontier, committed

             | NfaActionArgStart (id, nid) ->
                  let pending = dfa_action_add_arg_start_prev pending id in
                     close_next table (nid, counters) c closure frontier pending committed

             | NfaActionArgStop (id, nid) ->
                  let committed = dfa_action_add_arg_stop_prev pending committed id in
                     close_next table (nid, counters) c closure frontier pending committed

             | NfaActionSymbol (syms, nid) when List.mem c syms ->
                  let _, frontier, committed =
                     close_prev table (nid, counters) c DfaState.empty frontier (dfa_action_union pending committed)
                  in
                     closure, frontier, committed

             | NfaActionExceptSymbol (syms, nid) when not (c = eof || List.mem c syms) ->
                  let _, frontier, committed =
                     close_prev table (nid, counters) c DfaState.empty frontier (dfa_action_union pending committed)
                  in
                     closure, frontier, committed

             | NfaActionAnySymbol nid when c <> eof ->
                  let _, frontier, committed =
                     close_prev table (nid, counters) c DfaState.empty frontier (dfa_action_union pending committed)
                  in
                     closure, frontier, committed

             | NfaActionLimitNext (syms, nid) when List.mem c syms ->
                  close_next table (nid, counters) c closure frontier pending committed

               (* Counter operations *)
             | NfaActionResetCounter (i, nids) ->
                  let counters = NfaState.reset_counter counters i in
                  let nids = List.map (fun index -> index, counters) nids in
                     close_next_list table nids c closure frontier pending committed

             | NfaActionIncrCounter (i, min, final, max, start) ->
                  let nids = NfaState.incr_counter counters i min final max start in
                     close_next_list table nids c closure frontier pending committed

               (* Anything else is a failure *)
             | NfaActionNone
             | NfaActionSymbol _
             | NfaActionAnySymbol _
             | NfaActionExceptSymbol _
             | NfaActionLimitPrev _
             | NfaActionLimitNext _ ->
                  closure, frontier, committed

   and close_next_list table nids c closure frontier pending committed =
      List.fold_left (fun (closure, frontier, committed) nid ->
            close_next table nid c closure frontier pending committed) (closure, frontier, committed) nids

   (*
    * The next state is the frontier.
    *)
   let close_next_state table nids c =
      let _, frontier, actions = close_next_list table nids c DfaState.empty DfaState.empty dfa_action_empty dfa_action_empty in
         if !debug_lex then
            eprintf "@[<hv 3>NFA transition:@ @[<hv 3>current:@ %a@]@ symbol: %a@ @[<hv 3>next:@ %a@]@ @[<hv 3>actions:@ %a@]@." (**)
               pp_print_dfa_set nids
               pp_print_char c
               pp_print_dfa_set frontier
               pp_print_dfa_actions actions;
         frontier, actions

   (*
    * Add a state to the DFA.  It is initially empty.
    *)
   let dfa_find_state dfa nids =
      let { dfa_map    = map;
            dfa_length = dfa_id;
            dfa_states = states
          } = dfa
      in
         try DfaStateTable.find map nids with
            Not_found ->
               (* Make a new state *)
               let dfa_state =
                  { dfa_state_index = dfa_id;
                    dfa_state_set   = nids;
                    dfa_state_delta = TransTable.empty
                  }
               in
               let () =
                  (* Add to the map *)
                  dfa.dfa_map <- DfaStateTable.add map nids dfa_id
               in
               let () =
                  (* Add to the state array *)
                  let length = Array.length states in
                     if dfa_id = length then
                        let new_states = Array.create (length * 2) dfa_state in
                           Array.blit states 0 new_states 0 length;
                           dfa.dfa_states <- new_states
                     else
                        states.(dfa_id) <- dfa_state
               in
                  dfa.dfa_length <- succ dfa_id;
                  dfa_id

   (*
    * We are in DFA state i, processing symbol c, but we don't have
    * an entry in the transition table yet.
    *)
   let create_entry dfa dfa_state c =
      let { dfa_table = table } = dfa in
      let { dfa_state_set = nids;
            dfa_state_delta = delta
          } = dfa_state
      in
      let frontier, actions = close_next_state table nids c in
         if frontier = [] && dfa_action_is_empty actions then
            dfa_state.dfa_state_delta <- TransTable.add delta c DfaNoTransition
         else
            let dfa_id = dfa_find_state dfa frontier in
            let entry = DfaTransition (dfa_id, actions) in
               dfa_state.dfa_state_delta <- TransTable.add delta c entry

   (*
    * Transition function.
    * We are in DFA state dfa_id, processing symbol c.
    * Returns None if there is no transition.
    *)
   let rec dfa_delta dfa dfa_info dfa_state c =
      match TransTable.find dfa_state.dfa_state_delta c DfaUnknownTransition with
         DfaTransition (dfa_id, actions) ->
            if !debug_lex then
               eprintf "State %d %a: symbol %a, goto %d@." (**)
                  dfa_state.dfa_state_index
                  pp_print_dfa_set dfa_state.dfa_state_set
                  pp_print_char c dfa_id;
            dfa_eval_actions dfa_info actions;
            Some (dfa.dfa_states.(dfa_id))
       | DfaNoTransition ->
            if !debug_lex then
               eprintf "State %d %a: no transition for symbol %a@." (**)
                  dfa_state.dfa_state_index
                  pp_print_dfa_set dfa_state.dfa_state_set
                  pp_print_char c;
            None
       | DfaUnknownTransition ->
            if !debug_lex then
               eprintf "State %d %a: computing transition on symbol %a@." (**)
                  dfa_state.dfa_state_index
                  pp_print_dfa_set dfa_state.dfa_state_set
                  pp_print_char c;
            create_entry dfa dfa_state c;
            dfa_delta dfa dfa_info dfa_state c

   (*
    * Collect the arguments, given that all smaller arguments
    * have already been collected.
    *)
   let rec dfa_arg_search table arg depends =
      match depends with
         id :: depends ->
            (match table.(id) with
                Some args ->
                   Some (arg :: args)
              | None ->
                   dfa_arg_search table arg depends)
       | [] ->
            None

   let dfa_arg info line table arg =
      let { arg_index = index; arg_depends = depends } = arg in
      let args =
         try
            match IntTable.find info.dfa_args index with
               ArgStarted _ ->
                  None
             | ArgComplete (start, stop)
             | ArgFinished (start, stop) ->
                  let arg = String.sub line start (stop - start) in
                     match IntSet.to_list depends with
                        [] ->
                           Some [arg]
                      | depends ->
                           dfa_arg_search table arg depends

         with
            Not_found ->
               None
      in
         table.(index) <- args

   let dfa_args dfa info line id =
      (* First, define a table of possible arguments *)
      let table =
         let len = IntTable.cardinal dfa.dfa_arg_table in
         let table = Array.create len None in
            IntTable.iter (fun _ arg -> dfa_arg info line table arg) dfa.dfa_arg_table;
            table
      in

      (* Now choose the first one where the args are valid *)
      let rec search idl =
         match idl with
            [] ->
               []
          | id :: idl ->
               match table.(id) with
                  Some args ->
                     List.rev args
                | None ->
                     search idl
      in
         search (IntSet.to_list (IntTable.find dfa.dfa_arg_depends id))

   (*
    * Now the complete lexer.
    * We scan forward until no more transitions are possible.
    * Then return the last match.
    *)
   let lex dfa channel =
      let dfa_info =
         { dfa_stop_clause = -1;
           dfa_stop_pos    = 0;
           dfa_start_pos   = 0;
           dfa_args        = IntTable.empty;
           dfa_channel     = channel
         }
      in
      let rec loop dfa_state c =
         match dfa_delta dfa dfa_info dfa_state c with
            Some dfa_state ->
               loop dfa_state (Input.lex_next channel)
          | None ->
               ()
      in
      let dfa_state = dfa.dfa_states.(0) in
      let c = Input.lex_start channel in
      let () = loop dfa_state c in

      (* Now figure out what happened *)
      let { dfa_stop_clause = clause;
            dfa_stop_pos    = stop;
            dfa_args        = args
          } = dfa_info
      in
         (*
          * If we did not get a match, return the channel to
          * the starting position, and raise an exception.
          *)
         if clause < 0 then
            begin
               Input.lex_stop channel 0;
               raise (Failure "lex: no clause matched")
            end;

         (*
          * We have the clause:
          *   1. Set the channel to the final position
          *   2. Get the entire string.
          *   3. Get the arguments.
          *)
         let loc = Input.lex_loc channel stop in
         let lexeme = Input.lex_string channel stop in
         let args = dfa_args dfa dfa_info lexeme clause in
            Input.lex_stop channel stop;
            IntTable.find dfa.dfa_action_table clause, loc, lexeme, args

   (*
    * Return the input followed by a regular expression
    * terminator.
    *)
   let search dfa channel =
      let dfa_info =
         { dfa_stop_clause = -1;
           dfa_stop_pos    = 0;
           dfa_start_pos   = 0;
           dfa_args        = IntTable.empty;
           dfa_channel     = channel
         }
      in
      let start_state = dfa.dfa_states.(0) in
      let rec loop dfa_state c =
         match dfa_delta dfa dfa_info dfa_state c with
            Some dfa_state ->
               loop dfa_state (Input.lex_next channel)
          | None ->
               if dfa_info.dfa_stop_clause < 0 then
                  begin
                     (*
                      * The proper way to backtrack would be to do something
                      * like change the regex R to .*\(R\)  However, we don't
                      * want to modify the DFA, so we take this potentially
                      * quadratic hit.
                      *)
                     Input.lex_restart channel dfa_info.dfa_start_pos;
                     search (Input.lex_next channel)
                  end
      and search c =
         match dfa_delta dfa dfa_info start_state c with
            Some dfa_state ->
               dfa_info.dfa_start_pos <- Input.lex_pos channel;
               loop dfa_state (Input.lex_next channel)
          | None ->
               if c = eof then
                  dfa_info.dfa_start_pos <- Input.lex_pos channel
               else
                  search (Input.lex_next channel)
      in
      let c = Input.lex_start channel in
      let () = search c in

      (* Now figure out what happened *)
      let { dfa_stop_clause = clause;
            dfa_stop_pos    = stop;
            dfa_start_pos   = start;
            dfa_args        = args
          } = dfa_info
      in
      let skipped = Input.lex_string channel start in
      let matched =
         if clause < 0 then
            None
         else
            let lexeme = Input.lex_string channel stop in
            let args = dfa_args dfa dfa_info lexeme clause in
               Some (IntTable.find dfa.dfa_action_table clause, String.sub lexeme start (stop - start), args)
      in
         Input.lex_stop channel (max start stop);
         skipped, matched

   (*
    * Just check for a string match.
    *)
   let matches dfa channel =
      match search dfa channel with
         _, None ->
            false
       | _, Some _ ->
            true

   (*
    * Create the DFA from a list of regular expressions.
    *)
   let create exp =
      let nfa = create_nfa exp in
      let () =
         if !debug_lexgen || !debug_lex then
            eprintf "%a@." pp_print_nfa nfa
      in
      let { nfa_table    = nfa_table;
            nfa_start    = nfa_start;
            nfa_actions  = actions;
            nfa_args     = nfa_args;
            nfa_arg_info = nfa_arg_table
          } = nfa
      in
      let nfa_start = [nfa_start] in
      let start =
         { dfa_state_index  = 0;
           dfa_state_set    = nfa_start;
           dfa_state_delta  = TransTable.empty
         }
      in
         { dfa_states       = Array.create 64 start;
           dfa_length       = 1;
           dfa_map          = DfaStateTable.add DfaStateTable.empty nfa_start 0;
           dfa_table        = nfa_table;
           dfa_action_table = actions;
           dfa_arg_depends  = nfa_args;
           dfa_arg_table    = nfa_arg_table
         }

   (*
    * External functions.
    *)
   let empty =
      { lex_exp = empty_exp;
        lex_dfa = None
      }

   let add_clause lex action s =
      let arity, exp = add_clause_exp lex.lex_exp action s in
      let lex = { lex_exp = exp; lex_dfa = None } in
         arity, lex

   let remove_clause lex action =
      { lex_exp = remove_clause_exp lex.lex_exp action;
        lex_dfa = None
      }

   (*
    * Take the union of two lexers.
    * We assume that if we have seen a clause before,
    * then we have seen all the rest of the clauses too.
    *)
   let union info1 info2 =
      let { lex_exp = exp1; lex_dfa = dfa1 } = info1 in
      let { lex_exp = exp2; lex_dfa = dfa2 } = info2 in
         (* Catch degenerate cases first *)
         match exp1.exp_clauses, exp2.exp_clauses with
            [], _ -> info2
          | _, [] -> info1
          | _ ->
               let changed, exp = union_exp exp1 exp2 in
                  if changed then
                     { lex_exp = exp; lex_dfa = None }
                  else
                     info1

   let dfa_of_info info =
      match info.lex_dfa with
         Some dfa ->
            dfa
       | None ->
            let dfa = create info.lex_exp in
               info.lex_dfa <- Some dfa;
               dfa

   let lex info channel =
      lex (dfa_of_info info) channel

   let search info channel =
      search (dfa_of_info info) channel

   let matches info channel =
      matches (dfa_of_info info) channel

   let compile info =
      ignore (dfa_of_info info)

   let pp_print_lexer buf info =
      let { lex_exp = exp } = info in
      let dfa = dfa_of_info info in
         fprintf buf "@[<v 0>@[<hv 3>Lexer:@ %a@]" pp_print_exp exp;
         fprintf buf "@ @[<hv 3>NFA:";
         Array.iter (fun nfa_state -> fprintf buf "@ %a" pp_print_nfa_state nfa_state) dfa.dfa_table;
         fprintf buf "@]@]"

   let hash info =
      Hashtbl.hash_param max_int max_int info.lex_exp
end

(************************************************************************
 * Simplified Str replacement.
 *)
module LmAction =
struct
   type action = int

   let pp_print_action = pp_print_int

   let hash i = i
   let compare (i : int) (j : int) =
      if i < j then
         -1
      else if i > j then
         1
      else
         0

   let choose = min
end

module LmLexer = MakeLexer (Lm_channel.LexerInput) (LmAction)

module LmStr =
struct
   type t = LmLexer.t

   (*
    * Create a regular expression.
    *)
   let regexp s =
      snd (LmLexer.add_clause LmLexer.empty 0 s)

   (*
    * Perform the match.
    *)
   let string_match info s off =
      let input = Lm_channel.of_substring s off (String.length s - off) in
         try
            let _ = LmLexer.lex info input in
               true
         with
            Failure _ ->
               false
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
