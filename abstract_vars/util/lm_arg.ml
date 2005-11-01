(*
 * Parsing command line arguments, MCC-style. Arguments to options
 * may be separated from the option by a space, or may be placed
 * immediately after the option (without space) IF the option is
 * not ambiguous.  Also, options may be abbreviated as long as the
 * short form is not ambiguous.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2002, Justin David Smith, Caltech
 * Based on original code, Copyright (C) 2000 Jason Hickey, Caltech
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
open Format
open Lm_format_util


(***  Basic Specifications  ***)


(* argv, argv_len
   Information about the command-line arguments. *)
let argv = Sys.argv
let argv_len = Array.length argv


(* spec
   Argument specification.  Each option uses this specification to indicate
   what type of argument (if any) the option takes.  The following option
   specifications are defined.
      Unit f:     Call an arbitrary function f ()
      Set b:      Set the boolean (reference) value b to true
      Clear b:    Set the boolean (reference) value b to false
      String f:   Takes one argument:  call function f <string>
      Int f:      Takes one argument:  call function f <integer>
      Float f:    Takes one argument:  call function f <float>
      Rest f:     Call function f <arg>, for all remaining arguments

   section = (name, spec, desc) list
   Used to define a group of related arguments.  (name, spec) indicate the
   option name and option specification.  desc gives a textual description
   of the option.

   sections = (desc, section) list
   Used to define all option groups.  Each option group is prefixed by desc
   which briefly describes the section.
 *)
type spec =
   Unit of (unit -> unit)
 | Set of bool ref
 | Clear of bool ref
 | String of (string -> unit)
 | Int of (int -> unit)
 | Float of (float -> unit)
 | Rest of (string -> unit)

type section = (string * spec * string) list

type sections = (string * section) list


(* Bad
   Thrown by option processing when something goes wrong...  *)
exception Bad of string


(***  Option Table  ***)


(* CharCompare, CharTable
   Defines a table indexed by individual characters.  *)
module CharCompare = struct
   type t = char
   let compare = Pervasives.compare
end (* CharCompare *)

module CharTable = Lm_map.LmMake (CharCompare)


(* options
   The option table is a tree, where each edge is labelled by a character.
   To lookup the specification for an option, we walk the tree using the
   characters of the option until we reach a node that has a specification
   associated with it.  This tree is used to help us identify unambiguous
   prefixes, and also to determine where an option name ends and its value
   begins (when the name and value are not space-delimited).

   option_node
   The type of a node in the options tree.  Each node contains a spec if
   the node matches an option name, and may contain a subtree if there is
   at least one longer option that has this prefix.
      SpecNode spec:    Leaf node; this branch corresponds to the spec.
      NameNode tree:    No option corresponds to this branch, but there
                        are options in the subtree.
      SpecOrName (spec, tree):
                        This branch corresponds to an option with the
                        indicated spec; there are also suboptions in the
                        indicated subtree.
 *)
type option_node =
   SpecNode of spec
 | NameNode of option_node CharTable.t
 | SpecOrName of spec * option_node CharTable.t

type options = option_node CharTable.t


(* char_table_lookup
   Lookup an entry in the char table.  If no entry exists in the table,
   then None is returned (instead of raising an exception).  *)
let char_table_lookup table ch =
   try
      Some (CharTable.find table ch)
   with
      Not_found ->
         (* If the character is '_', try looking it up as '-'. This is a
            hack to accomodate both '_' and '-' in option names (proper
            GCC style uses hyphen, but our old options used underscores). *)
         if ch = '_' then
            try
               Some (CharTable.find table '-')
            with
               Not_found ->
                  None
         else
            None


(* add_option
   Add a new option name to the option tree.  If the exact option already
   exists, then an exception is thrown.  If a prefix or suffix of this
   option is already defined, then no error occurs.  *)
let add_option options name spec =
   let length = String.length name in

   (* deconstruct_name
      Updates the subtree rooted at options, based on the substring
      of name beginning with offset.  *)
   let rec deconstruct_name options offset =
      let ch = name.[offset] in
      let offset = offset + 1 in
      let entry =
         if offset < length then

            (* This is NOT the last character of the option; we
               need to build a subtree and recurse on ourself.  *)
            match char_table_lookup options ch with
               None ->
                  NameNode (deconstruct_name CharTable.empty offset)
             | Some (SpecNode spec') ->
                  SpecOrName (spec', deconstruct_name CharTable.empty offset)
             | Some (NameNode options) ->
                  NameNode  (deconstruct_name options offset)
             | Some (SpecOrName (spec', options)) ->
                  SpecOrName (spec', deconstruct_name options offset)
         else

            (* This is the last character of the option; this is
               where we might have a duplicate hit, and where we
               need to drop our specification.  *)
            match char_table_lookup options ch with
               None ->
                  SpecNode spec
             | Some (NameNode options) ->
                  SpecOrName (spec, options)
             | Some _ ->
                  raise (Bad ("Duplicate option defined: " ^ name))
      in
         (* Update this node in the tree *)
         CharTable.add options ch entry
   in
      deconstruct_name options 0


(* lookup_option
   Lookup the option with the indicated name in the options tree.  If there
   is an exact option match in the tree, we return the option spec and an
   empty string.  If we hit end up at a node without a spec, but we are an
   UNAMBIGUOUS prefix of an option in the tree, then we return that option's
   spec, and an empty string.

   The final case is more interesting:  when we end up at a leaf, then we
   split the ``name'' we were given into a name/value pair at that point,
   and return the excess characters as the option's value.  This is how we
   determine when the value associated with an option is not delimited by a
   space.  Note that any option which is a prefix of another option cannot
   take a value in this way.
 *)
let lookup_option options name =
   let length = String.length name in

   (* find_branch
      Checks to see if the subtree rooted at options is a linear branch.
      If so, return the spec at the end of the branch; otherwise, raise an
      exception (assuming the option was ambiguous if the branch splits,
      or that the option is unbound if there is no branch).  *)
   let rec find_branch options =
      CharTable.fold (fun spec _ options ->
         match spec, options with
            None, SpecNode spec ->
               Some spec
          | None, NameNode options ->
               find_branch options
          | _ ->
               raise (Bad ("Ambiguous option specified: " ^ name))) None options
   in
   let find_branch options =
      match find_branch options with
         None ->
            raise (Bad ("No such option: " ^ name))
       | Some spec ->
            spec
   in

   (* lookup_name
      Lookup an option in the subtree rooted at options, based on the
      substring of name beginning at offset.  *)
   let rec lookup_name options offset =
      let ch = name.[offset] in
      let offset = offset + 1 in
         if offset < length then

            (* We're not at the end of the name we're searching for
               yet; it is possible that we are looking at a name/value
               pair. *)
            match char_table_lookup options ch with
               None ->
                  (* No option with this prefix was defined *)
                  raise (Bad ("No such option: " ^ name))
             | Some (SpecNode spec) ->
                  (* Name was too long; assume it was a name/value pair *)
                  spec, String.sub name offset (length - offset)
             | Some (NameNode options)
             | Some (SpecOrName (_, options)) ->
                  (* Still searching... *)
                  lookup_name options offset
         else

            (* Last character in the name we were given; this is either
               an exact match, or (hopefully) an unambiguous prefix of
               an option in the tree. *)
            match char_table_lookup options ch with
               None ->
                  (* Last char of name, not no option matches *)
                  raise (Bad ("No such option: " ^ name))
             | Some (SpecNode spec)
             | Some (SpecOrName (spec, _)) ->
                  (* Exact match to an option in the tree. *)
                  spec, ""
             | Some (NameNode options) ->
                  (* Inexact match; try to find a branch. *)
                  find_branch options, ""
   in
      lookup_name options 0


(* compute_option_tree
   Convert a sections spec into an option tree.  Can raise an exception
   if the sections spec contains duplicate options.  *)
let compute_option_tree spec =
   let options = CharTable.empty in
   let options = List.fold_left (fun options (_, spec_block) ->
      List.fold_left (fun options (name, spec, _) ->
         add_option options name spec) options spec_block) options spec
   in
      options


(***  Help System  ***)


(* usage
   Display the usage message and help text for the options.  *)
let usage spec =
   List.iter (fun (opt, spec, doc) ->
      (* Descriptive text for the option argument *)
      let arg =
         match spec with
             Unit _
           | Set _
           | Clear _ ->
                ""
           | String _ ->
                " <string>"
           | Int _ ->
                " <number>"
           | Float _ ->
                " <float>"
           | Rest _ ->
                " ..."
      in
      let opt = opt ^ arg in

         (* Display information on a single option. *)
         if String.length opt > 20 then
            (* option name too long to fit on one line *)
            printf "@ %s@ %20s" opt ""
         else
            printf "@ %-20s" opt;
         printf ":  ";
         pp_print_paragraph std_formatter doc) spec

let usage spec usage_msg =
   (* Display help for all sections. *)
   printf "@[<v 0>%s" usage_msg;
   List.iter (fun (section, spec) ->
      printf "@ @ @[<v 3>%s:" section;
      usage spec;
      printf "@]") spec;
   printf "@]@."


(***  Option Processing  ***)


(* get_next_arg
   Get the next argument in the argument stream.  Returns
   the argument string, as well as the new current marker.  *)
let get_next_arg current =
   if current < argv_len then
      argv.(current), current + 1
   else
      raise (Bad "Missing argument")


(* parse
   Parses the program arguments, using a sections specification.  Any
   non-option argument is passed to the default function, in order; if
   -help or --help is intercepted on the argument stream, then the
   usage message is displayed.  *)
let parse spec default usage_msg =
   (* Convert spec into an options tree, for easier parsing *)
   let options = compute_option_tree spec in

   (* Parse a single option *)
   let rec parse_option current =
      if current < argv_len then
         (* Get the name of the option *)
         let opt, current = get_next_arg current in
         let current =
            if opt = "-help" || opt = "--help" then begin
               usage spec usage_msg;
               exit 1
            end else if String.length opt > 0 && opt.[0] = '-' then
               (* Get information on the option *)
               let spec, value = lookup_option options opt in

               (* If no value was embedded in the option, but the option
                  requires a value, then grab the next argument for its
                  value.  *)
               let value, current =
                  match spec, value with
                     String _,   ""
                   | Int _,      ""
                   | Float _,    "" ->
                        get_next_arg current
                   | Unit _,     ""
                   | Set _,      ""
                   | Clear _,    ""
                   | String _,   _
                   | Int _,      _
                   | Float _,    _ ->
                        value, current
                   | Rest f,     "" ->
                        let rec rest_function current =
                           if current < argv_len then begin
                              f argv.(current);
                              rest_function (current + 1)
                           end else
                              "", current
                        in
                           rest_function current
                   | _ ->
                        raise (Bad "Option cannot accept an argument")
               in

               (* Actually process the option. *)
               let () =
                  match spec with
                     Unit f ->
                        f ()
                   | Set x ->
                        x := true
                   | Clear x ->
                        x := false
                   | String f ->
                        f value
                   | Int f ->
                        f (int_of_string value)
                   | Float f ->
                        f (float_of_string value)
                   | Rest f ->
                        ()
               in
                  current
            else begin
               (* Not an option; pass to the default function *)
               default opt;
               current
            end
         in
            (* We're done with this option, advance to next *)
            parse_option current
      else
         (* No options left to parse; we're done! *)
         ()
   in
      parse_option 1


let parse spec default usage_msg =
   try
      parse spec default usage_msg
   with
      Bad s ->
         prerr_string "Error: ";
         prerr_endline s;
         exit 1
