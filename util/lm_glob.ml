(*
 * Glob expansion.
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
open Lm_filename_util

exception GlobException

(************************************************************************
 * Shell regular expressions.
 *)
let add_shell_pattern buf s =
   let len = String.length s in
      Buffer.add_string buf "^";
      for i = 0 to pred len do
         let c = s.[i] in
            match s.[i] with
               '*' ->
                  Buffer.add_string buf ".*"
             | '?' ->
                  Buffer.add_string buf "."
             | '.'
             | '+'
             | '^'
             | '$' ->
                  Buffer.add_char buf '\\';
                  Buffer.add_char buf c
             | _ ->
                  Buffer.add_char buf c
      done;
      Buffer.add_char buf '$'

let add_shell_disjunct buf s =
   Buffer.add_string buf "\\|";
   add_shell_pattern buf s

let regexp_of_shell_pattern s =
   let buf = Buffer.create 32 in
      add_shell_pattern buf s;
      Str.regexp (Buffer.contents buf)

let make_filter sl default =
   let buf = Buffer.create 32 in
      match sl with
         s :: sl ->
            add_shell_pattern buf s;
            List.iter (add_shell_disjunct buf) sl;
            let pattern = Str.regexp (Buffer.contents buf) in
               (fun name -> Str.string_match pattern name 0)
       | [] ->
            (fun name -> default)

(*
 * These are the files that CVS ignores by default.
 * https://www.cvshome.org/docs/manual/cvs-1.11.16/cvs_18.html#IDX266
 *)
let default_patterns =
   ["RCS";
    "SCCS";
    "CVS";
    "CVS.adm";
    "RCSLOG";
    "cvslog.*";
    "tags";
    "TAGS";
    ".make.state";
    ".nse_depinfo";
    "*~";
    "#*";
    ".#*";
    ",*";
    "_$*";
    "*$";
    "*.old";
    "*.bak";
    "*.BAK";
    "*.orig";
    "*.rej";
    ".del-*";
    "*.a";
    "*.olb";
    "*.o";
    "*.obj";
    "*.so";
    "*.exe";
    "*.Z";
    "*.elc";
    "*.ln";
    "core";
    "core.*"]

let stdignore =
   let buf = Buffer.create 256 in
      Buffer.add_string buf "^\\.cvsignore$";
      List.iter (add_shell_disjunct buf) default_patterns;
      Str.regexp (Buffer.contents buf)

(*
 * Load the ignore expression from .cvsignore.
 *)
let load_cvsignore dirname =
   let filename = Filename.concat dirname ".cvsignore" in

   (* Get the patterns from the file *)
   let inx = open_in filename in
   let rec collect patterns =
      try collect (Lm_string_util.tokens_std (input_line inx) @ patterns) with
         End_of_file ->
            patterns
   in
   let patterns = collect [] in
   let () = close_in inx in

   (* Concatenate them into a large regular expression *)
   let buf = Buffer.create 256 in
      Buffer.add_string buf "^\\.cvsignore$";
      List.iter (add_shell_disjunct buf) default_patterns;
      List.iter (add_shell_disjunct buf) patterns;
      Str.regexp (Buffer.contents buf)

let load_cvsignore dirname =
   let pattern =
      try load_cvsignore dirname with
         Sys_error _ ->
            stdignore
   in
      (fun name -> Str.string_match pattern name 0)

(*
 * Check if a filename refers to a directory.
 *)
let is_dir filename =
   try (Unix.lstat filename).Unix.st_kind = Unix.S_DIR with
      Unix.Unix_error _ ->
         false

(************************************************************************
 * Glob expansion.
 *)
type glob_options =
   GlobNoBraces         (* Do not perform csh-style brace expansion *)
 | GlobNoEscape         (* The \ character does not escape special characters *)
 | GlobNoCheck          (* If an expansion fails, return the expansion literally *)
 | GlobDot              (* Allow wildcards to match filenames with a leading . *)
 | GlobOnlyDirs         (* Return only directories in the result *)
 | GlobCVSIgnore        (* Ignore files as specified by .cvsignore files *)
 | GlobIgnore of string list  (* Ignore the files that match the pattern *)
 | GlobAllow of string list   (* Allow only files that match the pattern *)
 | GlobIgnoreFun of (string -> bool)  (* Ignore the files determined by the function *)
 | GlobAllowFun of (string -> bool)   (* Allow only the files determined by the function *)

type glob_option_bits =
   { glob_braces    : bool;
     glob_escape    : bool;
     glob_check     : bool;
     glob_dot       : bool;
     glob_dirs      : bool;
     glob_cvs       : bool;
     glob_ignore    : (string -> bool);
     glob_allow     : (string -> bool);
     glob_cvsignore : (string -> bool)
   }

let default_glob_options =
   { glob_braces = true;
     glob_escape = true;
     glob_check  = true;
     glob_dot    = false;
     glob_dirs   = false;
     glob_cvs    = false;
     glob_ignore = (fun _ -> false);
     glob_allow  = (fun _ -> true);
     glob_cvsignore = (fun _ -> false)
   }

(*
 * Collect glob options.
 *)
let glob_options_of_list l =
   let rec collect options l =
      match l with
         option :: l ->
            let options =
               match option with
                  GlobNoBraces  -> { options with glob_braces = false }
                | GlobNoEscape  -> { options with glob_escape = false }
                | GlobNoCheck   -> { options with glob_check = false }
                | GlobDot       -> { options with glob_dot = true }
                | GlobOnlyDirs  -> { options with glob_dirs = true }
                | GlobCVSIgnore -> { options with glob_cvs = true }
                | GlobIgnoreFun f -> { options with glob_ignore = f }
                | GlobAllowFun f  -> { options with glob_allow = f }
                | GlobIgnore sl   -> { options with glob_ignore = make_filter sl false }
                | GlobAllow sl    -> { options with glob_allow = make_filter sl true }
            in
               collect options l
       | [] ->
            options
   in
      collect default_glob_options l

(*
 * Determine if a string contains glob characters.
 *)
let is_glob_name options name =
   let len = String.length name in
   let rec search i =
      if i >= len then
         false
      else
         match name.[i] with
            '*'
          | '?'
          | '[' ->
               true
          | '\\' when options.glob_escape ->
               search (i + 2)
          | _ ->
               search (succ i)
   in
      search 0

(*
 * Perform brace expansion.
 *)
let rec expand_braces options expanded_names unexpanded_names =
   match unexpanded_names with
      name :: unexpanded_names ->
         let len = String.length name in
         let expanded_names, unexpanded_names =
            (* Search for the first brace *)
            let rec search_brace i =
               if i >= len then
                  name :: expanded_names, unexpanded_names
               else
                  match name.[i] with
                     '\\' when options.glob_escape ->
                        search_brace (i + 2)
                   | '{' ->
                        search_found 0 i (i + 1) [] (i + 1)
                   | _ ->
                        search_brace (i + 1)

            (* Found a brace, search for the parts *)
            and search_found level start last names i =
               if i >= len then
                  raise GlobException;

               match name.[i] with
                  '\\' when options.glob_escape ->
                     search_found level start last names (i + 2)
                | ',' when level = 0 ->
                     let name = String.sub name last (i - last) in
                        search_found level start (i + 1) (name :: names) (i + 1)
                | '{' ->
                      search_found (succ level) start last names (i + 1)
                | '}' when level = 0 ->
                      let name = String.sub name last (i - last) in
                      let names = name :: names in
                      let pref = String.sub name 0 start in
                      let suf = String.sub name i (len - i) in
                      let names = List.map (fun s -> pref ^ s ^ suf) names in
                         expanded_names, List.rev_append names unexpanded_names
                | '}' ->
                      search_found (pred level) start last names (i + 1)
                | _ ->
                      search_found level start last names (i + 1)
            in
               search_brace 0
         in
            expand_braces options expanded_names unexpanded_names
    | [] ->
          expanded_names

let glob_braces options names =
   if options.glob_braces then
      expand_braces options [] names
   else
      names

(*
 * Expand a glob pattern.
 * the dir is a fully-expanded directory name.
 *)
let glob_dir_pattern options dirs names dir pattern =
   let options =
      if options.glob_cvs then
         { options with glob_cvsignore = load_cvsignore dir }
      else
         options
   in
   let dirx = Unix.opendir dir in
   let rec collect dirs names =
      let name =
         try Some (Unix.readdir dirx) with
            End_of_file ->
               None
      in
         match name with
            Some ""
          | Some "."
          | Some ".." ->
               collect dirs names
          | None ->
               dirs, names
          | Some name ->
               let filename = Filename.concat dir name in
               let dir_flag = is_dir filename in
               let dirs, names =
                  if (options.glob_dot || name.[0] <> '.')
                     && (not options.glob_dirs || dir_flag)
                     && Str.string_match pattern name 0
                     && not (options.glob_ignore name)
                     && (options.glob_allow name)
                     && not (options.glob_cvsignore name)
                  then
                     if dir_flag then
                        filename :: dirs, names
                     else
                        dirs, filename :: names
                  else
                     dirs, names
               in
                  collect dirs names
   in
   let dirs_names = collect dirs names in
      Unix.closedir dirx;
      dirs_names

let glob_dirs_pattern options dirs pattern =
   let rec collect dirs' names' dirs =
      match dirs with
         dir :: dirs ->
            let dirs', names' = glob_dir_pattern options dirs' names' dir pattern in
               collect dirs' names' dirs
       | [] ->
            dirs', names'
   in
      collect [] [] dirs

let glob_dirs_name options dirs name =
   if is_glob_name options name then
      let options =
         if name <> "" && name.[0] = '.' then
            { options with glob_dot = true }
         else
            options
      in
      let pattern = regexp_of_shell_pattern name in
         glob_dirs_pattern options dirs pattern
   else
      List.fold_left (fun (dirs, names) dir ->
            let filename = Filename.concat dir name in
               try
                  let stat = Unix.stat filename in
                     if stat.Unix.st_kind = Unix.S_DIR then
                        filename :: dirs, names
                     else
                        dirs, filename :: names
               with
                  Unix.Unix_error _ ->
                     dirs, names) ([], []) dirs

(*
 * Perform a glob expansion on a single path.
 *)
let glob_name options dir name =
   (* Split the path into components *)
   let dir, path =
      match filename_path name with
         RelativePath path ->
            dir, path
       | AbsolutePath (root, path) ->
            string_of_root root, path
   in

   (* Walk through the path *)
   let rec glob dirs path =
      match path with
         [] ->
            dirs, []
       | [name] ->
            glob_dirs_name options dirs name
       | name :: path ->
            let options = { options with glob_dirs = true } in
            let dirs, _ = glob_dirs_name options dirs name in
               glob dirs path
   in
      glob [dir] path

(*
 * Perform the actual glob.
 *)
let glob options dir names =
   let options = glob_options_of_list options in
   let names = glob_braces options names in
      List.fold_left (fun (dirs, names) name ->
            let dirs', names' = glob_name options dir name in
            let dirs = List.rev_append dirs' dirs in
            let names = List.rev_append names' names in
               dirs, names) ([], []) names

(************************************************************************
 * Directory listings.
 *)

(*
 * Get all the names in the directory.
 *)
let list_dir_exn options hidden_dirs dirs names dirname =
   let inx = Unix.opendir dirname in
   let rec read hidden_dirs dirs names =
      let name =
         try Some (Unix.readdir inx) with
            End_of_file ->
               None
      in
         match name with
            Some "."
          | Some ".." ->
               read hidden_dirs dirs names
          | None ->
               hidden_dirs, dirs, names
          | Some name ->
               let hidden_dirs, dirs, names =
                  let filename = Filename.concat dirname name in
                  let dir_flag = is_dir filename in
                     if (options.glob_dot || name.[0] <> '.')
                        && (dir_flag || not options.glob_dirs)
                        && not (options.glob_ignore name)
                        && not (options.glob_cvsignore name)
                     then
                        if dir_flag then
                           if options.glob_allow name then
                              hidden_dirs, filename :: dirs, names
                           else
                              filename :: hidden_dirs, dirs, names
                        else if options.glob_allow name then
                           hidden_dirs, dirs, filename :: names
                        else
                           hidden_dirs, dirs, names
                     else
                        hidden_dirs, dirs, names
               in
                  read hidden_dirs dirs names
   in
   let hidden_dirs_names = read hidden_dirs dirs names in
      Unix.closedir inx;
      hidden_dirs_names

let list_dir_aux options hidden_dirs dirs names dirname =
   let options =
      if options.glob_cvs then
         { options with glob_cvsignore = load_cvsignore dirname }
      else
         options
   in
      try list_dir_exn options hidden_dirs dirs names dirname with
         Unix.Unix_error _
       | Sys_error _
       | Failure _ ->
            hidden_dirs, dirs, names

(*
 * Perform a directory listing.
 *)
let list_dirs options dirs =
   let options = glob_options_of_list options in
   let rec collect dirs names l =
      match l with
         dir :: l ->
            let _, dirs, names = list_dir_aux options [] dirs names dir in
               collect dirs names l
       | [] ->
            dirs, names
   in
      collect [] [] dirs

(*
 * Recursive directory listing.
 *)
let list_dirs_rec options dirs =
   let options = glob_options_of_list options in
   let rec collect examined_dirs hidden_dirs unexamined_dirs names =
      match hidden_dirs, unexamined_dirs with
         dir :: hidden_dirs, _ ->
            let hidden_dirs, unexamined_dirs, names =
               list_dir_aux options hidden_dirs unexamined_dirs names dir
            in
               collect examined_dirs hidden_dirs unexamined_dirs names
       | [], dir :: unexamined_dirs ->
            let examined_dirs = dir :: examined_dirs in
            let hidden_dirs, unexamined_dirs, names =
               list_dir_aux options hidden_dirs unexamined_dirs names dir
            in
               collect examined_dirs hidden_dirs unexamined_dirs names
       | [], [] ->
            examined_dirs, names
   in
   let hidden_dirs, unexamined_dirs =
      List.fold_left (fun (hidden_dirs, unexamined_dirs) dir ->
            if options.glob_allow dir then
               hidden_dirs, dir :: unexamined_dirs
            else
               dir :: hidden_dirs, unexamined_dirs) ([], []) dirs
   in
      collect [] hidden_dirs unexamined_dirs []

(*
 * Recursively expand all subdirectories.
 *)
let subdirs_of_dirs options dirs =
   let options = glob_options_of_list options in
   let options = { options with glob_dirs = true } in
   let rec collect listing hidden_dirs dirs =
      match hidden_dirs, dirs with
         dir :: hidden_dirs, _ ->
            let hidden_dirs, dirs, _ = list_dir_aux options hidden_dirs dirs [] dir in
               collect listing hidden_dirs dirs
       | [], dir :: dirs ->
            let listing = dir :: listing in
            let hidden_dirs, dirs, _ = list_dir_aux options hidden_dirs dirs [] dir in
               collect listing hidden_dirs dirs
       | [], [] ->
            listing
   in
      collect [] [] dirs

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
