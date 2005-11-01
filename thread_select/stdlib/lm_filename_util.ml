(*
 * Utilities on filenames.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Jason Hickey, Caltech
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

type pathname = string list

(*
 * Tests for whether a file is executable.
 *)
let euid =
   try Unix.geteuid () with
      Unix.Unix_error _ ->
         0

let groups =
   try Array.to_list (Unix.getgroups ()) with
      Unix.Unix_error _ ->
         []

let unix_is_executable s =
   let flag =
      try
         let { Unix.st_kind = kind;
               Unix.st_perm = perm;
               Unix.st_uid = uid;
               Unix.st_gid = gid
             } = Unix.stat s
         in
            (kind = Unix.S_REG)
            && ((perm land 0o001) <> 0
                || (List.mem gid groups && (perm land 0o010) <> 0)
                || (uid = euid && (perm land 0o100) <> 0))
      with
         Unix.Unix_error _ ->
            false
   in
      if flag then
         Some s
      else
         None

(*
 * On Windows, the file does not have the be executable,
 * it just has to exist.
 *)
let win32_suffixes =
   [""; ".exe"; ".com"; ".bat"]

let win32_is_executable =
   let rec search_win32 suffixes name =
      match suffixes with
         suffix :: suffixes ->
            let name' = name ^ suffix in
               if Sys.file_exists name' then
                  Some name'
               else
                  search_win32 suffixes name
       | [] ->
            None
   in
      search_win32 win32_suffixes

(*
 * System-dependent config.
 * On win32, use lowercase names, and watch for drive letters.
 *)
let has_drive_letters,
    normalize_string,
    normalize_path,
    separator_char,
    search_separator_char,
    is_executable =
   match Sys.os_type with
      "Win32" ->
         true, String.lowercase, List.map String.lowercase, '\\', ';', win32_is_executable
    | "Cygwin" ->
         false, String.lowercase, List.map String.lowercase, '/', ':', unix_is_executable
    | "Unix" ->
         false, (fun s -> s), (fun s -> s), '/', ':', unix_is_executable
    | s ->
         raise (Invalid_argument ("Omake_node: unknown system type " ^ s))

let separator_string = String.make 1 separator_char
let search_separator_string = String.make 1 search_separator_char

(*
 * Utilities for splitting paths.
 *)
type root =
   NullRoot
 | DriveRoot of char

type 'a path =
   RelativePath of 'a
 | AbsolutePath of root * 'a

let null_root = NullRoot

(*
 * Windows sucks.  Here are some utilities to
 * analyze drve letters.
 *)
let is_drive_letter c =
   match c with
      'a'..'z'
    | 'A'..'Z' ->
         true
    | _ ->
         false

let drive_skip name =
   let len = String.length name in
      if has_drive_letters && len >= 2 && is_drive_letter name.[0] && name.[1] = ':' then
         2
      else
         0

(*
 * Print the drive letter.
 *)
let string_of_root = function
   NullRoot ->
      separator_string
 | DriveRoot c ->
      let s = String.make 3 c in
         s.[1] <- ':';
         s.[2] <- separator_char;
         s

(*
 * Split the path into root part, and the rest.
 *)
let filename_string name =
   let len = String.length name in
      if has_drive_letters && len >= 3 && is_drive_letter name.[0] && name.[1] = ':' && (name.[2] = '/' || name.[2] = '\\') then
         let root = DriveRoot (Char.lowercase name.[0]) in
         let path = String.sub name 3 (len - 3) in
            AbsolutePath (root, path)
      else if len >= 1 && (name.[0] = '/' || name.[0] = '\\') then
         let root = NullRoot in
         let path = String.sub name 1 (len - 1) in
            AbsolutePath (root, path)
      else
         RelativePath name

(*
 * Split the rest into parts.
 *)
let filename_path name =
   match filename_string name with
      AbsolutePath (root, path) ->
         AbsolutePath (root, Lm_string_util.split "\\/" path)
    | RelativePath path ->
         RelativePath (Lm_string_util.split "\\/" path)

(*
 * Split a filename into root/suffix.
 *)
let split name =
   try
      let index = String.rindex name '.' in
      let len = String.length name in
      let root = String.sub name 0 index in
      let suffix = String.sub name index (len - index) in
         root, suffix
   with
      Not_found ->
         name, ""

(*
 * Separate this for efficiency.
 *)
let root name =
   try
      let index = String.rindex name '.' in
         String.sub name 0 index
   with
      Not_found ->
         name

let strip_suffixes name =
   let start =
      try String.rindex name '/' with
         Not_found ->
            try String.rindex name '\\' with
               Not_found ->
                  0
   in
      try
         let index = String.rindex_from name start '.' in
            String.sub name 0 index
      with
         Not_found ->
            name

(*
 * Pathname separator chars.
 *)
let separators = "/\\"

(*
 * Split a pathname.
 *)
let split_path = Lm_string_util.split separators

(*
 * Put it back together.
 *)
let concat_path = String.concat separator_string

(*
 * Simplify, remove leading directory.
 *)
let simplify_path path =
   let rec simplify path' = function
      dir::tl ->
         if dir = "" or dir = "." then
            simplify path' tl
         else if dir = ".." then
            match path' with
               [] ->
                  simplify path' tl
             | _::path'' ->
                  simplify path'' tl
         else
            simplify (dir :: path') tl
    | [] ->
         List.rev path'
   in
      simplify [] path

(*
 * Path searching.
 *)
let search_table = Hashtbl.create 19

(*
 * Get the system path.
 *)
let search_path =
   let path =
      try Sys.getenv "PATH" with
         Not_found ->
            "."
   in
      Lm_string_util.split search_separator_string path

(*
 * Search for the file in the path.
 * Win32 files do not need to be executable.
 *)
let search_command name =
   let rec search dirs name =
      match dirs with
         dir :: dirs ->
            let pathname = Filename.concat dir name in
               (match is_executable pathname with
                   Some pathname ->
                      pathname
                 | None ->
                      search dirs name)
       | [] ->
            raise Not_found
   in
      search search_path name

(*
 * Figure out where in the path the commands comes from.
 * The filename must be simple, no path separators.
 *)
let which name =
   (* Check for a simple filename *)
   if Lm_string_util.contains_any name separators then
      raise (Failure ("Lm_filename_util.which: path filenames are not allowed: " ^ name));

   (* Check the hashtable *)
   try Hashtbl.find search_table name with
      Not_found ->
         let fullname = search_command name in
            Hashtbl.add search_table name fullname;
            fullname

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
