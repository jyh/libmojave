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
 * System-dependent config.
 * On win32, use lowercase names, and watch for drive letters.
 *)
let has_drive_letters, normalize_string, normalize_path, separator_char =
   match Sys.os_type with
      "Win32" ->
         true, String.lowercase, List.map String.lowercase, '\\'
    | "Cygwin" ->
         false, String.lowercase, List.map String.lowercase, '/'
    | "Unix" ->
         false, (fun s -> s), (fun s -> s), '/'
    | s ->
         raise (Invalid_argument ("Omake_node: unknown system type " ^ s))

let separator_string = String.make 1 separator_char

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

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
