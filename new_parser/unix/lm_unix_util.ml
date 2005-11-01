(*
 * Extra unix utilities.
 *
 * ----------------------------------------------------------------
 *
 * This file is part of MetaPRL, a modular, higher order
 * logical framework that provides a logical programming
 * environment for OCaml and other languages.
 *
 * See the file doc/index.html for information on Nuprl,
 * OCaml, and more information about this system.
 *
 * Copyright (C) 1998 Jason Hickey, Cornell University
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
 * jyh@cs.cornell.edu
 *)

(*
 * Copy a file.
 *)
let rec complete_write fd buf off len =
   let count = Unix.write fd buf off len in
      if count < len then
         complete_write fd buf (off + count) (len - count)

let rec copy_file_fd buffer from_fd to_fd =
   let count = Unix.read from_fd buffer 0 (String.length buffer) in
      if count > 0 then
         begin
            complete_write to_fd buffer 0 count;
            copy_file_fd buffer from_fd to_fd
         end

let copy_file from_name to_name =
   let from_fd = Unix.openfile from_name [Unix.O_RDONLY] 438 in
      try
         let to_fd = Unix.openfile to_name [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 438 in
            try
               copy_file_fd (String.create 8192) from_fd to_fd;
               Unix.close from_fd;
               Unix.close to_fd
            with
               x ->
                  Unix.close to_fd;
                  raise x
      with
         x ->
            Unix.close from_fd;
            raise x

(*
 * Make a directory hierarchy.
 *)
let mkdirhier name =
   let rec mkdir head path =
      match path with
         dir :: rest ->
            let filename = Filename.concat head dir in

            (* If it is already a directory, keep it *)
            let is_dir =
               try (Unix.stat filename).Unix.st_kind = Unix.S_DIR with
                  Unix.Unix_error _ ->
                     false
            in
               if not is_dir then
                  Unix.mkdir filename 0o777;
               mkdir filename rest
       | [] ->
            ()
   in
   let head =
      if String.length name = 0 || name.[0] <> '/' then
         "."
      else
         "/"
   in
   let path = Lm_filename_util.split_path name in
   let path = Lm_filename_util.simplify_path path in
      mkdir head path

(*
 * -*-
 * Local Variables:
 * Caml-master: "refiner"
 * End:
 * -*-
 *)
