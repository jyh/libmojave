(*
 * Simple value database.  The entries in the database have
 * the following format.
 *
 *     - Field label (int)
 *     - Hostname (string)
 *     - Magic number (16 bytes)
 *     - Digest (used on the source file, for up-to-date info)
 *     - Value (marshaled)
 *
 * Invariant:
 *     - There is at most one entry for each host/field label.
 *
 *       If the magic number doesn't match, then the entry is
 *       out-of-date, and should be replaced.
 *
 * In some cases, the hostname doesn't matter.  Even so, if there
 * is an entry with the current hostname, and the magic number
 * doesn't match, it is out-of-date.
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
type t = Unix.file_descr

type tag = int
type magic = string
type digest = string

(*
 * Some kinds of entries are host-independent.
 *)
type host =
   HostIndependent
 | HostDependent

(*
 * Codes during unmarshaling.
 *)
type 'a unmarshal =
   UnmarshalValue of 'a
 | UnmarshalRemove of int
 | UnmarshalNext

(*
 * Codes during removal.
 *)
type remove =
   RemoveEntry of int
 | RemoveNext
 | RemoveAll

(*
 * Marshaling.
 *)
let hostname = Unix.gethostname ()
let digest_length = 16

(*
 * File operations.
 *)

(*
 * Win32 doesn't have a general truncate, so seek and truncate.
 *)
let seek_and_truncate fd pos =
   let _ = Unix.lseek fd pos Unix.SEEK_SET in
      Lm_unix_util.ftruncate fd

(*
 *
 * When an entry is removed, copy the remaining parts of
 * the file.
 *)
let bufsize = 4096

let file_shift fd pos1 pos2 =
   let buf = String.create bufsize in
   let rec copy pos1 pos2 =
      let _ = Unix.lseek fd pos2 Unix.SEEK_SET in
      let amount = Unix.read fd buf 0 bufsize in
         if amount <> 0 then
            let _ = Unix.lseek fd pos1 Unix.SEEK_SET in
               assert (Unix.write fd buf 0 amount = amount);
               copy (pos1 + amount) (pos2 + amount)
         else
            pos1
   in
   let pos1 = copy pos1 pos2 in
      seek_and_truncate fd pos1

(*
 * If some kind of error happens while removing an entry,
 * truncate the file at this point.
 *)
let remove_entry fd pos off =
   try file_shift fd pos off with
      Unix.Unix_error _ ->
         seek_and_truncate fd pos

(*
 * Unmarshaling.
 *)
let unmarshal_tag inx =
   input_binary_int inx

let unmarshal_digest inx =
   let s = String.create digest_length in
      really_input inx s 0 digest_length;
      s

let unmarshal_string inx =
   let len = input_binary_int inx in
      if len < 0 || len >= 1024 then
         raise (Failure "unmarshal_string")
      else
         let s = String.create len in
            really_input inx s 0 len;
            s

(*
 * Search for the appropriate entry.
 *)
let find fd tag host_mode magic digest =
   let _ = Unix.lseek fd 0 Unix.SEEK_SET in
   let inx = Unix.in_channel_of_descr fd in
   let head = String.create Marshal.header_size in

   (* Find the appropriate entry *)
   let unmarshal_entry () =
      (* Get the header *)
      let tag' = unmarshal_tag inx in
      let host' = unmarshal_string inx in
      let magic' = unmarshal_string inx in
      let digest' = unmarshal_digest inx in
         if tag' = tag && magic' = magic && digest' = digest && (host_mode = HostIndependent || host' = hostname) then
            (* Found a matching entry *)
            UnmarshalValue (Marshal.from_channel inx)
         else
            (* Skip over this entry *)
            let () = really_input inx head 0 Marshal.header_size in
            let size = Marshal.data_size head 0 in
            let pos = pos_in inx + size in
               if host' = hostname then
                  UnmarshalRemove pos
               else begin
                  seek_in inx pos;
                  UnmarshalNext
               end
   in

   (*
    * Search through the entries.  If an exception is raised,
    * truncate the file at the start of the entry.
    *)
   let rec search () =
      let start = pos_in inx in
      let code =
         try unmarshal_entry () with
            End_of_file
          | Failure _
          | Sys_error _
          | Invalid_argument _ ->
               seek_and_truncate fd start;
               raise Not_found
      in
         match code with
            UnmarshalValue x ->
               x
          | UnmarshalRemove size ->
               remove_entry fd start size;
               raise Not_found
          | UnmarshalNext ->
               search ()
   in
      search ()

(*
 * Remove an entry.  Search through the existing entries.
 *)
let remove fd tag magic =
   let _ = Unix.lseek fd 0 Unix.SEEK_SET in
   let inx = Unix.in_channel_of_descr fd in
   let head = String.create Marshal.header_size in

   (* Find the appropriate entry *)
   let unmarshal_entry () =
      (* Get the header *)
      let tag' = unmarshal_tag inx in
      let host' = unmarshal_string inx in
      let magic' = unmarshal_string inx in
      let digest' = unmarshal_digest inx in
      let () = really_input inx head 0 Marshal.header_size in
      let size = Marshal.data_size head 0 in
      let pos = pos_in inx + size in
         if tag' = tag && magic' = magic || host' = hostname then
            RemoveEntry pos
         else begin
            seek_in inx pos;
            RemoveNext
         end
   in

   (*
    * Search through the entries.  If an exception is raised,
    * truncate the file at the start of the entry.
    *)
   let rec search () =
      let start = pos_in inx in
      let code =
         try unmarshal_entry () with
            End_of_file
          | Failure _
          | Sys_error _
          | Invalid_argument _ ->
               RemoveAll
      in
         match code with
            RemoveEntry pos ->
               remove_entry fd start pos
          | RemoveNext ->
               search ()
          | RemoveAll ->
               seek_and_truncate fd start
   in
      search ()

(*
 * Add an entry.
 * Remove any existing entry, and add the new one to the end of the
 * file.
 *)
let marshal_tag outx tag =
   output_binary_int outx tag

let marshal_digest outx digest =
   assert (String.length digest = digest_length);
   Pervasives.output_string outx digest

let marshal_string outx s =
   let len = String.length s in
      output_binary_int outx len;
      Pervasives.output_string outx s

let marshal_entry fd tag magic_number digest x =
   let outx = Unix.out_channel_of_descr fd in
      marshal_tag outx tag;
      marshal_string outx hostname;
      marshal_string outx magic_number;
      marshal_digest outx digest;
      Marshal.to_channel outx x [];
      Pervasives.flush outx

let add fd tag magic digest x =
   remove fd tag magic;
   marshal_entry fd tag magic digest x

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
