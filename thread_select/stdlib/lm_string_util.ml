(*
 * String utilities.
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000 Jason Hickey, Caltech
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
 * Author: Jason Hickey <jyh@cs.caltech.edu>
 * Modified By: Aleksey Nogin <nogin@cs.cornell.edu>
 *)
open Lm_debug
open Lm_printf

(*
 * Show the file loading.
 *)
let debug_string =
   create_debug (**)
      { debug_name = "string";
        debug_description = "check string bounds";
        debug_value = false
      }

(*
 * A scratch buffer used by several functions in this module.
 *)
let code0 = Char.code '0'
let codea = Char.code 'a'
let codeA = Char.code 'A'

(*
 * Check all chars in the string.
 *)
let for_all f s =
   let len = String.length s in
   let rec check i =
      (i = len) or (f s.[i] & check (succ i))
   in
      check 0

(*
 * Find a char in a string.
 *)
let strchr s c =
   let l = String.length s in
   let rec aux i =
      if i < l then
         if s.[i] = c then
            i
         else
            aux (succ i)
      else
         raise Not_found
   in
      aux 0

(*
 * A more efficient reimplementation of String.contains.
 *)
let contains =
   let rec contains_aux s limit c i =
      (i < limit) && ((String.unsafe_get s i) = c || contains_aux s limit c (i+1))
   in
      fun s c ->
         contains_aux s (String.length s) c 0

(*
 * contains_string s1 s2
 * true iff any one of the characters in s2 appears in s1.
 *)
let contains_any =
   let rec search2 s2 len2 c i =
      (i < len2) && ((String.unsafe_get s2 i) = c || search2 s2 len2 c (i + 1))
   and search1 s1 len1 s2 len2 i =
      (i < len1) && (search2 s2 len2 (String.unsafe_get s1 i) 0 || search1 s1 len1 s2 len2 (i + 1))
   in
      (fun s1 s2 ->
            search1 s1 (String.length s1) s2 (String.length s2) 0)

(*
 * Index of first char in a set.
 *)
let index_set s set =
   let len = String.length s in
   let rec loop i =
      if i = len then
         raise Not_found
      else
         let c = s.[i] in
            if String.contains set c then
               i
            else
               loop (succ i)
   in
      loop 0

let rindex_set s set =
   let rec loop i =
      if i < 0 then
         raise Not_found
      else
         let c = s.[i] in
            if String.contains set c then
               i
            else
               loop (i - 1)
   in
      loop (String.length s - 1)

(*
 * Search for a pattern in the indicated buffer, within the start
 * and length constraints applied to the buffer.  Note that this
 * uses a very inefficient algorithm; at some point I (JDS) will
 * get around to converting this to the Knuth-Morris-Pratt or
 * maybe Rabin-Karp algorithm.
 *
 * On success, this returns the offset (RELATIVE TO start!) of
 * the first match found; on failure, this raises Not_found.
 *)
let strpat buffer start len pattern =
   let patlen = String.length pattern in
   let rec pattern_matches_prefix bufcur patcur =
      if patcur >= patlen then
         true
      else if buffer.[bufcur] <> pattern.[patcur] then
         false
      else
         pattern_matches_prefix (bufcur + 1) (patcur + 1)
   in
   let pattern_matches_prefix start = pattern_matches_prefix start 0 in
   let rec is_match start =
      if start + patlen > len then
         raise Not_found
      else if pattern_matches_prefix start then
         start
      else
         is_match (start + 1)
   in
      (is_match start) - start

(*
 * Escape a string using the C conventions.
 *)
let c_escaped s =
   let len = String.length s in
   let buf = Buffer.create len in
   let rec loop i =
      if i = len then
         Buffer.contents buf
      else
         let c = s.[i] in
         let _ =
            match c with
               '"' ->
                  Buffer.add_string buf "\\\""
             | ' '..'~' ->
                  Buffer.add_char buf c
             | _ ->
                  let code = Char.code c in
                     Buffer.add_char buf '\\';
                     Buffer.add_char buf (Char.chr (((code / 64) mod 8) + code0));
                     Buffer.add_char buf (Char.chr (((code / 8) mod 8) + code0));
                     Buffer.add_char buf (Char.chr ((code mod 8) + code0))
         in
            loop (succ i)
   in
      loop 0

(*
 * Escape a string using the Javascript single-quote conventions.
 *)
let js_escaped s =
   let len = String.length s in
   let buf = Buffer.create len in
   let rec loop i =
      if i = len then
         Buffer.contents buf
      else
         let c = s.[i] in
         let _ =
            match c with
               '\'' ->
                  Buffer.add_string buf "\\'"
             | ' '..'~' ->
                  Buffer.add_char buf c
             | _ ->
                  let code = Char.code c in
                     Buffer.add_char buf '\\';
                     Buffer.add_char buf (Char.chr (((code / 64) mod 8) + code0));
                     Buffer.add_char buf (Char.chr (((code / 8) mod 8) + code0));
                     Buffer.add_char buf (Char.chr ((code mod 8) + code0))
         in
            loop (succ i)
   in
      loop 0

(*
 * A generic definition of white space.
 *)
let white = " \t\r\n\012"
let quotes = "\"'"

(*
 * Split a string str into a list of substrings.
 * The string is split on any character in delims.  Empty substrings
 * are returned as empty strings in the list.  For example:
 *   split "-." "foo.bar--ba??z"
 * returns
 *   ["foo"; "bar"; ""; "ba??z"]
 *)
let split delims str =
   let strlen = String.length str in

   (* Find the next split index *)
   let rec next_split pos =
      if pos = strlen then
         strlen
      else
         let c = String.get str pos in
            if contains delims c then
               pos
            else
               next_split (pos + 1)
   in

   (* Build the list *)
   let rec str_split pos =
      let pos_end = next_split pos in
         if pos_end = strlen then
            [String.sub str pos (pos_end - pos)]
         else
            (String.sub str pos (pos_end - pos)) :: (str_split (pos_end + 1))
   in
      str_split 0

(*
 * Split a string str into a list of substrings.
 * The string is split on any character in delims.  Quotations
 * are not split.
 *
 * Empty substrings are _not_ returned as empty strings in the list.
 * For example:
 *   split ".-" "foo.bar--ba??z"
 * returns
 *   ["foo"; "bar"; "ba??z"]
 *)
let tokens_fold f x quotes delims str =
   let strlen = String.length str in

   (* Skip white space *)
   let rec skip_split pos =
      if pos = strlen then
         strlen
      else
         let c = str.[pos] in
            if contains delims c then
               skip_split (succ pos)
            else
               pos
   in

   (*
    * Find the next split index.
    *)
   let rec next_split pos =
      if pos = strlen then
         strlen
      else
         let c = str.[pos] in
            if contains delims c then
               pos
            else if contains quotes c then
               next_quote (succ pos)
            else
               next_split (succ pos)

   and next_quote pos =
      if pos = strlen then
         strlen
      else
         let c = str.[pos] in
            if contains quotes c then
               next_split (succ pos)
            else if c = '\\' && pos < pred strlen then
               next_quote (pos + 2)
            else
               next_quote (succ pos)
   in

   (* Build the list *)
   let rec str_split x pos =
      if pos = strlen then
         x
      else
         let pos_end = next_split pos in
         let x = f x str pos (pos_end - pos) in
            str_split x (skip_split pos_end)
   in
      str_split x (skip_split 0)

(*
 * Default token processor.
 *)
let tokens quotes delims str =
   let l =
      tokens_fold (fun l s off len ->
            String.sub s off len :: l) [] quotes delims str
   in
      List.rev l

let tokens_std = tokens quotes white

(*
 * Split a string based on a bounary.
 *)
let split_string boundary s =
   let len_s = String.length s in
   let len_b = String.length boundary in
   let c =
      if len_b = 0 then
         raise (Invalid_argument "split_string");
      boundary.[0]
   in
   let rec matches i j =
      if j = len_b then
         true
      else
         s.[i] = boundary.[j] && matches (succ i) (succ j)
   in
   let buf = Buffer.create 17 in
   let rec split l i =
      if len_s - i < len_b then
         begin
            Buffer.add_substring buf s i (len_s - i);
            Buffer.contents buf :: l
         end
      else if s.[i] = c && matches i 0 then
         let s' = Buffer.contents buf in
            Buffer.clear buf;
            split (s' :: l) (i + len_b)
      else
         begin
            Buffer.add_char buf s.[i];
            split l (succ i)
         end
   in
      List.rev (split [] 0)

(*
 * Split a string based on a MIME boundary.
 *)
let split_mime_string boundary s =
   let len_s = String.length s in
   let len_b = String.length boundary in
   let rec matches i j =
      if j = len_b then
         true
      else
         s.[i] = boundary.[j] && matches (succ i) (succ j)
   in
   let buf = Buffer.create 17 in

   (* Collect the delimited text *)
   let rec split l i =
      if len_s - i < len_b - 2 then
         l
      else if s.[i] = '-' && s.[i + 1] = '-' && matches (i + 2) 0 then
         let l = Buffer.contents buf :: l in
            Buffer.clear buf;
            skip l (i + 2 + len_b)
      else
         begin
            Buffer.add_char buf s.[i];
            split l (succ i)
         end

   (* Skip over garbage after the delimiter *)
   and skip l i =
      if len_s - i < 2 || (s.[i] = '-' && s.[i + 1] = '-') then
         l
      else
         split l (i + 2)

   (* Skip to the first delimiter *)
   and skip_start i =
      if len_s - i < len_b - 2 then
         []
      else if s.[i] = '-' && s.[i + 1] = '-' && matches (i + 2) 0 then
         skip [] (i + 2 + len_b)
      else
         skip_start (succ i)
   in
      List.rev (skip_start 0)

(*
 * Unescape a quoted string.
 *)
let unescape s =
   let slen = String.length s in
   let buf = Buffer.create slen in
   let off, len =
      if slen < 2 then
         0, slen
      else if s.[0] = '"' && s.[slen - 1] = '"' then
         1, slen - 1
      else
         0, slen
   in
   let rec collect i =
      if i = len then
         Buffer.contents buf
      else
         let c = s.[i] in
         let c, i =
            if c = '\\' && i + 1 < len then
               match s.[i + 1] with
                  't' -> '\t', i + 2
                | 'r' -> '\r', i + 2
                | 'n' -> '\n', i + 2
                | '\\' -> '\\', i + 2
                | ('0'..'9') when i + 3 < len ->
                     let code =
                        100 * Char.code s.[i + 1]
                        + 10 * Char.code s.[i + 2]
                        + Char.code s.[i + 3]
                        - 111 * Char.code '0'
                     in
                        Char.chr (code land 0xff), i + 4
                | c ->
                     c, i + 2
            else
               c, i + 1
         in
            Buffer.add_char buf c;
            collect i
   in
      collect off

(*
 * Trim all whitespace from a string, respecting quotes.
 *)
let trim_all quotes delims str =
   let scratch_buf = Buffer.create 17 in
      ignore (tokens_fold (fun first s off len ->
                    if not first then
                       Buffer.add_char scratch_buf ' ';
                    Buffer.add_substring scratch_buf s off len;
                    false) true quotes delims str);
      Buffer.contents scratch_buf

let trim_std = trim_all quotes white

(*
 * Trim outer whitespace from a string.
 *)
let trim s =
   let length = String.length s in
   let is_whitespace = String.contains white in
   let rec scan_for_first_nonws index =
      if index < length && is_whitespace s.[index] then
         scan_for_first_nonws (index + 1)
      else
         index
   in
   let rec scan_for_last_nonws index =
      if index >= 0 && is_whitespace s.[index] then
         scan_for_last_nonws (index - 1)
      else
         index
   in
   let first = scan_for_first_nonws 0 in
   let last  = scan_for_last_nonws (length - 1) in
      if first > last then
         ""
      else
         String.sub s first (last - first + 1)

(*
 * Need these for converting numbers.
 *)
let code0 = Char.code '0'
let codea = Char.code 'a'
let codeA = Char.code 'A'

(*
 * Turn a string into an argument list.
 *)
let parse_args_list line =
   let len = String.length line in
   let buf = String.create len in
   let rec skip i =
      if i = len then
         [[]]
      else
         match line.[i] with
            ' ' | '\t' | '\n' | '\r' ->
               skip (succ i)
          | '"' ->
               string 0 (succ i)
          | '\\' ->
               if len >= i+2 && line.[i+1]='\\'
               then [] :: skip (i+2)
               else raise(Invalid_argument ("Lm_string_util.parse_args: " ^ line))
          | _ ->
               collect i (succ i)
   and collect i j =
      if j = len then
         [[String.sub line i (j - i)]]
      else
         match line.[j] with
            ' ' | '\t' | '\n' | '\r' | '\\' ->
               let s = String.sub line i (j - i) in
               begin match skip j with
                  [] -> [[s]]
                | h::tl -> (s::h) :: tl
               end
          | _ ->
               collect i (succ j)
   and string j k =
      if k = len then
         raise (Invalid_argument ("Lm_string_util.parse_args: " ^ line))
         (* [String.sub buf 0 j] *)
      else
         let c = line.[k] in
            if c = '"' then
               let s = String.sub buf 0 j in
               match skip (succ k) with
                  [] -> raise (Invalid_argument "Lm_string_util.parse_args - internal error")
                | h::tl -> (s::h)::tl
            else if c = '\\' then
               escape j (succ k)
            else
               begin
                  buf.[j] <- c;
                  string (succ j) (succ k)
               end
   and escape j k =
      if k = len then
         raise (Invalid_argument ("Lm_string_util.parse_args: " ^ line))
         (* [String.sub buf 0 j] *)
      else
         let c,k =
            match line.[k] with
               't' -> '\t', succ k
             | 'n' -> '\n', succ k
             | 'r' -> '\r', succ k
             | '\\' -> '\\', succ k
             | ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') as c ->
                  Char.chr (100 * (Char.code c) +
                            10 * (Char.code line.[succ k]) +
                            (Char.code line.[k+2]) - 111 * code0),
                  k+3
             | c -> c, succ k
         in
            buf.[j] <- c;
            string (succ j) k
   in
   let _ =
      if !debug_string then
         eprintf "Lm_string_util.parse_args: %s@." (String.escaped line)
   in
   let args = skip 0 in
      if !debug_string then
         eprintf "Lm_string_util.parse_args: done@.";
      args

let parse_args s =
   match parse_args_list s with
      [] -> []
    | [l] -> l
    | _ -> raise (Invalid_argument ("Lm_string_util.parse_args - line includes \\\\:" ^ s))

(*
 * Concatenate strings.
 *)
let prepend sep sl =
   let scratch_buf = Buffer.create 17 in
   let collect s =
      Buffer.add_string scratch_buf sep;
      Buffer.add_string scratch_buf s
   in
      Buffer.clear scratch_buf;
      if sl = [] then
         Buffer.add_string scratch_buf sep
      else
         List.iter collect sl;
      Buffer.contents scratch_buf

(*
 * Read a file into a string.
 *)
let string_of_file name =
   let inx = open_in_bin name in
   let scratch_buf = Buffer.create 17 in
   let rec loop () =
      Buffer.add_char scratch_buf (input_char inx);
      loop ()
   in
      Buffer.clear scratch_buf;
      try loop () with
         End_of_file ->
            close_in inx;
            let s = Buffer.contents scratch_buf in
               Buffer.reset scratch_buf;
               s

(************************************************************************
 * DEBUG VERSIONS
 ************************************************************************)

(*
 * Create a new string containing garbage.
 *)
let create name i =
   if !debug_string then
      if i < 0  then
         begin
            eprintf "Lm_string_util.create: %s: %d < 0@." name i;
            raise (Failure "Lm_string_util.create")
         end;
   String.create i

(*
 * Make a string initialized with all chars the same.
 *)
let make name i c =
   if !debug_string then
      if i < 0 then
         begin
            eprintf "Lm_string_util.make: %s: %d < 0@." name i;
            raise (Failure "Lm_string_util.make")
         end;
   String.make i c

(*
 * Substring.
 *)
let sub name s i len =
   if !debug_string then
      let len' = String.length s in
         if i >= 0 & len >= 0 & i + len < len' then
            String.sub s i len
         else
            begin
               eprintf "Lm_string_util.sub error: %s: %s.[%d]@." name s i;
               raise (Failure "Lm_string_util.sub")
            end
   else
      String.sub s i len

let blit name froms i tos j len =
   if !debug_string then
      let from_len = String.length froms in
      let to_len = String.length tos in
         if i >= 0 & j >= 0 & len >= 0 & i + len < from_len & j + len < to_len then
            String.blit froms i tos j len
         else
            begin
               eprintf "String_util.blit_error: %s: %s %d %s %d %d@." name froms i tos j len;
               raise (Failure "String_util.blit")
            end
   else
      String.blit froms i tos j len

let set name s i c =
   if !debug_string then
      let len = String.length s in
         if i >= 0 & i < len then
            String.set s i c
         else
            begin
               eprintf "String_util.set error: %s: %s.[%d] <- %c@." name s i c;
               raise (Failure "String_util.set")
            end
   else
      String.set s i c

let get name s i =
   let len = String.length s in
      if i >= 0 & i < len then
         String.get s i
      else
         begin
            eprintf "String_util.get error: %s: %s[%d]@." name s i;
            raise (Failure "String_util.get")
         end

(************************************************************************
 * Hex notation.
 *)

(*
 * Turn a string into hex.
 *)
let hex_char =
   let zero = Char.code '0' in
   let a = Char.code 'a' - 10 in
   let hex_char code =
      if code < 10 then
         Char.chr (code + zero)
      else
         Char.chr (code + a)
   in
      hex_char

let hexify s =
   let len = String.length s in
   let buf = String.create (2 * len) in
      for i = 0 to pred len do
         let code = Char.code s.[i] in
            buf.[2 * i] <- hex_char ((code lsr 4) land 15);
            buf.[2 * i + 1] <- hex_char (code land 15)
      done;
      buf

let hexify_sub s off len =
   let buf = String.create (2 * len) in
      for i = 0 to pred len do
         let code = Char.code s.[off + i] in
            buf.[2 * i] <- hex_char ((code lsr 4) land 15);
            buf.[2 * i + 1] <- hex_char (code land 15)
      done;
      buf

let unhex i =
   match i with
      '0' .. '9' ->
         (Char.code i) - code0
    | 'a' .. 'f' ->
         (Char.code i) - codea + 10
    | 'A' .. 'F' ->
         (Char.code i) - codeA + 10
    | _ ->
         raise (Failure "unhexify")

let unhexify s =
   let len = String.length s in
      if len mod 2 = 0 then
         let buf = create "String_util.unhexify" (len / 2) in
         let rec unhexify i j =
            if j < len then
               begin
                  buf.[i] <- Char.chr ((unhex s.[j]) * 16 + (unhex s.[succ j]));
                  unhexify (i + 1) (j + 2)
               end
         in
            unhexify 0 0;
            buf
      else
         raise (Failure "unhexify")

let unhexify_int s =
   let len = String.length s in
   let rec unhexify index i =
      if i < len then
         unhexify (index * 16 + (unhex s.[i])) (succ i)
      else
         index
   in
      unhexify 0 0

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
