(*
 * Formatting to HTML documents.
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
open Lm_rformat_raw
open Lm_rformat

(*
 * We hack the indentation in the HTML printer.
 * Lm_format the data into lines, and print the tabstops in
 * the background color.
 *
 * The prefix is the white space that is inserted to
 * get the left margin right.
 *)
type html_buffer =
   { html_current_line   : (bool * string) Queue.t;
     mutable html_prefix : string;
     html_print_string   : string -> unit;
     html_print_newline  : unit -> unit
   }

(*
 * Have to escape special characters.
 *)
let html_escape_string buffer s =
   let len = String.length s in
   let rec collect i j =
      if j != len then
         match s.[j] with
            '<' ->
               collect_escape i j "&lt;"
          | '>' ->
               collect_escape i j "&gt;"
          | '&' ->
               collect_escape i j "&amp;"
          | _ ->
               collect i (succ j)
      else if i = 0 then
         Buffer.add_string buffer s
      else if i < j then
         Buffer.add_substring buffer s i (j - i)
   and collect_escape i j s' =
      if i < pred j then
         Buffer.add_substring buffer s i (j - i);
      Buffer.add_string buffer s';
      collect (succ j) (succ j)
   in
      collect 0 0

(*
 * Print strings.
 *)
let html_print_string buf s =
   Queue.add (true, s) buf.html_current_line

let html_print_invis buf s =
   Queue.add (false, s) buf.html_current_line

(*
 * Extract the entire line.
 *)
let html_line buf =
   let buffer = Buffer.create 100 in
      Queue.iter (fun (vis, h) ->
            if vis then
               html_escape_string buffer h
            else
               Buffer.add_string buffer h) buf.html_current_line;
      Buffer.contents buffer

let html_visible buf =
   let buffer = Buffer.create 100 in
      Queue.iter (fun (vis, h) ->
            if vis then
               Buffer.add_string buffer h) buf.html_current_line;
      Buffer.contents buffer

let html_push_line buf =
   let line = html_line buf in
      buf.html_print_string line;
      Queue.clear buf.html_current_line

let html_flush buf =
   html_push_line buf

(*
 * Set up all pending tabstops.
 *)
let html_tab_line buf =
   buf.html_prefix ^ html_visible buf

(*
 * Newline.
 * Compute all pending tabstops,
 * then push the line and the new tabstop.
 *)
let html_tab buf (col, _) _ =
   if col = 0 then
      begin
         html_push_line buf;
         buf.html_print_newline ();
         buf.html_print_string "<br>\n";
         buf.html_prefix <- ""
      end
   else
      let tabline = buf.html_prefix ^ html_visible buf in
         html_push_line buf;
         let prefix =
            if col >= String.length tabline then
               tabline
            else
               String.sub tabline 0 col
         in
         let spacer = Printf.sprintf "<span style=\"visibility:hidden\">%s</span>" prefix in
            buf.html_prefix <- prefix;
            buf.html_print_newline ();
            buf.html_print_string "<br>\n";
            buf.html_print_string spacer

let html_tag buf s =
   buf.html_print_string ("<" ^ s ^ ">")

let html_etag buf s =
   buf.html_print_string ("</" ^ s ^ ">")

(*
 * An HTML printer.
 *)
let make_html_printer_aux raw =
   let { raw_print_string  = output_string;
         raw_print_newline = output_newline
       } = raw
   in
   let print_string s =
      output_string s 0 (String.length s)
   in
   let buf =
      { html_current_line  = Queue.create ();
        html_prefix        = "";
        html_print_string  = print_string;
        html_print_newline = output_newline
      }
   in
   let info =
      { print_string    = html_print_string buf;
        print_invis     = html_print_invis buf;
        print_tab       = html_tab buf;
        print_begin_tag = html_tag buf;
        print_end_tag   = html_etag buf
      }
   in
      buf, info

let make_html_printer raw =
   snd (make_html_printer_aux raw)

let print_html_raw rmargin buf raw =
   let hbuf, info = make_html_printer_aux raw in
      print_to_printer buf rmargin info;
      html_flush hbuf;
      raw.raw_print_flush ()

let print_html_channel rmargin buf out =
   print_html_raw rmargin buf (raw_channel_printer out)

let print_html_buffer rmargin buf out =
   print_html_raw rmargin buf (raw_buffer_printer out)

let print_html_string rmargin buf  =
   let out = Buffer.create 100 in
      print_html_buffer rmargin buf out;
      Buffer.contents out

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
