(*
 * Convert to standard format.
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
 * Author: Jason Hickey
 * jyh@cs.caltech.edu
 *)

open Lm_rformat
open Lm_make_printf

(************************************************************************
 * TYPES                                                                *
 ************************************************************************)

(*
 * Multiple formatted output.
 *)
type formatter =
   { mutable form_buffer : unit buffer;
     mutable form_depth : bool list;
     mutable form_out_string : (string -> int -> int -> unit);
     mutable form_out_flush : (unit -> unit);
     mutable form_out_newline : (unit -> unit);
     mutable form_out_space : (int -> unit);
     mutable form_max_boxes : int;
     mutable form_max_indent : int;
     mutable form_ellipsis : string;
     mutable form_margin : int
   }

(************************************************************************
 * STANDARD OPERATIONS                                                  *
 ************************************************************************)

let default_max_boxes = max_int
let default_max_indent = max_int
let default_ellipsis = "..."
let default_margin = 80

let output_substring = output

(*
 * Flush the buffer.
 *)
let flush_form form =
   let { form_buffer = buf;
         form_depth = depth;
         form_out_flush = flush';
         form_out_string = out_string;
         form_out_newline = out_newline;
         form_out_space = out_space;
         form_margin = margin
       } = form
   in
   let print_string s =
      out_string s 0 (String.length s)
   in
   let print_tab i =
      out_newline ();
      out_space i
   in
   let printer =
      { print_string = print_string;
        print_invis = print_string;
        print_tab = print_tab;
        print_begin_block = (fun _ _ -> ());
        print_end_block = (fun _ _ -> ());
        print_begin_tag = (fun _ _ -> ());
        print_end_tag = (fun _ _ -> ())
      }
   in
   let rec close = function
      h :: t ->
         if h then
            format_popm buf;
         format_ezone buf;
         close t
    | [] ->
         ()
   in
      close depth;
      print_to_printer margin buf printer;
      form.form_buffer <- new_buffer ();
      form.form_depth <- []

(*
 * Generic operations.
 *)
let pp_open_hbox form () =
   format_lzone form.form_buffer;
   form.form_depth <- false :: form.form_depth

let pp_open_vbox form tab =
   format_hzone form.form_buffer;
   format_pushm form.form_buffer tab;
   form.form_depth <- true :: form.form_depth

let pp_open_hvbox form tab =
   format_hzone form.form_buffer;
   format_pushm form.form_buffer tab;
   form.form_depth <- true :: form.form_depth

let pp_open_hovbox form tab =
   format_hzone form.form_buffer;
   format_pushm form.form_buffer tab;
   form.form_depth <- true :: form.form_depth

let pp_open_box = pp_open_hovbox

let pp_close_box form () =
   match form.form_depth with
      h :: t ->
         if h then
            format_popm form.form_buffer;
         form.form_depth <- t;
         format_ezone form.form_buffer
    | [] ->
         ()

let pp_print_string form s =
   format_string form.form_buffer s

let pp_print_as form i s =
   let len = String.length s in
      if i < len then
         begin
            format_string form.form_buffer (String.sub s 0 i);
            format_izone form.form_buffer;
            format_string form.form_buffer (String.sub s i (len - i));
            format_ezone form.form_buffer
         end
      else
         begin
            format_string form.form_buffer s;
            if i > len then
               format_string form.form_buffer (String.make (i - len) ' ')
         end

let pp_print_int form i =
   format_int form.form_buffer i

let pp_print_float form x =
   format_string form.form_buffer (string_of_float x)

let pp_print_char form c =
   format_char form.form_buffer c

let pp_print_bool form x =
   format_string form.form_buffer (if x then "true" else "false")

let pp_print_break form nspaces offset =
   format_sbreak form.form_buffer (String.make nspaces ' ') (String.make offset ' ')

let pp_print_cut form () =
   format_sbreak form.form_buffer "" ""

let pp_print_space form () =
   format_hspace form.form_buffer

let pp_force_newline form () =
   format_newline form.form_buffer

let pp_print_flush form () =
   flush_form form;
   form.form_out_flush ()

let pp_print_newline form () =
   flush_form form;
   form.form_out_newline ();
   form.form_out_flush ()

let pp_print_if_newline form () =
   ()

let pp_open_tbox form () =
   ()

let pp_close_tbox form () =
   ()

let pp_print_tbreak = pp_print_break

let pp_set_tab form () =
   ()

let pp_print_tab form () =
   ()

let pp_set_margin form margin =
   form.form_margin <- margin

let pp_get_margin form () =
   form.form_margin

let pp_set_max_indent form indent =
   form.form_max_indent <- indent

let pp_get_max_indent form () =
   form.form_max_indent

let pp_set_max_boxes form max =
   form.form_max_boxes <- max

let pp_get_max_boxes form () =
   form.form_max_boxes

let pp_over_max_boxes form () =
   List.length form.form_depth > form.form_max_boxes

let pp_set_ellipsis_text form text =
   form.form_ellipsis <- text

let pp_get_ellipsis_text form () =
   form.form_ellipsis

let pp_set_formatter_out_channel form outx =
   form.form_out_string <- output_substring outx;
   form.form_out_flush <- (fun () -> flush outx);
   form.form_out_newline <- (fun () -> output_char outx '\n');
   form.form_out_space <- (fun i ->
                                for j = 0 to pred i do
                                   output_char outx ' '
                                done)

let pp_set_formatter_output_functions form outx flush =
   form.form_out_string <- outx;
   form.form_out_flush <- flush

let pp_get_formatter_output_functions form () =
   form.form_out_string, form.form_out_flush

let pp_set_all_formatter_output_functions form outx flush outnewline outspace =
   form.form_out_string <- outx;
   form.form_out_flush <- flush;
   form.form_out_newline <- outnewline;
   form.form_out_space <- outspace

let pp_get_all_formatter_output_functions form () =
   form.form_out_string,
   form.form_out_flush,
   form.form_out_newline,
   form.form_out_space

(************************************************************************
 * STANDARD BUFFERS                                                     *
 ************************************************************************)

let formatter_of_out_channel outx =
   { form_buffer = new_buffer ();
     form_depth = [];
     form_out_string = output_substring outx;
     form_out_flush = (fun () -> flush outx);
     form_out_newline = (fun () -> output_char outx '\n');
     form_out_space = (fun i ->
                            for j = 0 to pred i do
                               output_char outx ' '
                            done);
     form_max_boxes = default_max_boxes;
     form_max_indent = default_max_indent;
     form_ellipsis = default_ellipsis;
     form_margin = default_margin
   }

let std_formatter = formatter_of_out_channel stdout
let err_formatter = formatter_of_out_channel stderr

let formatter_of_buffer buf =
   { form_buffer = new_buffer ();
     form_depth = [];
     form_out_string = Buffer.add_substring buf;
     form_out_flush = (fun () -> ());
     form_out_newline = (fun () -> Buffer.add_char buf '\n');
     form_out_space =
        (fun i ->
              for j = 0 to pred i do
                 Buffer.add_char buf ' '
              done);
     form_max_boxes = default_max_boxes;
     form_max_indent = default_max_indent;
     form_ellipsis = default_ellipsis;
     form_margin = default_margin
   }

let stdbuf = Buffer.create 19

let str_formatter = formatter_of_buffer stdbuf

let flush_str_formatter () =
   flush_form str_formatter;
   let s = Buffer.contents stdbuf in
      Buffer.clear stdbuf;
      s

let make_formatter outx flush =
   { form_buffer = new_buffer ();
     form_depth = [];
     form_out_string = outx;
     form_out_flush = flush;
     form_out_newline = (fun () -> outx "\n" 0 1);
     form_out_space = (fun i ->
                            for j = 0 to pred i do
                               outx " " 0 1
                            done);
     form_max_boxes = default_max_boxes;
     form_max_indent = default_max_indent;
     form_ellipsis = default_ellipsis;
     form_margin = default_margin
   }

(*
 * Boxes.
 *)
let open_box = pp_open_box std_formatter
let open_vbox = pp_open_vbox std_formatter
let open_hbox = pp_open_hbox std_formatter
let open_hvbox = pp_open_hvbox std_formatter
let open_hovbox = pp_open_hovbox std_formatter
let close_box = pp_close_box std_formatter

(*
 * Formatting functions.
 *)
let print_string = pp_print_string std_formatter
let print_as = pp_print_as std_formatter
let print_int = pp_print_int std_formatter
let print_float = pp_print_float std_formatter
let print_char = pp_print_char std_formatter
let print_bool = pp_print_bool std_formatter

(*
 * Break hints.
 *)
let print_space = pp_print_space std_formatter
let print_cut = pp_print_cut std_formatter
let print_break = pp_print_break std_formatter
let print_flush = pp_print_flush std_formatter
let print_newline = pp_print_newline std_formatter
let force_newline = pp_force_newline std_formatter
let print_if_newline = pp_print_if_newline std_formatter

(*
 * Margin.
 *)
let set_margin = pp_set_margin std_formatter
let get_margin = pp_get_margin std_formatter

(*
 * Indentation limit.
 *)
let set_max_indent = pp_set_max_indent std_formatter
let get_max_indent = pp_get_max_indent std_formatter

(*
 * Formatting depth.
 *)
let set_max_boxes = pp_set_max_boxes std_formatter
let get_max_boxes = pp_get_max_boxes std_formatter
let over_max_boxes = pp_over_max_boxes std_formatter

(*
 * Tabulations.
 *)
let open_tbox = pp_open_tbox std_formatter
let close_tbox = pp_close_tbox std_formatter
let print_tbreak = pp_print_tbreak std_formatter
let set_tab = pp_set_tab std_formatter
let print_tab = pp_print_tab std_formatter

(*
 * Ellipsis.
 *)
let set_ellipsis_text = pp_set_ellipsis_text std_formatter
let get_ellipsis_text = pp_get_ellipsis_text std_formatter

(*
 * Redirecting formatter output.
 *)
let set_formatter_out_channel = pp_set_formatter_out_channel std_formatter
let set_formatter_output_functions = pp_set_formatter_output_functions std_formatter
let get_formatter_output_functions = pp_get_formatter_output_functions std_formatter
let set_all_formatter_output_functions = pp_set_all_formatter_output_functions std_formatter
let get_all_formatter_output_functions = pp_get_all_formatter_output_functions std_formatter

(************************************************************************
 * PRINTF
 ************************************************************************)

(*
 * Format args.
 *)
module Args =
struct
   type t = formatter

   let print_char = pp_print_char
   let print_string = pp_print_string

   let open_box = pp_open_box
   let open_hbox fmt = pp_open_hbox fmt ()
   let open_vbox = pp_open_vbox
   let open_hvbox = pp_open_hvbox
   let open_hovbox = pp_open_hovbox
   let close_box fmt = pp_close_box fmt ()

   let print_cut fmt = pp_print_cut fmt ()
   let print_space fmt = pp_print_space fmt ()
   let force_newline fmt = pp_force_newline fmt ()
   let print_break = pp_print_break
   let print_flush fmt = pp_print_flush fmt ()
   let print_newline fmt = pp_print_newline fmt ()
end

module Printf = MakePrintf (Args)

let fprintf = Printf.fprintf

let printf s =
   fprintf std_formatter s

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
