(*
 * Override the usual out_channels to use the Lm_buffer module.
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
open Lm_pervasives

(*
 * Printing.
 *)
val eprintf : ('a, out_channel, unit) format -> 'a
val printf  : ('a, out_channel, unit) format -> 'a
val sprintf : ('a, unit, string) format -> 'a
val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a

(*
 * Flushing.
 *)
val eflush : out_channel -> unit

(*
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
