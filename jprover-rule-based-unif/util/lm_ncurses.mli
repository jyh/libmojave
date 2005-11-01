(*
 * Simple NCurses interface.
 * Copyright (C) 2002 Justin David Smith, Caltech
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
 *)


type window


type attr =
   A_NORMAL
 | A_STANDOUT
 | A_UNDERLINE
 | A_REVERSE
 | A_BLINK
 | A_DIM
 | A_BOLD
 | A_PROTECT
 | A_INVIS
 | A_ALTCHARSET
 | A_CHARTEXT


val key_down   : int
val key_up     : int
val key_left   : int
val key_right  : int
val key_home   : int
val key_end    : int
val key_npage  : int
val key_ppage  : int
val key_enter  : int
val key_cancel : int

val key_err    : int
val key_ctrla  : int
val key_ctrld  : int
val key_ctrle  : int
val key_ctrlj  : int
val key_ctrll  : int
val key_ctrlm  : int
val key_ctrlu  : int
val key_ctrlv  : int


val curses_enabled : bool
val initscr : unit -> unit
val endwin : unit -> unit
val newwin : int -> int -> int -> int -> window
val delwin : window -> unit
val waddch : window -> char -> unit
val waddstr : window -> string -> unit
val wattron : window -> attr -> unit
val wattroff : window -> attr -> unit
val wgetch : window -> int
val wgetstr : window -> string
val wrefresh : window -> unit
val wnoutrefresh : window -> unit
val doupdate : unit -> unit
val refreshscreen : unit -> unit
val werase : window -> unit
val wclrtoeol : window -> unit
val wclrtobot : window -> unit
val wmove : window -> int -> int -> unit
val getyx : window -> int * int
val getmaxyx : window -> int * int
val scrollok : window -> bool -> unit
val echook : bool -> unit
val wscrl : window -> int -> unit
