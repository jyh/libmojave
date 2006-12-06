/*
 * Get terminal size
 *
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2000-2006 Mojave Group, Caltech
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
 * Modified by: Aleksey Nogin <nogin@cs.cornell.edu>
 */
#include <stdio.h>
#ifdef __CYGWIN__
#   include <sys/termios.h>
#endif
#ifndef WIN32
#   include <sys/ioctl.h>
#endif

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

value caml_term_size(value arg)
{
    CAMLparam1(arg);
    CAMLlocal1(buf);

    /* Return a pair of numbers */
    buf = alloc_small(2, 0);

    /* Get the terminal size, return None on failure */
#ifdef WIN32
    Field(buf, 0) = Val_int(24);
    Field(buf, 1) = Val_int(80);
#else
    {
        struct winsize ws;

        if(ioctl(Int_val(arg), TIOCGWINSZ, &ws) < 0)
            failwith("termsize: standard input is not a terminal");
    
        /* Return the pair of numbers */
        Field(buf, 0) = Val_int(ws.ws_row);
        Field(buf, 1) = Val_int(ws.ws_col);
    }
#endif

    CAMLreturn(buf);
}

