/*
 * These printers use printf, both to ensure compatibility
 * with libc, and to make our life easier.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2001 Jason Hickey, Caltech
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
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

/*
 * Some large buffer.
 */
#define BUFSIZE         (1 << 12)

/*
 * Print a char.
 */
value ml_print_char(value v_fmt, value v_char)
{
    char buffer[BUFSIZE];
    char *fmt = String_val(v_fmt);
    char c = Int_val(v_char);
    if(snprintf(buffer, sizeof(buffer), fmt, c) < 0)
        failwith("ml_print_char");
    return copy_string(buffer);
}

/*
 * Print an int.
 */
value ml_print_int(value v_fmt, value v_int)
{
    char buffer[BUFSIZE];
    char *fmt = String_val(v_fmt);
    int i = Int_val(v_int);
    if(snprintf(buffer, sizeof(buffer), fmt, i) < 0)
        failwith("ml_print_int");
    return copy_string(buffer);
}


/*
 * Print an int.
 */
value ml_print_float(value v_fmt, value v_float)
{
    char buffer[BUFSIZE];
    char *fmt = String_val(v_fmt);
    double x = Double_val(v_float);
    if(snprintf(buffer, sizeof(buffer), fmt, x) < 0)
        failwith("ml_print_float");
    return copy_string(buffer);
}


/*
 * Print an int.
 */
value ml_print_string(value v_fmt, value v_string)
{
    char buffer[BUFSIZE];
    char *fmt = String_val(v_fmt);
    char *s = String_val(v_string);
    if(snprintf(buffer, sizeof(buffer), fmt, s) < 0)
        failwith("ml_print_string");
    return copy_string(buffer);
}

