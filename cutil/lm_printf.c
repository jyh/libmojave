/*
 * These printers use printf, both to ensure compatibility
 * with libc, and to make our life easier.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2001 Mojave Group, Caltech
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 * 
 * Additional permission is given to link this library with the
 * OpenSSL project's "OpenSSL" library, and with the OCaml runtime,
 * and you may distribute the linked executables.  See the file
 * LICENSE.libmojave for more details.
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
#ifdef HAVE_SNPRINTF
    if(snprintf(buffer, sizeof(buffer), fmt, c) < 0)
        failwith("ml_print_char");
#else
    if(sprintf(buffer, fmt, c) < 0)
        failwith("ml_print_char");
#endif
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
#ifdef HAVE_SNPRINTF
    if(snprintf(buffer, sizeof(buffer), fmt, i) < 0)
        failwith("ml_print_int");
#else
    if(sprintf(buffer, fmt, i) < 0)
        failwith("ml_print_int");
#endif
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
#ifdef HAVE_SNPRINTF
    if(snprintf(buffer, sizeof(buffer), fmt, x) < 0)
        failwith("ml_print_float");
#else
    if(sprintf(buffer, fmt, x) < 0)
        failwith("ml_print_float");
#endif
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
#ifdef HAVE_SNPRINTF
    if(snprintf(buffer, sizeof(buffer), fmt, s) < 0)
        failwith("ml_print_string");
#else
    if(sprintf(buffer, fmt, s) < 0)
        failwith("ml_print_string");
#endif
    return copy_string(buffer);
}

