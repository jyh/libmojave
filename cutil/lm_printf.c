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
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#ifdef WIN32 
#include <windows.h>
/* Disable some of the warnings */
#pragma warning( disable : 4996)
#endif /* WIN32 */

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
    char c = (char) Int_val(v_char);
#ifdef HAVE_SNPRINTF
    if(snprintf(buffer, sizeof(buffer), fmt, c) < 0)
        caml_failwith("ml_print_char");
#else
    if(sprintf(buffer, fmt, c) < 0)
        caml_failwith("ml_print_char");
#endif
    return caml_copy_string(buffer);
}

/*
 * Print an int.
 *
 * The value is an OCaml int (intnat), not a C int: the length modifier
 * for intnat (ARCH_INTNAT_PRINTF_FORMAT) goes before the conversion,
 * and an unsigned conversion (u x X o) prints the int's bits as
 * Stdlib.Printf does, masked to the width of an OCaml int (%x of -1 is
 * 7fffffffffffffff on a 64-bit machine).  Until 2026 the value was read
 * as an int, so anything outside 32 bits printed wrong.
 */
value ml_print_int(value v_fmt, value v_int)
{
    char buffer[BUFSIZE];
    char fmt[64];
    const char *spec = String_val(v_fmt);
    size_t len = caml_string_length(v_fmt);
    intnat i = Long_val(v_int);
    uintnat mask = (((uintnat) 1) << (8 * sizeof(value) - 1)) - 1;
    char conv;
    int code;

    if(len < 2 || len - 1 + sizeof(ARCH_INTNAT_PRINTF_FORMAT) + 1 > sizeof(fmt))
        caml_failwith("ml_print_int");
    conv = spec[len - 1];
    if(conv == 'u' || conv == 'x' || conv == 'X' || conv == 'o') {
        /* + and space apply only to signed conversions; MinGW's snprintf
           honours the space for %u anyway, so drop both here */
        size_t i, j = 0;
        for(i = 0; i < len - 1; i++)
            if(spec[i] != '+' && spec[i] != ' ')
                fmt[j++] = spec[i];
        fmt[j] = 0;
    }
    else {
        memcpy(fmt, spec, len - 1);
        fmt[len - 1] = 0;
    }
    strcat(fmt, ARCH_INTNAT_PRINTF_FORMAT);
    len = strlen(fmt);
    fmt[len] = conv;
    fmt[len + 1] = 0;
    if(conv == 'u' || conv == 'x' || conv == 'X' || conv == 'o') {
#ifdef HAVE_SNPRINTF
        code = snprintf(buffer, sizeof(buffer), fmt, ((uintnat) i) & mask);
#else
        code = sprintf(buffer, fmt, ((uintnat) i) & mask);
#endif
    }
    else {
#ifdef HAVE_SNPRINTF
        code = snprintf(buffer, sizeof(buffer), fmt, i);
#else
        code = sprintf(buffer, fmt, i);
#endif
    }
    if(code < 0)
        caml_failwith("ml_print_int");
    return caml_copy_string(buffer);
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
        caml_failwith("ml_print_float");
#else
    if(sprintf(buffer, fmt, x) < 0)
        caml_failwith("ml_print_float");
#endif
    return caml_copy_string(buffer);
}

/*
 * Print a string.
 */
value ml_print_string(value v_fmt, value v_string)
{
    char buffer[BUFSIZE], *bufp;
    int len, size, code;
    char *fmt, *s;
    value v_result;

    /* Degenerate case if the format is %s */
    fmt = String_val(v_fmt);
    if(strcmp(fmt, "%s") == 0)
        return v_string;

    /* Make an attempt to ensure that the buffer is large enough */
    s = String_val(v_string);
    len = strlen(s);
    if(len < BUFSIZE) {
        size = BUFSIZE;
        bufp = buffer;
    }
    else {
        size = len * 2;
        bufp = malloc(size);
        if(bufp == 0)
            caml_failwith("ml_print_string");
    }

#ifdef HAVE_SNPRINTF
    code = snprintf(bufp, size, fmt, s);
#else
    code = sprintf(bufp, fmt, s);
#endif
    if(code < 0) {
        if(bufp != buffer)
            free(bufp);
        caml_failwith("ml_print_string");
    }
    v_result = caml_copy_string(bufp);
    if(bufp != buffer)
        free(bufp);
    return v_result;
}

/*
 * Print a string.
 */
value ml_print_string2(value v_width, value v_fmt, value v_string)
{
    char buffer[BUFSIZE], *bufp;
    int width, len, size, code;
    char *fmt, *s;
    value v_result;

    /* Degenerate case if the format is %s */
    fmt = String_val(v_fmt);
    if(strcmp(fmt, "%s") == 0)
        return v_string;

    /* Make an attempt to ensure that the buffer is large enough */
    s = String_val(v_string);
    len = strlen(s);
    width = Int_val(v_width);
    if(width > len)
        len = width;
    if(len < BUFSIZE / 2) {
        size = BUFSIZE;
        bufp = buffer;
    }
    else {
        size = len * 2;
        bufp = malloc(size);
        if(bufp == 0)
            caml_failwith("ml_print_string");
    }

#ifdef HAVE_SNPRINTF
    code = snprintf(bufp, size, fmt, s);
#else
    code = sprintf(bufp, fmt, s);
#endif
    if(code < 0) {
        if(bufp != buffer)
            free(bufp);
        caml_failwith("ml_print_string");
    }
    v_result = caml_copy_string(bufp);
    if(bufp != buffer)
        free(bufp);
    return v_result;
}

