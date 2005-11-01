/*
 * Get characters from the current locale.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave group, Caltech
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
#include <ctype.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/custom.h>

/*
 * Character classes.
 */
static value get_chars(int (*f)(int))
{
    char buf[256];
    char *p;
    int i;

    p = buf;
    for(i = 0; i != 256; i++) {
        if(f(i))
            *p++ = i;
    }
    *p = 0;
    return copy_string(buf);
}

value omake_alnum(value v_unit)
{
    return get_chars(isalnum);
}

value omake_alpha(value v_unit)
{
    return get_chars(isalpha);
}

value omake_graph(value v_unit)
{
    return get_chars(isgraph);
}

value omake_lower(value v_unit)
{
    return get_chars(islower);
}

value omake_upper(value v_unit)
{
    return get_chars(isupper);
}

value omake_punct(value v_unit)
{
    return get_chars(ispunct);
}

value omake_space(value v_unit)
{
    return get_chars(isspace);
}
