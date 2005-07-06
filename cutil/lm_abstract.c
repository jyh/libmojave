/*
 * Raw data operations on strings.
 *
 * ------------------------------------------------
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

/* Standard includes */
#include <stdio.h>
#include <memory.h>
#include <stdlib.h>
#include <sys/types.h>

/* Caml includes */
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

/*
 * Make a new abstract value.
 * We start counting from 0.
 */
value new_abstract()
{
    return (value) alloc(1, Abstract_tag);
}

value string_compare (value v1, value v2) {
   mlsize_t len1, len2, len;
   unsigned char * p1, * p2;
   len1 = string_length(v1);
   len2 = string_length(v2);
	int i = len1 - len2;
   if (i) return Val_int(i);
   for (len = len1,
          p1 = (unsigned char *) String_val(v1),
          p2 = (unsigned char *) String_val(v2);
        len > 0;
        len--, p1++, p2++)
     if (*p1 != *p2) return Val_int((int)*p1 - (int)*p2);
	return Val_int(0);
}

