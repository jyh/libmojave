/*
 * A special "custom" value that prohibits Pervasives.compare,
 * but allows marshalling. By temporarily adding this value as a
 * _first_ field in a * data structure that is not supposed to be
 * compared using Pervasives.compare, one can weed out all the
 * inappropriate usages of Pervasives.compare, (=), (<), etc.
 *
 * ------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2005, MetaPRL Group
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
 * Author: Aleksey Nogin @email{nogin@cs.caltech.edu}
 * @end[license]
 */

/* Caml includes */
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>

static void lm_nocompare_serialize(value v, unsigned long * wsize_32, unsigned long * wsize_64)
{
    *wsize_32 = 0;
    *wsize_64 = 0;
}

static unsigned long lm_nocompare_deserialize(void * dst)
{
    return 0;
}

static void lm_nomarshal_serialize(value v, unsigned long * wsize_32, unsigned long * wsize_64)
{
    caml_invalid_argument("Lm_nocompare: some code attempted to marshal a value\n   on a data structure that specifically prohibits this");
    *wsize_32 = 0;
    *wsize_64 = 0;
}

static unsigned long lm_nomarshal_deserialize(void * dst)
{
    caml_invalid_argument("Lm_nocompare: some code attempted to marshal a value\n   on a data structure that specifically prohibits this");
    return 0;
}

static int lm_nocompare_compare(value v1, value v2)
{
    caml_invalid_argument("Lm_nocompare: some code attempted to use a Pervasives comparison operation\n   on a data structure that specifically prohibits this");
}

struct custom_operations lm_nocompare_ops = {
    "LibMojave/Lm_nocompare",
    custom_finalize_default,
    lm_nocompare_compare,
    custom_hash_default,
    lm_nocompare_serialize,
    lm_nocompare_deserialize
};

struct custom_operations lm_nomarshal_ops = {
    "LibMojave/Lm_nomarshal",
    custom_finalize_default,
    lm_nocompare_compare,
    custom_hash_default,
    lm_nomarshal_serialize,
    lm_nomarshal_deserialize
};

value lm_nocompare_create(value v)
{
    CAMLparam1(v);
    CAMLlocal1(result);
    result = caml_alloc_custom(&lm_nocompare_ops, 0, 0, 0);
    CAMLreturn(result);
}

value lm_nomarshal_create(value v)
{
    CAMLparam1(v);
    CAMLlocal1(result);
    result = caml_alloc_custom(&lm_nomarshal_ops, 0, 0, 0);
    CAMLreturn(result);
}

value lm_nocompare_init(value v)
{
    caml_register_custom_operations(&lm_nocompare_ops);
    caml_register_custom_operations(&lm_nomarshal_ops);
    return Val_unit;
}
