(*
 * A special "custom" value that prohibits Pervasives.compare,
 * but allows marshalling. By temporarily adding Lm_nocompare.it as a
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
 *)

type t

val it : t
val nocompare : t
val nomarshal : t

