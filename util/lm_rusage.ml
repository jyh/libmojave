(*
 * Simple rusage interface.
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


(* rusage
   Data type that contains information on process resource usage.  Currently
   a limited form of the full data structure returned by libc.  This contains
   the user and system runtimes, broken down into seconds and microseconds,
   for the requested processes.  *)
type rusage = 
   { ru_utime_sec  : int;
     ru_utime_usec : int;
     ru_stime_sec  : int;
     ru_stime_usec : int;
   }


(* rusage_who
   Who to report statistics on: the current process, or the aggregate of all
   its children.  *)
type rusage_who =
   RUSAGE_SELF
 | RUSAGE_CHILDREN


(***  External Calls  ***)


external caml_getrusage_time : rusage_who -> rusage = "caml_getrusage_time"
external caml_setrlimit_time : int -> unit = "caml_setrlimit_time"


(***  Interface  ***)


(* getrusage_time who
   Returns the resource usage of the indicated process.  *)
let getrusage_time who = caml_getrusage_time who


(* setrlimit_time time
   Sets the resource CPU limit for the current process to the indicated
   time interval (in seconds).  Note that this number can only be decreased
   from the current resource limit; it cannot be increased.  *)
let setrlimit_time time = caml_setrlimit_time time


(* total_rusage rusage
   Takes the rusage structure returned by getrusage_time, and computes the
   total compute time of the process.  Returns the pair (sec, usec) which
   indicate the process total running time.  *)
let total_rusage rusage =
   let sec  = rusage.ru_utime_sec  + rusage.ru_stime_sec in
   let usec = rusage.ru_utime_usec + rusage.ru_stime_usec in
   let rec increment_sec sec usec =
      if usec >= 1000000 then
         increment_sec (sec + 1) (usec - 1000000)
      else
         sec, usec
   in
      increment_sec sec usec
