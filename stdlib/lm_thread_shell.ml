(*
 * A simple job interface, where separate states get integer
 * ids.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004 Mojave Group, Caltech
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
 *)
open Lm_thread
open Lm_thread_sig
open Lm_printf

(*
 * List of states, indexed by pid.
 *)
type pid = string * int

module PidCompare =
struct
   type t = pid

   let compare ((s1, i1) : pid) ((s2, i2) : pid) =
      let cmp = i1 - i2 in
         if cmp = 0 then
            compare s1 s2
         else
            cmp
end

module PidTable = Lm_map.LmMake (PidCompare)

type job_type =
   HiddenJob
 | VisibleJob

type job =
   { job_type   : job_type;
     job_state  : State.t
   }

type info =
   { mutable info_jobs  : job PidTable.t }

type current =
   { mutable current_pid   : pid;
     mutable current_state : State.t
   }

(*
 * Common state for all threads.
 *)
let root_id = "id", 1

let info_entry =
   let current = State.current () in
   let job =
      { job_type   = VisibleJob;
        job_state  = current
      }
   in
   let info =
      { info_jobs  = PidTable.add PidTable.empty root_id job }
   in
      State.shared_val "Lm_thread_shell.info" info

(*
 * Sub-state for each thread.
 *)
let current_entry =
   let current =
      { current_pid = root_id;
        current_state = State.current ()
      }
   in
   let fork current =
      { current with current_pid = current.current_pid }
   in
      State.private_val "Lm_thread_shell.current" current fork

(*
 * Get a process name that is not used.
 *)
let new_pid info name =
   let jobs = info.info_jobs in
   let rec search i =
      let pid = name, i in
         if PidTable.mem jobs pid then
            search (succ i)
         else
            pid
   in
      search 1

(*
 * Create a new pid with its own state.
 *)
let create name job_type =
   State.write info_entry (fun info ->
         let pid = new_pid info name in
         let { info_jobs = jobs } = info in
         let state = State.create () in
         let job =
            { job_type  = job_type;
              job_state = state
            }
         in
            State.with_state state (fun () ->
                  State.write current_entry (fun current ->
                        current.current_pid <- pid;
                        current.current_state <- state)) ();
            info.info_jobs <- PidTable.add jobs pid job;
            pid)

(*
 * Get the pid for a string.
 * Check that it is defined.
 *)
let pid_of_string pid =
   (* Split the string based in integer suffix *)
   let len = String.length pid in
   let pid =
      let rec search i =
         if i = 0 then
            raise Not_found;
         let j = pred i in
            match pid.[j] with
               '0'..'9' ->
                  search j
             | _ ->
                  if i = len then
                     raise Not_found;
                  let id = String.sub pid 0 i in
                  let i = int_of_string (String.sub pid i (len - i)) in
                     id, i
      in
         search len
   in
      State.read info_entry (fun info ->
            if PidTable.mem info.info_jobs pid then
               pid
            else
               raise Not_found)

let string_of_pid (id, i) =
   id ^ string_of_int i

(*
 * Create a job with an exact id.
 *)
let create_or_find pid mode =
   try pid_of_string pid with
      Not_found ->
         create pid mode

(*
 * Return the current pid.
 *)
let get_pid () =
   State.read current_entry (fun current -> current.current_pid)

let get_pids () =
   List.rev (State.read info_entry (fun info ->
                   PidTable.fold (fun pids pid job ->
                         match job.job_type with
                            HiddenJob ->
                               pids
                          | VisibleJob ->
                               pid :: pids) [] info.info_jobs))

(*
 * Set the pid used by all processes.
 *)
let set_pid pid =
   State.read info_entry (fun info ->
   State.write current_entry (fun current ->
         let state = (PidTable.find info.info_jobs pid).job_state in
            current.current_state <- state;
            current.current_pid <- pid;
            State.set state))

(*
 * Evaluate in the current pid.
 *)
let with_current f x =
   let state = State.read current_entry (fun current -> current.current_state) in
      State.with_state state f x

(*
 * Evaluate in a specific pid.
 *)
let with_pid pid f x =
   let job =
      State.read info_entry (fun info ->
            PidTable.find info.info_jobs pid)
   in
      State.with_state job.job_state f x

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
