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
open Lm_int_set
open Lm_thread
open Lm_thread_sig

(*
 * List of states, indexed by pid.
 *)
type pid = int

type job =
   { job_hidden : bool;
     job_state  : State.t
   }

type info =
   { mutable info_index         : int;
     mutable info_jobs          : job IntTable.t;
   }

type current =
   { mutable current_pid   : int;
     mutable current_state : State.t
   }

(*
 * Common state for all threads.
 *)
let info_entry =
   let current = State.current () in
   let job =
      { job_hidden = false;
        job_state  = current
      }
   in
   let info =
      { info_index = 2;
        info_jobs  = IntTable.add IntTable.empty 1 job
      }
   in
      State.shared_val "Lm_thread_shell.info" info

(*
 * Sub-state for each thread.
 *)
let current_entry =
   let current =
      { current_pid = 1;
        current_state = State.current ()
      }
   in
   let fork current =
      { current with current_pid = current.current_pid }
   in
      State.private_val "Lm_thread_shell.current" current fork

(*
 * Create a new pid with its own state.
 *)
let create hidden =
   State.write info_entry (fun info ->
         let { info_index = index;
               info_jobs = jobs
             } = info
         in
         let job =
            { job_hidden = hidden;
              job_state = State.create ()
            }
         in
            info.info_jobs <- IntTable.add jobs index job;
            info.info_index <- succ index;
            index)

(*
 * Get the pid for a string.
 * Check that it is defined.
 *)
let pid_of_string s =
   State.read info_entry (fun info ->
         let pid = int_of_string s in
            if IntTable.mem info.info_jobs pid then
               pid
            else
               raise Not_found)

(*
 * Return the current pid.
 *)
let get_pid () =
   State.read current_entry (fun current -> current.current_pid)

let get_pids () =
   List.rev (State.read info_entry (fun info ->
                   IntTable.fold (fun pids i job ->
                         if job.job_hidden then
                            pids
                         else
                            i :: pids) [] info.info_jobs))

(*
 * Set the pid used by all processes.
 *)
let set_pid pid =
   State.read info_entry (fun info ->
   State.write current_entry (fun current ->
         let state = (IntTable.find info.info_jobs pid).job_state in
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
            IntTable.find info.info_jobs pid)
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
