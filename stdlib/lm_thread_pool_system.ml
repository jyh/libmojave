(*
 * On Win32, select does not work on pipes.  Instead, we use
 * threads to call all the handlers.  We keep a thread pool.
 * When a thread makes progress, it wakes up the main process,
 * and returns to the pool.  Each file descriptor is assigned
 * a thread.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003-2005 Jason Hickey, Caltech
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
open Lm_printf
open Lm_debug

(*
 * Build debugging.
 *)
let debug_thread =
   create_debug (**)
      { debug_name = "thread";
        debug_description = "Display thread debugging";
        debug_value = false
      }

(*
 * Data structures.
 *)
module IntCompare =
struct
   type t = int
   let compare = (-)
end

module IntTable = Lm_map.LmMake (IntCompare)

(*
 * Jobs are identified by descriptor.
 * If the job is not visible, it is not reported
 * to wait.
 *)
type job =
   { job_id  : int;
     job_fun : unit -> unit;
     job_visible : bool
   }

(*
 * We keep a master lock: only one thread is allowed to run at
 * any given time.  This doesn't really affect performance,
 * since OCaml enforces this restriction anyway.  Note: the
 * threads should release the lock before they wait for I/O.
 *)
type pool =
   { mutable pool_pid          : int;
     mutable pool_size         : int;
     mutable pool_ready        : job list;
     mutable pool_ready_length : int;
     mutable pool_running      : job IntTable.t;
     mutable pool_finished     : job list;
     pool_finished_wait        : Condition.t;
     pool_consumer_wait        : Condition.t;
     pool_lock                 : Mutex.t
   }

(*
 * The pool.
 *)
let pool =
   { pool_pid           = 1;
     pool_size          = 0;
     pool_ready         = [];
     pool_ready_length  = 0;
     pool_running       = IntTable.empty;
     pool_finished      = [];
     pool_finished_wait = Condition.create ();
     pool_consumer_wait = Condition.create ();
     pool_lock          = Mutex.create ()
   }

(*
 * Lock for the main thread.
 *)
let () = Mutex.lock pool.pool_lock

(*
 * Threads are enabled.
 *)
let enabled = true

(*
 * Temporarily unlock the pool while performing IO.
 * The check_status function may generate exceptions.
 *)
let blocking_section f x =
   Mutex.unlock pool.pool_lock;
   try
      let y = f x in
         Mutex.lock pool.pool_lock;
         y
   with
      exn ->
         Mutex.lock pool.pool_lock;
         raise exn

let resume_inner_section f x =
   Mutex.lock pool.pool_lock;
   try
      let y = f x in
         Mutex.unlock pool.pool_lock;
         y
   with
      exn ->
         Mutex.unlock pool.pool_lock;
         raise exn

(*
 * Thread main loop.
 *)
let thread_main_loop () =
   let id = Thread.id (Thread.self ()) in
      Mutex.lock pool.pool_lock;
      if !debug_thread then
         eprintf "Thread %d: entered main loop@." id;
      let rec loop () =
         match pool.pool_ready with
            job :: rest ->
               pool.pool_ready <- rest;
               pool.pool_ready_length <- pred pool.pool_ready_length;
               pool.pool_running <- IntTable.add pool.pool_running job.job_id job;
               if !debug_thread then
                  eprintf "Thread %d: calling function: %d@." id job.job_id;
               (try job.job_fun () with
                   exn ->
                      eprintf "Omake_exec_thread: thread raised exception: %s: %d@." (Printexc.to_string exn) job.job_id;
                      ());
               pool.pool_running <- IntTable.remove pool.pool_running job.job_id;
               if job.job_visible then
                  begin
                     pool.pool_finished <- job :: pool.pool_finished;
                     Condition.signal pool.pool_finished_wait
                  end;
               loop ()
          | [] ->
               if !debug_thread then
                  eprintf "Thread %d: waiting@." id;
               Condition.wait pool.pool_consumer_wait pool.pool_lock;
               if !debug_thread then
                  eprintf "Thread %d: waited@." id;
               loop ()
      in
         loop ()

(*
 * Start a thread doing something.
 *)
let create visible f =
   let id = succ pool.pool_pid in
   let job =
      { job_id = id;
        job_fun = f;
        job_visible = visible
      }
   in
      pool.pool_pid <- id;
      pool.pool_ready <- job :: pool.pool_ready;
      pool.pool_ready_length <- succ pool.pool_ready_length;

      (* Enlarge the pool if needed *)
      if pool.pool_size < pool.pool_ready_length + IntTable.cardinal pool.pool_running then
         begin
            pool.pool_size <- succ pool.pool_size;
            ignore (Thread.create thread_main_loop ())
         end;

      (* Wake up one of the waiters if they are waiting *)
      Condition.signal pool.pool_consumer_wait;
      if !debug_thread then
         eprintf "Create: %d@." id;
      id

(*
 * Wait until something happens, and return the identifier of
 * all the threads that completed.
 *)
let wait () =
   (* Wait until a thread finishes *)
   while pool.pool_finished = [] do
      if !debug_thread then
         eprintf "Main: waiting: %d+%d@." pool.pool_ready_length (IntTable.cardinal pool.pool_running);
      Condition.wait pool.pool_finished_wait pool.pool_lock;
      if !debug_thread then
         eprintf "Main: waited@.";
   done;

   (* Return pids of all the threads that finished *)
   let pids = List.map (fun job -> job.job_id) pool.pool_finished in
      pool.pool_finished <- [];
      pids

(*
 * Wait until a specific pid disappears.
 *)
let waitpid id =
   (* Wait until a thread finishes *)
   while IntTable.mem pool.pool_running id do
      if !debug_thread then
         eprintf "Main: waiting: %d+%d@." pool.pool_ready_length (IntTable.cardinal pool.pool_running);
      Condition.wait pool.pool_finished_wait pool.pool_lock;
      if !debug_thread then
         eprintf "Main: waited@.";
   done

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
