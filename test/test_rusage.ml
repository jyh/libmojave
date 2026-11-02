(* Lm_rusage against the operating system. Times vary, so only
   true/false is printed. On Unix: after burning CPU the process's user
   time is positive and agrees with Unix.times (both read getrusage);
   microseconds stay below 1e6; a second read is not smaller; a child
   that burns CPU shows in RUSAGE_CHILDREN once waited; and a child that
   sets its CPU limit to 1 s and spins is killed by SIGXCPU. On Windows
   the stubs are "not implemented" and return zeros, and setrlimit_time
   does nothing; test_rusage.win32.expected pins that. *)
open Pin

(* Spin in user space; the clock (a system call) is read only every 10M
   steps, so the time spent is user time, not system time. *)
let burn seconds =
  let t0 = Sys.time () in
  let x = ref 0 in
  while Sys.time () -. t0 < seconds do
    for i = 1 to 10_000_000 do x := !x lxor i done
  done;
  ignore (Sys.opaque_identity !x)

let secs s us = float_of_int s +. float_of_int us *. 1e-6
let user (r : Lm_rusage.rusage_short) = secs r.ru_utime_sec r.ru_utime_usec
let usec_ok (r : Lm_rusage.rusage_short) =
  r.ru_utime_usec >= 0 && r.ru_utime_usec < 1_000_000
  && r.ru_stime_usec >= 0 && r.ru_stime_usec < 1_000_000
let near a b = Float.abs (a -. b) < 0.05
let b = string_of_bool

let () =
  burn 0.3;
  let r1 = Lm_rusage.getrusage_time Lm_rusage.RUSAGE_SELF in
  let t = Unix.times () in
  let full = Lm_rusage.getrusage () in
  let r2 = Lm_rusage.getrusage_time Lm_rusage.RUSAGE_SELF in
  if Sys.win32 then begin
    line "getrusage_time RUSAGE_SELF" "on Windows" (b (r1 = { ru_utime_sec = 0; ru_utime_usec = 0; ru_stime_sec = 0; ru_stime_usec = 0 }));
    line "getrusage" "on Windows" (b (full.ru_utime = 0.0 && full.ru_stime = 0.0 && full.ru_maxrss = 0 && full.ru_nivcsw = 0));
    line "setrlimit_time 1" "on Windows" (guard (fun () -> Lm_rusage.setrlimit_time 1; burn 1.5; "returned, no limit"))
  end else begin
    line "getrusage_time RUSAGE_SELF" "after 0.3 s of CPU: user time > 0.2" (b (user r1 > 0.2));
    line "getrusage_time" "microseconds in [0, 1e6)" (b (usec_ok r1 && usec_ok r2));
    line "getrusage_time" "user time agrees with Unix.times within 0.05 s" (b (near (user r1) t.Unix.tms_utime));
    line "getrusage_time" "a second read is not smaller" (b (user r2 >= user r1));
    line "getrusage" "ru_utime agrees with getrusage_time within 0.05 s" (b (near full.ru_utime (user r1)));
    let c0 = user (Lm_rusage.getrusage_time Lm_rusage.RUSAGE_CHILDREN) in
    (match Unix.fork () with
     | 0 -> burn 0.3; exit 0
     | pid -> ignore (Unix.waitpid [] pid));
    let c1 = user (Lm_rusage.getrusage_time Lm_rusage.RUSAGE_CHILDREN) in
    line "getrusage_time RUSAGE_CHILDREN" "grows by > 0.2 s after a child burns 0.3 s" (b (c1 -. c0 > 0.2));
    (match Unix.fork () with
     | 0 -> Lm_rusage.setrlimit_time 1; burn 10.0; exit 0
     | pid ->
       let _, st = Unix.waitpid [] pid in
       line "setrlimit_time 1" "a spinning child is killed by SIGXCPU"
         (b (st = Unix.WSIGNALED Sys.sigxcpu)))
  end
