(* Lm_glob's ~user expansion and tilde_collapse with more than one entry in
   its table. Only the running user's own account is read: getpwuid on our
   own uid gives the real home R and login name N. The test runs itself
   again with HOME = R ^ "-c", so the table holds R-c (the home, as "~")
   beside R (as "~N", added by gethomedir N). Output names R as <R> and N
   as <N>. Cases: tilde_collapse with the home alone (a path under it, and
   one that merely starts with its name), then with R added, where R-c sorts
   between R and every path under R; and glob ~, ~N, ~N/x, an unknown user. *)
open Pin
module G = Lm_glob

let pw = Unix.getpwuid (Unix.getuid ())
let real = pw.Unix.pw_dir and me = pw.Unix.pw_name

let norm s =
  let b = Buffer.create 64 in
  let n = String.length s and lr = String.length real and lm = String.length me in
  let rec go i =
    if i >= n then ()
    else if i + lr <= n && String.sub s i lr = real then (Buffer.add_string b "<R>"; go (i + lr))
    else if i + lm <= n && String.sub s i lm = me then (Buffer.add_string b "<N>"; go (i + lm))
    else (Buffer.add_char b s.[i]; go (i + 1)) in
  go 0; Buffer.contents b

let collapse what s = line ("tilde_collapse " ^ what) "" (guard (fun () -> str (norm (G.tilde_collapse s))))
let glob what pat =
  line ("glob " ^ what) "" (guard (fun () ->
      let dirs, files = G.glob G.default_glob_options "/" [ pat ] in
      list (fun s -> str (norm s)) (dirs @ files)))

let child () =
  line "home_dir" "" (str (norm G.home_dir));
  (* the table holds only the home, <R>-c as "~" *)
  collapse "<R>-c/x" (real ^ "-c/x");
  collapse "<R>-cx/y (no separator after the entry <R>-c)" (real ^ "-cx/y");
  line "gethomedir <N> (adds <R> as \"~<N>\")" "" (guard (fun () -> str (norm (G.gethomedir me))));
  (* <R> < <R>-c < <R>/... in the table's string order *)
  collapse "<R>/x" (real ^ "/x");
  collapse "<R>" real;
  collapse "<R>-c/x, again" (real ^ "-c/x");
  glob "\"~\"" "~";
  glob "\"~<N>\"" ("~" ^ me);
  glob "\"~<N>/x\"" ("~" ^ me ^ "/x");
  glob "an unknown user" "~no_such_user_zq9"

(* HOME = <R>/n: the home nested inside <R>, so the longest entry must win. *)
let nested () =
  line "home_dir" "" (str (norm G.home_dir));
  line "gethomedir <N>" "" (guard (fun () -> str (norm (G.gethomedir me))));
  collapse "<R>/n/x (inside both: the longer wins)" (real ^ "/n/x");
  collapse "<R>/m (inside <R> only)" (real ^ "/m")

let run stage home =
  let env = Array.append [| "HOME=" ^ home; "TEST_GLOB_TILDE_CHILD=" ^ stage |]
      (Array.of_list (List.filter (fun e -> not (String.starts_with ~prefix:"HOME=" e)) (Array.to_list (Unix.environment ())))) in
  let pid = Unix.create_process_env Sys.executable_name [| Sys.executable_name |] env Unix.stdin Unix.stdout Unix.stderr in
  match Unix.waitpid [] pid with
  | _, Unix.WEXITED 0 -> ()
  | _ -> print_endline ("child " ^ stage ^ " failed")

let () =
  match Sys.getenv_opt "TEST_GLOB_TILDE_CHILD" with
  | Some "1" -> child ()
  | Some _ -> nested ()
  | None ->
      run "1" (real ^ "-c");
      print_endline "-- HOME = <R>/n";
      run "2" (real ^ "/n")
