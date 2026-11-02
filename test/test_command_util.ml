(* Lm_command_util, the read-eval-print helpers (util/lm_command_util),
   which no test reached.  The pure helpers are printed directly.  The
   loop runs in a child whose stdin is a pipe (as test_readline_long; the
   child's TERM is "dumb", so prompts carry no terminal escapes): a command
   table with "echo" (prints the arguments it was given) and "quit", the
   arguments split by space_filter, fed these lines:
     "echo a b", "  echo a b" (leading spaces), "<TAB>echo c" (a leading
     tab), "help echo", "nope x", "", "echo", "quit".
   A second child gets only end of file, which runs ri_cmd_quit. *)
open Pin
open Lm_command_util

let show l = "[" ^ String.concat "; " (List.map (Printf.sprintf "%S") l) ^ "]"

let child_loop () =
  let commands = [
    "echo", (fun args -> Printf.printf "echo got %s\n%!" (show args); Continue), "print the arguments";
    "quit", (fun _ -> Exit "quit"), "leave" ] in
  let run = { ri_appname = "t"; ri_curses = false; ri_hack_title = false; ri_help_prefix = "Test commands.";
              ri_prompt = (fun () -> "p> "); ri_commands = commands; ri_cmd_quit = (fun _ -> Exit "eof");
              ri_arg_filter = space_filter } in
  let r = run_loop read_eval_print run in
  Format.printf "@.";
  Printf.printf "run_loop returned %S\n%!" r

let feed input =
  let env = Array.append (Array.of_list (List.filter (fun s -> not (String.length s >= 5 && String.sub s 0 5 = "TERM=")) (Array.to_list (Unix.environment ())))) [| "TERM=dumb" |] in
  let r, w = Unix.pipe ~cloexec:true () in
  let pid = Unix.create_process_env Sys.executable_name [| Sys.executable_name; "child" |] env r Unix.stdout Unix.stderr in
  Unix.close r;
  let k = Unix.write_substring w input 0 (String.length input) in
  assert (k = String.length input);
  Unix.close w;
  match Unix.waitpid [] pid with
  | _, Unix.WEXITED 0 -> ()
  | _ -> print_endline "child failed"

let parent () =
  line "get_next_argument" "[a; b]" (let a, rest = get_next_argument [ "a"; "b" ] in a ^ " / " ^ show rest);
  line "get_next_argument" "[]" (let a, rest = get_next_argument [] in Printf.sprintf "%S / %s" a (show rest));
  line "reconstruct_arguments" "[a; \"b c\"; \"\"]" (Printf.sprintf "%S" (reconstruct_arguments [ "a"; "b c"; "" ]));
  line "reconstruct_arguments" "[]" (Printf.sprintf "%S" (reconstruct_arguments []));
  line "space_filter" "\"  a\\tb  c\\n\"" (show (space_filter "  a\tb  c\n"));
  line "space_filter" "\"\"" (show (space_filter ""));
  line "no_filter" "\" x \"" (Printf.sprintf "%S" (no_filter " x "));
  let hit = ref 0 in
  line "continue" "f 3" (match continue (fun n -> hit := n) 3 with Continue -> Printf.sprintf "Continue, f got %d" !hit | Exit () -> "Exit");
  flush stdout;
  print_endline "-- loop";
  feed "echo a b\n  echo a b\n\techo c\nhelp echo\nnope x\n\necho\nquit\n";
  print_endline "-- end of file";
  feed ""

let () = if Array.length Sys.argv > 1 && Sys.argv.(1) = "child" then child_loop () else parent ()
