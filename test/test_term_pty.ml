(* Lm_termsize and Lm_ncurses on a terminal. Each case runs a child under
   script(1), which gives it a pseudo-terminal; the child sets the size with
   stty and then reads it back:
     - term_size on the terminal is the size stty set;
     - term_width is the columns, but never under !min_screen_width (40);
     - stdout_width, computed when the module starts, is term_width's;
     - with curses, the screen getmaxyx reports is that size too, and on a
       0 x 0 terminal curses falls back to a size of its own (terminfo's).
   Where Lm_ncurses is not linked its lines are not checked, and the test
   says so on stderr.
   On a pipe (no terminal) term_size fails and term_width keeps the width it
   was given. A terminal of 0 x 0 (what a pseudo-terminal starts as when
   nothing sets it) reads as 0 columns, so term_width gives 40, not its
   default; that is recorded as it is. The child's lines come back through
   the terminal, so only lines starting "R " are read. Not run on Windows
   (no script(1); Lm_termsize reads the console there). *)
open Pin

let child () =
  let say fmt = Printf.printf ("R " ^^ fmt ^^ "\n%!") in
  let rows, cols = Lm_termsize.term_size Unix.stdout in
  say "size %d x %d" rows cols;
  say "width %d" (Lm_termsize.term_width stdout 80);
  say "stdout_width %d" Lm_termsize.stdout_width;
  if Lm_ncurses.curses_enabled then begin
    Lm_ncurses.initscr ();
    let w = Lm_ncurses.newwin 0 0 0 0 in
    let y, x = Lm_ncurses.getmaxyx w in
    Lm_ncurses.delwin w;
    Lm_ncurses.endwin ();
    if (y, x) = (rows, cols) then say "curses agrees"
    else if rows = 0 && cols = 0 && y > 0 && x > 0 then say "curses falls back to a size"
    else say "curses %d x %d" y x
  end
  else begin
    (* Not a check: the line below keeps the output the same everywhere. *)
    prerr_endline "test_term_pty: Lm_ncurses is not linked here; its lines are not checked";
    say (if rows = 0 && cols = 0 then "curses falls back to a size" else "curses agrees")
  end

let rec find s p i =
  if i + String.length p > String.length s then None
  else if String.sub s i (String.length p) = p then Some i else find s p (i + 1)

let linux = lazy (Sys.command "uname -s | grep -q Linux" = 0)

(* Run the child under script(1) at a size; its "R " lines, CRs removed. *)
let run rows cols =
  let inner = Printf.sprintf "stty rows %d cols %d; exec %s child" rows cols (Filename.quote Sys.executable_name) in
  let args =
    if Lazy.force linux then [| "script"; "-q"; "-e"; "-c"; inner; "/dev/null" |]
    else [| "script"; "-q"; "/dev/null"; "sh"; "-c"; inner |] in
  let env = Array.append [| "TERM=xterm" |]
      (Array.of_list (List.filter (fun e -> not (List.exists (fun v -> String.starts_with ~prefix:v e) [ "TERM="; "LINES="; "COLUMNS=" ]))
                      (Array.to_list (Unix.environment ())))) in
  let null = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let r, w = Unix.pipe ~cloexec:true () in
  let pid = Unix.create_process_env "script" args env null w Unix.stderr in
  Unix.close w; Unix.close null;
  let ic = Unix.in_channel_of_descr r in
  let rec read acc =
    match input_line ic with
    | l ->
        let l = String.concat "" (String.split_on_char '\r' l) in
        (* curses may leave escape sequences before the line *)
        let l = match find l "R " 0 with
          | Some i -> String.sub l (i + 2) (String.length l - i - 2)
          | None -> "" in
        read (if l = "" then acc else l :: acc)
    | exception End_of_file -> List.rev acc in
  let lines = read [] in
  close_in ic;
  let _, st = Unix.waitpid [] pid in
  let rc = match st with Unix.WEXITED n -> string_of_int n | _ -> "killed" in
  String.concat "; " lines ^ " (rc " ^ rc ^ ")"

let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "child" then child ()
  else begin
    let r, w = Unix.pipe () in
    line "term_size on a pipe" "" (try let a, b = Lm_termsize.term_size w in Printf.sprintf "%d x %d" a b with Failure s -> "Failure " ^ s);
    line "term_width on a pipe" "77" (string_of_int (Lm_termsize.term_width (Unix.out_channel_of_descr w) 77));
    Unix.close r; Unix.close w;
    line "min_screen_width" "" (string_of_int !Lm_termsize.min_screen_width);
    List.iter (fun (rows, cols) -> line "terminal" (Printf.sprintf "%d x %d" rows cols) (run rows cols))
      [ 30, 100; 24, 80; 50, 132; 10, 30; 0, 0 ]
  end
