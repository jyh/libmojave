(* Lm_ncurses_display with no display open (it was 0 % covered; row 357).
   Its interface promises that, until start_display is called, display_active
   is false, stop_display is a no-op, and every function that needs the
   display raises Failure. Nothing here opens a display: start_display calls
   initscr, which ends the process when there is no terminal, so the open
   path (start_display, in_display, text_viewer, menu_select) is NOT covered
   here; it needs a pseudo-terminal. curses_enabled is not printed: it
   depends on how the library was configured. *)
open Lm_ncurses_display

let try_ name f =
  match f () with
  | () -> Printf.printf "%s: returned\n" name
  | exception Failure msg -> Printf.printf "%s: Failure %S\n" name msg
  | exception e -> Printf.printf "%s: raised %s\n" name (Printexc.to_string e)

let () =
  Printf.printf "display_active () = %b\n" (display_active ());
  try_ "update_status" (fun () -> update_status "status");
  try_ "putchar" (fun () -> putchar 'a');
  try_ "putsubstr" (fun () -> putsubstr "abc" 0 2);
  try_ "getchar" (fun () -> ignore (getchar ()));
  try_ "flush" (fun () -> flush ());
  try_ "stop_display" (fun () -> stop_display ());
  try_ "stop_display again" (fun () -> stop_display ());
  Printf.printf "display_active () = %b\n" (display_active ());
  let fmt = make_formatter () in
  try_ "make_formatter, print and flush"
    (fun () -> Lm_printf.fprintf fmt "hello@.");
  Printf.printf "display_active () = %b\n" (display_active ())
