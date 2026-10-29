(* Lm_rprintf.sprintf and Lm_format.sprintf (both built on
   Lm_make_printf, which parses the format and calls C's snprintf)
   against Stdlib.Printf.sprintf, from a fixed LCG. Each case is a
   random conversion: flags from - 0 + space, a width 0-8 or none, a
   precision 0-6 or none (not for s and c), and a conversion from d i u x X o (ints),
   f e E g G (floats), s and c, on a random value. The spec is typed
   with Scanf.format_from_string against the conversion's template;
   a spec Stdlib refuses is not a case. One line per conversion. *)
open Pin

let seed = ref 102
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n

(* Only specs whose meaning OCaml and C agree on: no # (OCaml's puts
   underscores in an int); no precision on s or c (OCaml ignores it, C
   truncates); only - among the flags of s and c (0 is undefined in C);
   no width on c (OCaml's compiler refuses it); no precision 0 on an int
   (C prints no digit for 0, OCaml prints "0"). *)
let spec conv =
  let flags = String.concat "" (List.filter (fun f -> rnd 4 = 0 && not ((conv = "s" || conv = "c") && f <> "-")) [ "-"; "0"; "+"; " " ]) in
  let width = if rnd 3 = 0 || conv = "c" then "" else string_of_int (rnd 9) in
  let is_int = String.contains "diuxXo" conv.[0] in
  let prec = if rnd 2 = 0 || conv = "s" || conv = "c" then "" else "." ^ string_of_int ((if is_int then 1 else 0) + rnd 7) in
  "%" ^ flags ^ width ^ prec ^ conv

let ints = [| 0; 1; -1; 7; -42; 255; 65535; -65536; max_int; min_int; 123456789 |]
let floats = [| 0.0; -0.0; 1.0; -1.5; 3.14159; 1e-7; 2.5e10; 123456.789; -0.000123; 1e300 |]
let strings = [| ""; "a"; "hello"; "two words"; "tab\there" |]

let bad = Hashtbl.create 8 and first = Hashtbl.create 8 and count = Hashtbl.create 8 and order = ref []
let check op ok d =
  if not (List.mem op !order) then order := op :: !order;
  Hashtbl.replace count op (1 + try Hashtbl.find count op with Not_found -> 0);
  if not ok then begin
    Hashtbl.replace bad op (1 + try Hashtbl.find bad op with Not_found -> 0);
    if not (Hashtbl.mem first op) then Hashtbl.replace first op (d ())
  end

let guard f = try f () with e -> "EXN " ^ Printexc.to_string e

let () =
  for _ = 1 to 6000 do
    let conv = [| "d"; "i"; "u"; "x"; "X"; "o"; "f"; "e"; "E"; "g"; "G"; "s"; "c" |].(rnd 13) in
    let s = spec conv in
    let compare_with name want lm_r lm_f =
      check (name ^ " Lm_rprintf") (want = lm_r) (fun () -> Printf.sprintf "%s: Printf %S, Lm_rprintf %S" s want lm_r);
      check (name ^ " Lm_format") (want = lm_f) (fun () -> Printf.sprintf "%s: Printf %S, Lm_format %S" s want lm_f) in
    match conv with
    | "d" | "i" | "u" | "x" | "X" | "o" ->
        (match Scanf.format_from_string s "%d" with
         | exception _ -> ()
         | fmt ->
             let v = ints.(rnd (Array.length ints)) in
             compare_with ("%" ^ conv) (Printf.sprintf fmt v)
               (guard (fun () -> Lm_rprintf.sprintf fmt v)) (guard (fun () -> Lm_format.sprintf fmt v)))
    | "f" | "e" | "E" | "g" | "G" ->
        (match Scanf.format_from_string s "%f" with
         | exception _ -> ()
         | fmt ->
             let v = floats.(rnd (Array.length floats)) in
             compare_with ("%" ^ conv) (Printf.sprintf fmt v)
               (guard (fun () -> Lm_rprintf.sprintf fmt v)) (guard (fun () -> Lm_format.sprintf fmt v)))
    | "s" ->
        (match Scanf.format_from_string s "%s" with
         | exception _ -> ()
         | fmt ->
             let v = strings.(rnd (Array.length strings)) in
             compare_with "%s" (Printf.sprintf fmt v)
               (guard (fun () -> Lm_rprintf.sprintf fmt v)) (guard (fun () -> Lm_format.sprintf fmt v)))
    | _ ->
        (match Scanf.format_from_string s "%c" with
         | exception _ -> ()
         | fmt ->
             let v = "aZ 9".[rnd 4] in
             compare_with "%c" (Printf.sprintf fmt v)
               (guard (fun () -> Lm_rprintf.sprintf fmt v)) (guard (fun () -> Lm_format.sprintf fmt v)))
  done;
  List.iter (fun op ->
      let k = try Hashtbl.find bad op with Not_found -> 0 in
      line op (string_of_int (Hashtbl.find count op) ^ " cases")
        (if k = 0 then "0 disagree" else Printf.sprintf "%d disagree (first: %s)" k (Hashtbl.find first op)))
    (List.sort compare !order)
