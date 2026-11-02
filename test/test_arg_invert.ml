(* Lm_arg's inverted options and its usage text (the 2026-11-02 coverage
   run reached neither). "--no-x" looks up "-x" (an unambiguous prefix
   too) and runs a Set as a Clear, a Clear as a Set, and likewise the fold
   forms; any other kind is "Not an invertable option", an unknown name
   "No such option". Each line runs parse_argv (or fold_argv) on a fresh
   state and prints the flags, or the exception. Then usage, whose
   invertable options carry "*". *)
open Pin
module A = Lm_arg

let a = ref false and b = ref false and g = ref ""
let spec =
  A.StrictOptions,
  [ "Flags",
    [ "-alpha", A.Set a, "set alpha";
      "-beta", A.Clear b, "clear beta";
      "-gamma", A.String (fun s -> g := s), "a string" ] ]

let run args =
  let res =
    try
      A.parse_argv (Array.of_list ("prog" :: args)) spec (fun s -> g := "anon:" ^ s) "usage";
      Printf.sprintf "alpha %b beta %b gamma %S" !a !b !g
    with
    | A.BogusArg s -> "BogusArg " ^ s
    | e -> "EXN " ^ Printexc.to_string e in
  line "parse_argv" (list str args) res

let () =
  List.iter (fun (start_a, start_b, args) -> a := start_a; b := start_b; g := ""; run args)
    [ (false, true, [ "-alpha" ]);
      (true, true, [ "--no-alpha" ]);
      (false, false, [ "--no-beta" ]);
      (false, true, [ "-beta" ]);
      (true, true, [ "--no-alp" ]);
      (true, true, [ "--no-alpha"; "-alpha" ]);
      (false, true, [ "--no-gamma"; "x" ]);
      (false, true, [ "--no-zeta" ]);
      (false, true, [ "--no-" ]);
      (false, true, [ "--noalpha" ]);
      (false, true, [ "-gamma"; "s"; "--no-alpha"; "rest" ]) ]

(* The fold forms. *)
let () =
  let fspec =
    A.StrictOptions,
    [ "Fold",
      [ "-on", A.SetFold (fun acc v -> Printf.sprintf "%s on=%b" acc v), "set";
        "-off", A.ClearFold (fun acc v -> Printf.sprintf "%s off=%b" acc v), "clear" ] ] in
  List.iter (fun args ->
      let r = (try A.fold_argv (Array.of_list ("prog" :: args)) fspec "" (fun acc s -> acc ^ " anon:" ^ s, false) "usage"
               with A.BogusArg s -> "BogusArg " ^ s | e -> "EXN " ^ Printexc.to_string e) in
      line "fold_argv" (list str args) (str r))
    [ [ "-on" ]; [ "--no-on" ]; [ "-off" ]; [ "--no-off" ]; [ "-o" ]; [ "--no-o" ] ]

(* Usage text. *)
let () =
  Printf.printf "usage:\n%!";
  A.usage spec "Usage: prog [options]";
  Lm_printf.flush Lm_printf.stdout
