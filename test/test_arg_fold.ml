(* Lm_arg's folding forms against its non-folding ones (which test_arg
   pins): for random argv vectors, in both spec modes, fold_argv with
   UnitFold, SetFold, ClearFold, StringFold, IntFold, FloatFold and
   RestFold must log the same effects, in the same order, with the same
   anonymous arguments and the same exception, as parse_argv with Unit,
   Set, Clear, String, Int, Float and Rest. Per mode: how many vectors
   differ, and the first. --help is left out (it prints the usage).
   Then the anonymous function's "rest" answer (true sends every later
   argument to it, options included), and fold and parse on this
   program's own argv (no arguments). *)
open Pin
module A = Lm_arg

let plain mode argv =
  let log = ref [] in
  let say s = log := s :: !log in
  let v = ref false and q = ref true in
  let spec = [
    "-v", A.Set v, "verbose";
    "-q", A.Clear q, "quiet";
    "-o", A.String (fun s -> say ("o=" ^ s)), "output";
    "-n", A.Int (fun i -> say ("n=" ^ string_of_int i)), "count";
    "-x", A.Float (fun f -> say (Printf.sprintf "x=%g" f)), "scale";
    "-u", A.Unit (fun () -> say "u"), "unit";
    "--long", A.Unit (fun () -> say "long"), "a long option";
    "--", A.Rest (fun s -> say ("rest=" ^ s)), "the rest";
  ] in
  (* Set and Clear are logged when they happen, to compare with the fold *)
  let spec = List.map (fun (n, s, d) -> match s with
      | A.Set r -> n, A.Unit (fun () -> r := true; say "set v"), d
      | A.Clear r -> n, A.Unit (fun () -> r := false; say "clear q"), d
      | s -> n, s, d) spec in
  let r = guard (fun () ->
      A.parse_argv (Array.of_list ("prog" :: argv)) (mode, [ "Options", spec ]) (fun s -> say ("anon=" ^ s)) "usage";
      "ok") in
  r ^ " [" ^ String.concat " " (List.rev !log) ^ "]"

let folded mode argv =
  let say acc s = s :: acc in
  let spec = [
    "-v", A.SetFold (fun acc b -> say acc (if b then "set v" else "?")), "verbose";
    "-q", A.ClearFold (fun acc b -> say acc (if b then "?" else "clear q")), "quiet";
    "-o", A.StringFold (fun acc s -> say acc ("o=" ^ s)), "output";
    "-n", A.IntFold (fun acc i -> say acc ("n=" ^ string_of_int i)), "count";
    "-x", A.FloatFold (fun acc f -> say acc (Printf.sprintf "x=%g" f)), "scale";
    "-u", A.UnitFold (fun acc -> say acc "u"), "unit";
    "--long", A.UnitFold (fun acc -> say acc "long"), "a long option";
    "--", A.RestFold (fun acc s -> say acc ("rest=" ^ s)), "the rest";
  ] in
  let last = ref [] in
  let r = guard (fun () ->
      last := A.fold_argv (Array.of_list ("prog" :: argv)) (mode, [ "Options", spec ]) []
          (fun acc s -> say acc ("anon=" ^ s), false) "usage";
      "ok") in
  r ^ " [" ^ String.concat " " (List.rev !last) ^ "]"

let seed = ref 89
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 12) mod n
let tokens = [| "a"; "b"; "-v"; "-q"; "-o"; "f"; "-n"; "42"; "-7"; "x"; "-x"; "1.5"; "-u"; "--long"; "--lo";
                "--"; "-vq"; "-qv"; "-of"; "-n3"; "-vu"; "-z"; "-" |]

let () =
  List.iter (fun (name, mode) ->
      seed := 89;
      let bad = ref 0 and first = ref "" in
      for _ = 1 to 2000 do
        let argv = List.init (rnd 6) (fun _ -> tokens.(rnd (Array.length tokens))) in
        let p = plain mode argv and f = folded mode argv in
        (* an exception after some effects: the fold's log is lost with it, so compare the exception only *)
        let same = p = f || (String.length p > 4 && String.sub p 0 4 = "EXN " && String.sub p 0 (String.index p '[') = String.sub f 0 (String.index f '[')) in
        if not same then begin incr bad; if !first = "" then first := Printf.sprintf "%s: parse %s, fold %s" (list str argv) p f end
      done;
      line ("fold_argv (Fold specs) against parse_argv: vectors that differ, " ^ name) "2000 vectors"
        (Printf.sprintf "%d%s" !bad (if !bad = 0 then "" else " (first: " ^ !first ^ ")")))
    [ "strict", A.StrictOptions; "multi", A.MultiLetterOptions ];
  (* the anonymous function asks for the rest *)
  let r = A.fold_argv [| "prog"; "-u"; "a"; "stop"; "-u"; "b" |] (A.StrictOptions, [ "O", [ "-u", A.UnitFold (fun acc -> "u" :: acc), "" ] ]) []
      (fun acc s -> ("anon=" ^ s) :: acc, s = "stop") "usage" in
  line "fold_argv: anonymous \"stop\" asks for the rest" (list str [ "-u"; "a"; "stop"; "-u"; "b" ]) (list str (List.rev r));
  (* this program has no arguments *)
  line "fold on Sys.argv (no arguments)" "" (guard (fun () ->
      string_of_int (A.fold (A.StrictOptions, [ "O", [ "-u", A.UnitFold (fun n -> n + 1), "" ] ]) 7 (fun n _ -> n + 100, false) "usage")));
  line "parse on Sys.argv (no arguments)" "" (guard (fun () ->
      let n = ref 0 in A.parse (A.StrictOptions, [ "O", [ "-u", A.Unit (fun () -> incr n), "" ] ]) (fun _ -> n := 99) "usage"; string_of_int !n))
