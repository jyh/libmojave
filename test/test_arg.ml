(* Lm_arg pinned on argv vectors under both spec modes: every effect of
   the spec (in order), the anonymous arguments, and the exception if one
   is raised. *)
open Pin
module A = Lm_arg

let run mode argv =
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
  let r = guard (fun () ->
      A.parse_argv (Array.of_list ("prog" :: argv)) (mode, [ "Options", spec ]) (fun s -> say ("anon=" ^ s)) "usage";
      "ok") in
  Printf.sprintf "%s v=%b q=%b [%s]" r !v !q (String.concat " " (List.rev !log))

let vectors = [
  []; [ "a" ]; [ "-v" ]; [ "-v"; "-q" ]; [ "-vq" ]; [ "-qv"; "a" ]; [ "-o"; "f" ]; [ "-of" ]; [ "-o" ];
  [ "-n"; "42" ]; [ "-n"; "-3" ]; [ "-n"; "x" ]; [ "-n42" ]; [ "-x"; "1.5" ]; [ "-x"; "e" ]; [ "-u"; "a"; "-u" ];
  [ "--long" ]; [ "--lo" ]; [ "-z" ]; [ "a"; "--"; "-v"; "b" ]; [ "--"; "--" ]; [ "-" ]; [ "-vu" ]; [ "-vo"; "f" ]; [ "-ov" ];
]

let () =
  List.iter (fun (name, mode) ->
      List.iter (fun argv -> line name (list str argv) (run mode argv)) vectors)
    [ ("strict", A.StrictOptions); ("multi", A.MultiLetterOptions) ]
