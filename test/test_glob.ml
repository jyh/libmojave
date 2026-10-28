open Pin
module G = Lm_glob

let o = G.default_glob_options

let () =
  List.iter (fun s -> line "is_glob_string" (str s) (guard (fun () -> string_of_bool (G.is_glob_string o s))))
    [ "abc"; "*.ml"; "a?c"; "[ab]c"; "{a,b}"; "a\\*b"; "~"; "" ];
  List.iter (fun s -> line "glob_add_escaped" (str s)
               (guard (fun () -> let b = Buffer.create 8 in G.glob_add_escaped o b s; str (Buffer.contents b))))
    [ "abc"; "*?[]"; "a b"; "\\" ];
  (* glob over a fixture tree built in the test's own working directory *)
  let mk d = try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> () in
  let touch f = close_out (open_out f) in
  mk "gfx"; mk "gfx/sub"; List.iter touch [ "gfx/a.ml"; "gfx/b.ml"; "gfx/c.mli"; "gfx/.h.ml"; "gfx/sub/d.ml" ];
  let sorted l = List.sort compare l in
  List.iter (fun pat -> line "glob" (str pat)
               (guard (fun () -> let dirs, files = G.glob o "." [ pat ] in
                                 list str (sorted dirs) ^ " " ^ list str (sorted files))))
    [ "gfx/*.ml"; "gfx/*"; "gfx/?.ml*"; "gfx/[ab].ml"; "gfx/{a,c}.*"; "gfx/*/d.ml"; "gfx/none*"; "gfx/a.ml" ]
