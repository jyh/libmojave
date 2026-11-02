(* Lm_glob options no other test uses, over a fixture tree: GlobNoBraces,
   GlobNoEscape, GlobNoTilde with GlobHomeDir, GlobIgnoreFun,
   GlobAllowFun, GlobCVSIgnore and GlobProperSubdirs. Each line prints
   the library's answer and, where one is cheap, a model written from the
   option's comment in lm_glob.mli: "ok" when they agree, else both.
   Paths print relative to the fixture (<F>), sorted. Unix only: the
   fixture has file names Windows cannot hold (a backslash). *)
open Pin
module G = Lm_glob

let fx =
  let d = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "lm_glob_options_%d" (Unix.getpid ())) in
  Unix.mkdir d 0o755; d

let mkdir p = Unix.mkdir (Filename.concat fx p) 0o755
let touch p = close_out (open_out (Filename.concat fx p))
let write p s = let oc = open_out (Filename.concat fx p) in output_string oc s; close_out oc

let () =
  List.iter mkdir [ "home"; "sub"; "sub/d"; "sub/e" ];
  List.iter touch
    [ "a.ml"; "b.ml"; "c.o"; "xay"; "xby"; "x{a,b}y"; "a\\b"; "~"; "home/h1"; "home/h2.txt";
      "sub/k.tmp"; "sub/k.ml"; "sub/core"; "sub/corefile"; "sub/keep"; "sub/it's"; "sub/d/f" ];
  (* user patterns: a glob, a plain name, and a name that is a prefix of another file *)
  write "sub/.cvsignore" "*.tmp core\nit's\n"

let rel s =
  let n = String.length fx in
  if String.length s >= n && String.sub s 0 n = fx then "<F>" ^ String.sub s n (String.length s - n) else s
let show (dirs, files) =
  let f l = list str (List.sort compare (List.map rel l)) in
  "dirs " ^ f dirs ^ " files " ^ f files
let verdict got want = if got = want then "ok " ^ got else "got " ^ got ^ " MODEL " ^ want

let glob opts root pats = guard (fun () -> show (G.glob (G.create_options opts) root pats))
let model dirs files = show (dirs, files)

let () =
  line "braces" "x{a,b}y" (verdict (glob [] fx [ "x{a,b}y" ]) (model [] [ "xay"; "xby" ]));
  line "GlobNoBraces" "x{a,b}y" (verdict (glob [ G.GlobNoBraces ] fx [ "x{a,b}y" ]) (model [] [ "x{a,b}y" ]));
  line "escape" "a\\\\* (a literal a*: no such file)" (glob [ G.GlobIgnoreCheck ] fx [ "a\\*" ]);
  (* a backslash that escapes nothing separates path components, as on
     Windows, so with GlobNoEscape it always does, and a name holding a
     backslash cannot be globbed (recorded) *)
  line "GlobNoEscape" "sub\\\\k*" (verdict (glob [ G.GlobNoEscape ] fx [ "sub\\k*" ]) (model [] [ "sub/k.ml"; "sub/k.tmp"; "sub/keep" ]));
  line "GlobNoEscape" "a\\\\* (= a/*)" (glob [ G.GlobNoEscape ] fx [ "a\\*" ]);
  line "escape" "a\\\\b (= a/b)" (glob [ G.GlobNoCheck ] fx [ "a\\b" ]);
  let home = Filename.concat fx "home" in
  line "GlobHomeDir" "~/h*" (verdict (glob [ G.GlobHomeDir home ] fx [ "~/h*" ])
                               (model [] [ Filename.concat home "h1"; Filename.concat home "h2.txt" ]));
  line "GlobHomeDir" "~" (verdict (glob [ G.GlobHomeDir home ] fx [ "~" ]) (model [ home ] []));
  line "GlobNoTilde" "~" (verdict (glob [ G.GlobNoTilde; G.GlobHomeDir home ] fx [ "~" ]) (model [] [ "~" ]));
  line "escaped tilde" "\\\\~" (verdict (glob [ G.GlobHomeDir home ] fx [ "\\~" ]) (model [] [ "~" ]));
  let starts_a n = n <> "" && n.[0] = 'a' in
  line "GlobIgnoreFun" "*.ml, ignore a*" (verdict (glob [ G.GlobIgnoreFun starts_a ] fx [ "*.ml" ]) (model [] [ "b.ml" ]));
  line "GlobAllowFun" "*.ml, allow a*" (verdict (glob [ G.GlobAllowFun starts_a ] fx [ "*.ml" ]) (model [] [ "a.ml" ]));
  (* .cvsignore: the user's patterns, CVS's defaults ( *.o ... ) and the
     file itself are ignored; a pattern matches a whole name *)
  let want = model [ "sub/d"; "sub/e" ] [ "sub/corefile"; "sub/k.ml"; "sub/keep" ] in
  line "GlobCVSIgnore" "sub/* (root = the fixture)" (verdict (glob [ G.GlobCVSIgnore ] fx [ "sub/*" ]) want);
  let here = Sys.getcwd () in
  Sys.chdir fx;
  line "GlobCVSIgnore" "sub/* (cwd = the fixture, root = .)" (verdict (glob [ G.GlobCVSIgnore ] "." [ "sub/*" ]) want);
  Sys.chdir here;
  line "no GlobCVSIgnore" "sub/*" (glob [] fx [ "sub/*" ]);
  let subdirs opts = guard (fun () -> list str (List.sort compare (G.subdirs_of_dirs (G.create_options opts) fx [ "sub" ]))) in
  line "subdirs_of_dirs" "sub" (verdict (subdirs []) (list str [ "sub"; "sub/d"; "sub/e" ]));
  line "GlobProperSubdirs" "subdirs_of_dirs sub" (verdict (subdirs [ G.GlobProperSubdirs ]) (list str [ "sub/d"; "sub/e" ]))

(* Remove the fixture. *)
let () =
  let rec rm p =
    if Sys.is_directory p then (Array.iter (fun n -> rm (Filename.concat p n)) (Sys.readdir p); Unix.rmdir p)
    else Sys.remove p in
  rm fx
