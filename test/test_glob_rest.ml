(* The rest of Lm_glob, with output that is the same on every platform
   (separators shown as /, results sorted; no line prints a home
   directory or a user name).
   Model: regex_of_shell_pattern p, matched with LmStr.string_match at
   position 0, against a brute-force shell matcher, on random patterns
   over a b . * ? [ab] [!ab] [a-c] \* and random names over a b c . ! -:
   * matches any string, ? any character, [...] any listed character or
   range, [!...] any other, \x the character x. Per check: how many
   (pattern, name) pairs differ, and the first.
   Pins, over a fixture tree: glob_argv (argument order kept), list_dirs,
   list_dirs_rec, subdirs_of_dirs, and options (GlobDot, GlobOnlyFiles,
   GlobOnlyDirs, GlobIgnore, GlobAllow, GlobNoCheck). Then tilde_collapse
   of home_dir paths, and gethomedir of an unknown user. *)
open Pin
module G = Lm_glob

let seed = ref 101
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 12) mod n

type atom = Star | Any | Lit of char | Set of bool * (char * char) list

let atoms () =
  List.init (rnd 5) (fun _ ->
      match rnd 9 with
      | 0 | 1 -> Star
      | 2 -> Any
      | 3 -> Set (false, [ 'a', 'a'; 'b', 'b' ])
      | 4 -> Set (true, [ 'a', 'a'; 'b', 'b' ])
      | 5 -> Set (false, [ 'a', 'c' ])
      | 6 -> Lit '*'
      | _ -> Lit "ab.".[rnd 3])
let render l = String.concat "" (List.map (function
    | Star -> "*" | Any -> "?" | Lit '*' -> "\\*" | Lit c -> String.make 1 c
    | Set (neg, rs) -> "[" ^ (if neg then "!" else "") ^ String.concat "" (List.map (fun (a, b) ->
        if a = b then String.make 1 a else Printf.sprintf "%c-%c" a b) rs) ^ "]") l)
let rec matches pat s i =
  match pat with
  | [] -> i = String.length s
  | Star :: rest -> let rec any j = j <= String.length s && (matches rest s j || any (j + 1)) in any i
  | _ when i >= String.length s -> false
  | Any :: rest -> matches rest s (i + 1)
  | Lit c :: rest -> s.[i] = c && matches rest s (i + 1)
  | Set (neg, rs) :: rest ->
      let inn = List.exists (fun (a, b) -> s.[i] >= a && s.[i] <= b) rs in
      inn <> neg && matches rest s (i + 1)

let o = G.default_glob_options
let norm s = String.map (fun c -> if c = '\\' then '/' else c) s
let sorted l = List.sort compare (List.map norm l)

let () =
  seed := 101;
  let bad = ref 0 and first = ref "" and n = 4000 in
  for _ = 1 to n do
    let pat = atoms () in
    let name = String.init (rnd 5) (fun _ -> "abc.!-".[rnd 6]) in
    let p = render pat in
    let got = (try string_of_bool (Lm_lexer.LmStr.string_match (G.regex_of_shell_pattern o p) name 0)
               with e -> "EXN " ^ Printexc.to_string e) in
    let want = string_of_bool (matches pat name 0) in
    if got <> want then begin incr bad; if !first = "" then first := Printf.sprintf "%S on %S: %s, want %s" p name got want end
  done;
  line "regex_of_shell_pattern against a shell matcher: pairs that differ" (string_of_int n ^ " pairs")
    (Printf.sprintf "%d%s" !bad (if !bad = 0 then "" else " (first: " ^ !first ^ ")"));
  (* fixture *)
  let mk d = try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> () in
  let touch f = close_out (open_out f) in
  mk "grx"; mk "grx/sub"; mk "grx/sub/deep"; mk "grx/.hid";
  List.iter touch [ "grx/a.ml"; "grx/b.ml"; "grx/c.mli"; "grx/.h.ml"; "grx/sub/d.ml"; "grx/sub/deep/e.ml" ];
  let pair (d, f) = list str (sorted d) ^ " " ^ list str (sorted f) in
  line "glob_argv [grx/b.ml; grx/a*; plain; grx/*.mli]" "" (guard (fun () ->
      list str (List.map norm (G.glob_argv o "." [ "grx/b.ml"; "grx/a*"; "plain"; "grx/*.mli" ]))));
  line "list_dirs . [grx]" "" (guard (fun () -> pair (G.list_dirs o "." [ "grx" ])));
  line "list_dirs_rec . [grx]" "" (guard (fun () -> pair (G.list_dirs_rec o "." [ "grx" ])));
  line "subdirs_of_dirs . [grx]" "" (guard (fun () -> list str (sorted (G.subdirs_of_dirs o "." [ "grx" ]))));
  let with_opts name opts pat =
    line ("glob with " ^ name) (str pat) (guard (fun () -> pair (G.glob (G.create_options opts) "." [ pat ]))) in
  line "glob grx/[!a]*.ml (every .ml but a.ml)" "" (guard (fun () -> pair (G.glob o "." [ "grx/[!a]*.ml" ])));
  with_opts "GlobDot" [ G.GlobDot ] "grx/*";
  with_opts "GlobOnlyFiles" [ G.GlobOnlyFiles ] "grx/*";
  with_opts "GlobOnlyDirs" [ G.GlobOnlyDirs ] "grx/*";
  with_opts "GlobIgnore [*.mli]" [ G.GlobIgnore [ "*.mli" ] ] "grx/*";
  with_opts "GlobAllow [a*; c*]" [ G.GlobAllow [ "a*"; "c*" ] ] "grx/*";
  with_opts "GlobNoCheck" [ G.GlobNoCheck ] "grx/none*";
  with_opts "GlobIgnoreCheck" [ G.GlobIgnoreCheck ] "grx/none*";
  with_opts "(default)" [] "grx/none*";
  (* home directory *)
  line "tilde_collapse (home_dir), (home_dir/x/y), \"/elsewhere/x\"" "" (guard (fun () ->
      String.concat " " (List.map (fun s -> str (norm (G.tilde_collapse s)))
                           [ G.home_dir; Filename.concat G.home_dir "x/y"; "/elsewhere/x" ])));
  line "gethomedir of a user that does not exist" "" (guard (fun () -> ignore (G.gethomedir "no-such-user-zq"); "a directory"))
