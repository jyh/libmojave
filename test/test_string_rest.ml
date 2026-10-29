(* The rest of Lm_string_util (what test_string_util and test_string_model
   do not call). Models first, from a fixed LCG over an alphabet of
   letters, space, tab, newline, both quotes, backslash, % and a few bytes
   above 127:
   - parse_args inverts concat_argv on lists of non-empty strings, and
     string_argv [s] and quote_string s on one (the interface: a string
     with white space or inner quotes is quoted and its inner quotes
     escaped). Strings here hold no backslash: the quoting functions copy
     a backslash as an escape already written (\x stays \x), which the
     parser then reads as one, so a backslash does not round-trip by
     design; a pin line shows it. Nor is a string wrapped in double
     quotes ("..."), which the interface passes through as already
     quoted. string_argv and quote_argv join their list into ONE
     argument;
   - decode_hex_name inverts encode_hex_name;
   - hexify_sub s off len is hexify of that substring;
   - strpat buf start len pat is the first offset, relative to start, of
     pat inside buf's start .. start+len-1, or Not_found (a naive search).
   Per check: how many cases differ, and the first. Then pins of the
   escapes, trims, MIME splitting, tokens, prefixes, file reading and the
   checked versions of make, blit and get. *)
open Pin
module U = Lm_string_util

let seed = ref 73
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 12) mod n
let alphabet = "ab  \t\n\"'\\%\xc3\xa9Z"
let no_backslash = "ab  \t\n\"'%\xc3\xa9Z"
let rword maxlen =
  let rec go () =
    let s = String.init (rnd (maxlen + 1)) (fun _ -> no_backslash.[rnd (String.length no_backslash)]) in
    let n = String.length s in
    if s = "" || (n >= 2 && s.[0] = '"' && s.[n - 1] = '"') then go () else s in
  go ()
let rstring maxlen = String.init (rnd (maxlen + 1)) (fun _ -> alphabet.[rnd (String.length alphabet)])
let rec rnonempty maxlen = let s = rstring maxlen in if s = "" then rnonempty maxlen else s

let check name n case =
  seed := 73;
  let bad = ref 0 and first = ref "" in
  for _ = 1 to n do
    match case () with
    | true, _ -> ()
    | false, d -> incr bad; if !first = "" then first := d ()
  done;
  line (name ^ ": cases that differ") (string_of_int n ^ " cases")
    (Printf.sprintf "%d%s" !bad (if !bad = 0 then "" else " (first: " ^ !first ^ ")"))

let args n f () =
  let l = List.init n (fun _ -> rword 6) in
  let s = f l in
  let back = (try U.parse_args s with e -> [ "EXN " ^ Printexc.to_string e ]) in
  back = l, (fun () -> Printf.sprintf "%s -> %S -> %s" (list str l) s (list str back))

let () =
  check "parse_args (concat_argv l) = l" 3000 (fun () -> args (1 + rnd 4) U.concat_argv ());
  check "parse_args (string_argv [s]) = [s]" 3000 (args 1 U.string_argv);
  check "parse_args (quote_string s) = [s]" 3000 (args 1 (fun l -> U.quote_string (List.hd l)));
  check "decode_hex_name (encode_hex_name s) = s" 3000 (fun () ->
      let s = String.init (rnd 12) (fun _ -> Char.chr (rnd 256)) in
      let e = U.encode_hex_name s in
      let back = (try U.decode_hex_name e with ex -> "EXN " ^ Printexc.to_string ex) in
      back = s, (fun () -> Printf.sprintf "%S -> %S -> %S" s e back));
  check "hexify_sub s off len = hexify (String.sub s off len)" 3000 (fun () ->
      let s = rstring 10 in
      let off = rnd (String.length s + 1) in
      let len = rnd (String.length s - off + 1) in
      let got = guard (fun () -> U.hexify_sub s off len) and want = U.hexify (String.sub s off len) in
      got = want, (fun () -> Printf.sprintf "%S %d %d: %S, want %S" s off len got want));
  check "strpat buf start len pat against a naive search" 3000 (fun () ->
      let buf = rstring 12 and pat = rnonempty 2 in
      let start = rnd (String.length buf + 1) in
      let len = rnd (String.length buf - start + 1) in
      let rec naive i =
        if i + String.length pat > len then None
        else if String.sub buf (start + i) (String.length pat) = pat then Some i
        else naive (i + 1) in
      let got = (try Some (U.strpat buf start len pat) with Not_found -> None) in
      let want = naive 0 in
      got = want, (fun () -> Printf.sprintf "%S %d %d %S: %s, want %s" buf start len pat
                                (opt string_of_int got) (opt string_of_int want)));
  (* pins *)
  line "concat_argv [\"a\\\\b\"; \"c d\\\\\"]; parse_args of it" "" (guard (fun () ->
      let s = U.concat_argv [ "a\\b"; "c d\\" ] in str s ^ " -> " ^ guard (fun () -> list str (U.parse_args s))));
  line "string_argv [\"a\"; \"b c\"]; quote_argv [\"a\"; \"b\"]" "" (guard (fun () -> str (U.string_argv [ "a"; "b c" ])) ^ " " ^ guard (fun () -> str (U.quote_argv [ "a"; "b" ])));
  line "parse_args \"\"; concat_argv [\"\"; \"a\"]; quote_argv [\"\"]" ""
    (guard (fun () -> list str (U.parse_args "")) ^ " " ^ guard (fun () -> str (U.concat_argv [ ""; "a" ]))
     ^ " " ^ guard (fun () -> str (U.quote_argv [ "" ])));
  List.iter (fun s -> line "parse_args" (str s) (guard (fun () -> list str (U.parse_args s))))
    [ "a b"; "  a\t b  "; "\"a b\" c"; "'a b' c"; "a\\ b"; "\"a\\\"b\""; "a\"b c\"d"; "\"unclosed" ];
  line "parse_args_list" (str "a b \\\\ c d") (guard (fun () -> list (list str) (U.parse_args_list "a b \\\\ c d")));
  List.iter (fun s ->
      line "shell_quotes / quote_string" (str s)
        (guard (fun () -> str (U.shell_quotes s)) ^ " / " ^ guard (fun () -> str (U.quote_string s))))
    [ "abc"; "a b"; "a\"b"; "a'b"; "a\\b"; "" ];
  line "unhex '0' '9' 'a' 'f' 'A' 'F'" "" (String.concat " " (List.map (fun c -> guard (fun () -> string_of_int (U.unhex c))) [ '0'; '9'; 'a'; 'f'; 'A'; 'F' ]));
  line "unhex 'g'" "" (guard (fun () -> string_of_int (U.unhex 'g')));
  line "quotes" "" (str U.quotes);
  List.iter (fun s ->
      line "mysql_escaped / html_pre_escaped / html_escaped_nonwhite" (str s)
        (String.concat " / " (List.map (fun f -> guard (fun () -> str (f s)))
                                [ U.mysql_escaped; U.html_pre_escaped; U.html_escaped_nonwhite ])))
    [ "a'b\"c\\d"; "<a href=\"x\">&amp;</a>"; " two  spaces\tand\nnewline "; "nul\000byte" ];
  line "split_mime_string \"ABC\" (the interface's example)" "" (guard (fun () ->
      list str (U.split_mime_string "ABC" "--ABC\r\nfooAB.--ABC\r\nbar--ABC--")));
  line "split_mime_string \"B\" \"no delimiter\"" "" (guard (fun () -> list str (U.split_mime_string "B" "no delimiter")));
  List.iter (fun s -> line "trim_std" (str s) (guard (fun () -> str (U.trim_std s))))
    [ "  a  b  "; "a \" x  y \" b"; "\t\n"; "" ];
  line "trim_all \"'\" \" \" \"  a  'b  c'  d \"" "" (guard (fun () -> str (U.trim_all "'" " " "  a  'b  c'  d ")));
  line "tokens \"'\" \" \" \"a 'b c' d\"" "" (guard (fun () -> list str (U.tokens "'" " " "a 'b c' d")));
  line "prepend \"-I\" [\"a\"; \"b c\"]; prepend \"x\" []" "" (guard (fun () -> str (U.prepend "-I" [ "a"; "b c" ])) ^ " " ^ guard (fun () -> str (U.prepend "x" [])));
  let file = Filename.temp_file "test_string_rest" ".txt" in
  let oc = open_out_bin file in
  output_string oc "line 1\nline 2\r\n\000end"; close_out oc;
  line "string_of_file (a 21-byte file)" "" (guard (fun () -> str (U.string_of_file file)));
  Sys.remove file;
  line "string_of_file (the file removed)" "" (try ignore (U.string_of_file file); "no exception"
                                               with Sys_error _ -> "Sys_error" | e -> "EXN " ^ Printexc.to_string e);
  line "make \"t\" 3 'x'; make \"t\" (-1) 'x'" "" (guard (fun () -> str (U.make "t" 3 'x')) ^ " " ^ guard (fun () -> str (U.make "t" (-1) 'x')));
  line "get \"t\" \"abc\" 1; get \"t\" \"abc\" 3" "" (guard (fun () -> String.make 1 (U.get "t" "abc" 1)) ^ " " ^ guard (fun () -> String.make 1 (U.get "t" "abc" 3)));
  line "blit \"t\" \"abcd\" 1 dst 0 2, then out of range" "" (guard (fun () ->
      let dst = Bytes.of_string "....." in
      U.blit "t" (Bytes.of_string "abcd") 1 dst 0 2;
      Bytes.to_string dst) ^ " " ^ guard (fun () -> U.blit "t" (Bytes.of_string "ab") 1 (Bytes.create 3) 0 5; "no exception"))
