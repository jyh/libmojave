(* The rest of Lm_channel (what test_channel and the lexer tests do not
   reach). Models, from a fixed LCG, over strings of a b space and
   newline (no carriage return, so Windows text mode reads them alike):
   - of_substring s off len reads String.sub s off len;
   - input_entire_line, called until End_of_file, gives the lines of the
     string with their newlines (the last without one if it has none);
   - of_fun reads, in order, every byte its reader returns, however the
     reader cuts them (1 to 7 bytes a call, 0 at the end), and what is
     written and flushed reaches its writer.
   Per check: how many cases differ, and the first. Then pins: locations
   (of_loc_string, create_loc_string, set_line), lex_buffer, descr of a
   string channel, set_io_functions. *)
open Pin
module C = Lm_channel

let seed = ref 97
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 12) mod n
let rstring n = String.init (rnd (n + 1)) (fun _ -> "ab \n".[rnd 4])

let read_all ch =
  let b = Buffer.create 16 in
  (try while true do Buffer.add_char b (C.input_char ch) done with End_of_file -> ());
  Buffer.contents b

let check name n case =
  seed := 97;
  let bad = ref 0 and first = ref "" in
  for _ = 1 to n do
    match case () with
    | true, _ -> ()
    | false, d -> incr bad; if !first = "" then first := d ()
  done;
  line (name ^ ": cases that differ") (string_of_int n ^ " cases")
    (Printf.sprintf "%d%s" !bad (if !bad = 0 then "" else " (first: " ^ !first ^ ")"))

let lines_of s =
  let rec go i acc =
    if i >= String.length s then List.rev acc
    else match String.index_from_opt s i '\n' with
      | Some j -> go (j + 1) (String.sub s i (j - i + 1) :: acc)
      | None -> List.rev (String.sub s i (String.length s - i) :: acc) in
  go 0 []

let () =
  check "of_substring s off len reads String.sub s off len" 2000 (fun () ->
      let s = rstring 30 in
      let off = rnd (String.length s + 1) in
      let len = rnd (String.length s - off + 1) in
      let got = guard (fun () -> read_all (C.of_substring s off len)) in
      got = String.sub s off len, (fun () -> Printf.sprintf "%S %d %d: %S" s off len got));
  check "input_entire_line until End_of_file: the lines, newlines kept" 2000 (fun () ->
      let s = rstring 30 in
      let ch = C.of_string s in
      let rec go acc = match C.input_entire_line ch with l -> go (l :: acc) | exception End_of_file -> List.rev acc in
      let got = (try go [] with e -> [ "EXN " ^ Printexc.to_string e ]) in
      got = lines_of s, (fun () -> Printf.sprintf "%S: %s" s (list str got)));
  check "of_fun: reads what the reader gives, however it is cut" 2000 (fun () ->
      let src = rstring 200 in
      let pos = ref 0 in
      let reader b off len =
        let k = min (min len (1 + rnd 7)) (String.length src - !pos) in
        Bytes.blit_string src !pos b off k; pos := !pos + k; k in
      let written = Buffer.create 16 in
      let writer b off len = Buffer.add_subbytes written b off len; len in
      let ch = C.of_fun reader writer in
      let got = guard (fun () -> read_all ch) in
      let out = rstring 50 in
      let wrote = guard (fun () -> C.output_string ch out; C.flush ch; Buffer.contents written) in
      got = src && wrote = out, (fun () -> Printf.sprintf "%S: read %S; wrote %S, writer got %S" src got out wrote));
  (* locations *)
  let at ch n =
    for _ = 1 to n do ignore (C.input_char ch) done;
    Lm_location.string_of_location (C.loc ch) in
  line "of_loc_string \"f.ml\" 10 5 \"ab\\ncd\": loc after 0, 2, 4 chars" "" (String.concat " | " (List.map (fun n ->
      guard (fun () -> at (C.of_loc_string "f.ml" 10 5 "ab\ncd") n)) [ 0; 2; 4 ]));
  line "of_string \"ab\\ncd\": loc after 4 chars" "" (guard (fun () -> at (C.of_string "ab\ncd") 4));
  line "of_string then set_line \"g.ml\" 20, then 4 chars" "" (guard (fun () ->
      let ch = C.of_string "ab\ncd\nef" in ignore (C.input_char ch); C.set_line ch "g.ml" 20; at ch 4));
  line "create_loc_string \"h.ml\" 3 1: write \"x\\ny\", loc; contents" "" (guard (fun () ->
      let ch = C.create_loc_string "h.ml" 3 1 in
      C.output_string ch "x\ny"; C.flush ch;
      Lm_location.string_of_location (C.loc ch) ^ " " ^ str (C.to_string ch)));
  (* lex_buffer: the rest of the input buffer *)
  line "lex_buffer after lex_start and two lex_next" "" (guard (fun () ->
      let ch = C.of_string "hello world" in
      let _ = C.LexerInput.lex_start ch in
      let _ = C.LexerInput.lex_next ch in
      let _ = C.LexerInput.lex_next ch in
      let b = Buffer.create 16 in
      C.LexerInput.lex_buffer ch b;
      str (Buffer.contents b) ^ ", then input_char " ^ (try Printf.sprintf "%C" (C.input_char ch) with e -> Printexc.to_string e)));
  line "descr of a string channel" "" (try ignore (C.descr (C.of_string "x")); "a descriptor" with
                                       | Unix.Unix_error (e, f, _) -> Printf.sprintf "Unix_error %s %s" (Unix.error_message e) f);
  line "set_io_functions: a string channel read through a new reader" "" (guard (fun () ->
      let ch = C.of_string "" in
      let fed = ref false in
      C.set_io_functions ch (fun b off len -> if !fed then 0 else (fed := true; let k = min len 3 in Bytes.blit_string "new" 0 b off k; k)) (fun _ _ len -> len);
      let got = read_all ch in
      str got ^ (if !fed then "" else " (the reader was never called)")))

(* Text mode (Windows only: set_binary_mode is a no-op elsewhere): a
   carriage return at the end of a full 4,096-byte read makes the channel
   read one more byte to see whether a newline follows. The answer is
   whether the bytes come back, with \r\n read as \n or not. *)
let () =
  let src = String.make 4095 'a' ^ "\r\nend\r\n" in
  let pos = ref 0 in
  let reader b off len =
    let k = min len (String.length src - !pos) in
    Bytes.blit_string src !pos b off k; pos := !pos + k; k in
  let ch = C.of_fun reader (fun _ _ len -> len) in
  C.set_binary_mode ch false;
  let squashed = String.concat "\n" (String.split_on_char '\r' src |> List.map (fun s ->
      if String.length s > 0 && s.[0] = '\n' then String.sub s 1 (String.length s - 1) else s)) in
  line "text mode, \\r at byte 4,096: the bytes come back" "" (guard (fun () ->
      let got = read_all ch in string_of_bool (got = src || got = squashed)))
