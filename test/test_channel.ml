open Pin
module C = Lm_channel

let () =
  let ch = C.of_string "line one\nline two\n\nlast" in
  let rec lines acc = match C.input_line ch with
    | l -> lines (l :: acc)
    | exception e -> List.rev (("END " ^ Printexc.to_string e) :: acc) in
  line "input_line*" "\"line one\\nline two\\n\\nlast\"" (list str (lines []));
  let ch = C.of_string "abcdef" in
  line "input_char" "\"abcdef\"" (guard (fun () -> Printf.sprintf "%C" (C.input_char ch)));
  line "input_char again" "" (guard (fun () -> Printf.sprintf "%C" (C.input_char ch)));
  line "tell on a string channel" "" (guard (fun () -> string_of_int (C.tell ch)));
  line "read 3 on a string channel" "" (guard (fun () ->
    let b = Bytes.make 3 '.' in let n = C.read ch b 0 3 in Printf.sprintf "%d %S" n (Bytes.to_string b)));
  line "input_buffer 3 on a fresh string channel" "\"abcdef\"" (guard (fun () ->
    let ch = C.of_string "abcdef" in let b = Bytes.make 3 '.' in C.input_buffer ch b 0 3; Bytes.to_string b));
  line "create_string + output_string" "" (guard (fun () ->
    let out = C.create_string () in
    C.output_string out "hello"; C.output_string out ", world"; C.flush out; str (C.to_string out)));
  line "seek 4 SET then input_char" "\"0123456789\"" (guard (fun () ->
    let ch = C.of_string "0123456789" in let p = C.seek ch 4 Unix.SEEK_SET in Printf.sprintf "%d %C" p (C.input_char ch)));
  line "input_char at eof" "\"\"" (guard (fun () -> String.make 1 (C.input_char (C.of_string ""))))
