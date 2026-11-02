(* Lm_rformat.format_quoted_string against a reader, and unmarshal_buffers
   on bad input. format_quoted_string prints a string as it is, or in double
   quotes with \n \r \t and \" escaped when it holds one of those characters
   or a space, starts with ', or is empty. The quotes exist so the text reads
   back: the model is OCaml's own string-literal reader (Scanf "%S"), which
   must give back the original from every quoted output, and an unquoted
   output must equal the original. 3000 strings from a fixed LCG over
   a, space, newline, CR, tab, ", ', backslash and \001; the cases where the
   model fails are counted and the first is shown. Then unmarshal_buffers:
   a round trip, the empty string, a short string, a corrupt header, and a
   record of the right shape with another version number. *)
open Pin
module F = Lm_rformat
module T = Lm_rformat_text

let seed = ref 11
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n
let alphabet = [| 'a'; ' '; '\n'; '\r'; '\t'; '"'; '\''; '\\'; '\001' |]

let quoted s =
  let b = F.new_buffer () in F.format_quoted_string b s; T.print_text_string 1000 b

let reads_back s =
  let o = quoted s in
  if String.length o > 0 && o.[0] = '"' then
    (match Scanf.sscanf o "%S%!" (fun x -> x) with x -> x = s | exception _ -> false)
  else o = s

let () =
  let bad = ref 0 and first = ref "" and with_bs = ref 0 and bad_bs = ref 0 in
  for _ = 1 to 3000 do
    let s = String.init (rnd 6) (fun _ -> alphabet.(rnd (Array.length alphabet))) in
    let has_bs = String.contains s '\\' in
    if has_bs then incr with_bs;
    if not (reads_back s) then begin
      incr bad; if has_bs then incr bad_bs;
      if !first = "" then first := Printf.sprintf "%S printed as %S" s (quoted s)
    end
  done;
  line "format_quoted_string" "3000 strings, read back with %S"
    (Printf.sprintf "%d fail (%d of the %d with a backslash)%s" !bad !bad_bs !with_bs
       (if !first = "" then "" else "; first: " ^ !first));
  List.iter (fun s -> line "format_quoted_string" (str s) (str (quoted s)))
    [ "a\\b"; "a\\ b"; "x\\\" y"; "\\" ]

let () =
  let b = F.new_buffer () in
  F.format_string b "hello"; F.format_space b; F.format_string b "world";
  let s = F.marshal_buffers [ b ] in
  let attempt name x =
    line "unmarshal_buffers" name
      (match F.unmarshal_buffers x with
       | l -> Printf.sprintf "%d buffer(s): %s" (List.length l)
                (String.concat " | " (List.map (T.print_text_string 80) l))
       | exception Failure m -> "Failure " ^ m
       | exception e -> "raised " ^ Printexc.to_string e) in
  attempt "round trip" s;
  attempt "empty string" "";
  attempt "short string" (String.sub s 0 10);
  attempt "header cut short of its data" (String.sub s 0 (String.length s - 1));
  attempt "corrupt header" (String.make (String.length s) 'x');
  (* the record is { marshal_version : string; marshal_buffers : ... list }:
     the same shape with another version string *)
  attempt "another version" (Marshal.to_string ("another version", ([] : int list)) [])
