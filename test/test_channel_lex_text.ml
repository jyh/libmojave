(* The lexer's fill on a text-mode Lm_channel (binary = false; Windows
   only). lex_fill squashes "\r\n" to "\n" as fillbuf does, reading one
   extra character after a final "\r". Every line of a file is lexed
   (clause [^\n]*\n, and \' for the end) from a text-mode file channel.
   Elsewhere create forces binary, so this compares against a per-platform
   expected file: the Unix one measured, the Windows one a prediction made
   by script (every "\r\n" read as "\n", a lone "\r" kept) until the
   Windows lanes measure it. Cases: short lines with a lone "\r", and
   4095 a's then "\r\n" across the channel's 4096-byte text fill. *)
open Pin
module C = Lm_channel
module Action = struct
  type action = string
  let pp_print_action out a = Lm_printf.pp_print_string out a
  let hash = Hashtbl.hash
  let compare = compare
  let choose = min
end
module L = Lm_lexer.MakeLexer (Lm_channel.LexerInput) (Action)

let file = "test_channel_lex_text.tmp"
let raw_write s = let oc = open_out_bin file in output_string oc s; close_out oc
let lexer = snd (L.add_clause (snd (L.add_clause L.empty "line" "[^\n]*\n")) "eof" "\\'")
let lex_all () =
  let ch = C.create file C.FileChannel C.InChannel false (Some (Unix.openfile file [ Unix.O_RDONLY ] 0o644)) in
  let b = Buffer.create 64 in
  let rec go n =
    match L.lex lexer ch with
    | "eof", _, _, _ -> n
    | _, _, s, _ -> Buffer.add_string b s; go (n + 1)
    | exception e -> Buffer.add_string b ("EXN " ^ Printexc.to_string e); n in
  let n = go 0 in
  C.close ch; n, Buffer.contents b
let count c s = String.fold_left (fun n x -> if x = c then n + 1 else n) 0 s

let () =
  raw_write "x\r\ny\r\n\r\nz\rw\r\n";
  let n, s = lex_all () in
  line "lex lines of \"x\\r\\ny\\r\\n\\r\\nz\\rw\\r\\n\"" "" (Printf.sprintf "%d lines, %s" n (str s));
  raw_write (String.make 4095 'a' ^ "\r\n" ^ "tail\r\n");
  let n, s = lex_all () in
  line "lex lines of 4095 a, \"\\r\\n\" at the fill boundary, \"tail\\r\\n\"" ""
    (Printf.sprintf "%d lines, length %d, \\r %d, \\n %d, ends %s" n (String.length s) (count '\r' s) (count '\n' s)
       (str (String.sub s (String.length s - 8) 8)));
  Sys.remove file
