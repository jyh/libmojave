(* Lm_channel's text-mode write (Windows only) past one buffer: expand_text
   turns every "\n" of the 4096-byte output buffer into "\r\n" in a write
   buffer twice that size. Each case is written through a text-mode file
   channel, and the same string through Stdlib's own text-mode channel
   (open_out), and both files' bytes are summarised, so on Windows OCaml
   itself is a second oracle. Elsewhere both are binary. Per-platform
   expected files: the Unix one measured, the Windows one a prediction
   made by script (every "\n" written as "\r\n", "\r\n" as "\r\r\n") until
   the Windows lanes measure it. *)
open Pin
module C = Lm_channel

let file = "test_channel_text_write.tmp"
let raw_read () =
  let ic = open_in_bin file in
  let s = really_input_string ic (in_channel_length ic) in close_in ic; s
let count c s = String.fold_left (fun n x -> if x = c then n + 1 else n) 0 s
let summary s = Printf.sprintf "length %d, \\r %d, \\n %d, ends %s" (String.length s) (count '\r' s) (count '\n' s)
    (str (String.sub s (max 0 (String.length s - 8)) (min 8 (String.length s))))
let lm s =
  let ch = C.create file C.FileChannel C.OutChannel false
      (Some (Unix.openfile file [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644)) in
  C.output_string ch s; C.close ch; summary (raw_read ())
let std s = let oc = open_out file in output_string oc s; close_out oc; summary (raw_read ())

let () =
  List.iter (fun (name, s) ->
      line ("text write, " ^ name) "Lm_channel" (lm s);
      line ("text write, " ^ name) "Stdlib open_out" (std s))
    [ ("5000 newlines", String.make 5000 '\n');
      ("4095 a then \"\\n\" then 4096 b", String.make 4095 'a' ^ "\n" ^ String.make 4096 'b');
      ("\"a\\r\\nb\\n\"", "a\r\nb\n") ];
  Sys.remove file
