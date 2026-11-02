(* Lm_format's channel formatters against its buffer formatter. The same
   documents are printed through formatter_of_buffer, through
   formatter_of_out_channel (to a file, read back), and through a buffer
   formatter retargeted with pp_set_formatter_out_channel; the three texts
   must be byte-identical, at margins 80, 20 and 8 so that indentation
   (the spaces each output path writes for itself) is exercised. *)
open Pin
module F = Lm_format

let docs : (string * (F.formatter -> unit)) list =
  [ ("words", fun f -> F.fprintf f "alpha beta gamma");
    ("hov", fun f -> F.fprintf f "@[<hov 2>alpha@ beta@ gamma@ delta@ epsilon@ zeta@]");
    ("v", fun f -> F.fprintf f "@[<v 4>first@,second@,third@]");
    ("nested", fun f -> F.fprintf f "@[<hov 1>(f@ @[<hov 2>(g@ x@ y)@]@ @[<hov 2>(h@ z)@])@]");
    ("break", fun f -> F.fprintf f "@[<hov 0>a@;<1 6>b@;<1 6>c@;<1 6>d@]");
    ("newline", fun f -> F.fprintf f "one@\ntwo@\n   three") ]

let read_file file =
  let ic = open_in_bin file in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic; s

let () =
  List.iter (fun (name, d) ->
      List.iter (fun m ->
          let b = Buffer.create 64 in
          let fb = F.formatter_of_buffer b in
          F.pp_set_margin fb m; d fb; F.pp_print_flush fb ();
          let file1 = Filename.temp_file "format_channel" ".txt" in
          let oc1 = open_out_bin file1 in
          let fc = F.formatter_of_out_channel oc1 in
          F.pp_set_margin fc m; d fc; F.pp_print_flush fc (); close_out oc1;
          let c1 = read_file file1 in Sys.remove file1;
          let file2 = Filename.temp_file "format_channel" ".txt" in
          let oc2 = open_out_bin file2 in
          let fr = F.formatter_of_buffer (Buffer.create 1) in
          F.pp_set_formatter_out_channel fr oc2;
          F.pp_set_margin fr m; d fr; F.pp_print_flush fr (); close_out oc2;
          let c2 = read_file file2 in Sys.remove file2;
          let s = Buffer.contents b in
          line name (Printf.sprintf "margin %d: buffer = formatter_of_out_channel = pp_set_formatter_out_channel" m)
            (if s = c1 && s = c2 then "true" else Printf.sprintf "false (buffer %S channel %S retargeted %S)" s c1 c2);
          line name (Printf.sprintf "margin %d: text" m) (str s))
        [ 80; 20; 8 ])
    docs
