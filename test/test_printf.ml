open Pin
module P = Lm_printf

let render f =
  let b = Buffer.create 64 in
  let ppf = P.formatter_of_buffer b in
  P.pp_set_margin ppf 20;
  f ppf; P.pp_print_flush ppf (); Buffer.contents b

let () =
  let b = Buffer.create 16 in
  P.bprintf b "%d|%s|%5.2f|%x" 42 "s" 3.14159 255;
  line "bprintf" "%d|%s|%5.2f|%x" (str (Buffer.contents b));
  P.bprintf b "+more";
  line "bprintf appends" "" (str (Buffer.contents b));
  line "sprintf" "@[<hv 2>a@ b@]" (str (P.sprintf "@[<hv 2>a@ b@]"));
  line "box breaks at margin 20" "" (str (render (fun ppf ->
    P.fprintf ppf "@[<hov 4>%s@ %s@ %s@ %s@ %s@]" "alpha" "beta" "gamma" "delta" "epsilon")));
  line "vertical box" "" (str (render (fun ppf -> P.fprintf ppf "@[<v 3>x@,y@,z@]")));
  line "nested boxes" "" (str (render (fun ppf ->
    P.fprintf ppf "@[<v 2>top@ @[<h>a b@]@ @[<v 4>inner@ deep@]@]")));
  (* the out-functions path changed for OCaml 5 (stage a): drive it end to end *)
  let log = Buffer.create 64 in
  let ppf = P.formatter_of_buffer (Buffer.create 1) in
  P.pp_set_all_formatter_output_functions ppf
    (fun s i n -> Buffer.add_string log ("S(" ^ String.sub s i n ^ ")"))
    (fun () -> Buffer.add_string log "F")
    (fun () -> Buffer.add_string log "N")
    (fun n -> Buffer.add_string log ("W" ^ string_of_int n));
  P.pp_set_margin ppf 12;
  P.fprintf ppf "@[<v 3>head@,body1@,body2@]@.";
  line "set_all output functions trace" "" (str (Buffer.contents log));
  let o, f, n, w = P.pp_get_all_formatter_output_functions ppf () in
  Buffer.clear log; o "abc" 1 1; f (); n (); w 2;
  line "get_all returns the functions set" "" (str (Buffer.contents log))
