(* Lm_position: positions record where an error came from. Each
   constructor keeps its detail only when debug_pos is set; otherwise it
   passes the inner position through. loc_of_pos should give the same
   location either way, and pp_print_pos prints the location, then the
   detail under debug_pos. Nothing in metaprl uses the module; omake has
   its own copy. *)
open Pin

module Name = struct
  type t = int
  let name = "Test"
  let loc_of_value i = Lm_location.create_loc (Lm_symbol.add "v.ml") i 0 i 1
  let pp_print_value buf i = Lm_printf.fprintf buf "v%d" i
end
module P = Lm_position.MakePos (Name)

let loc f a = Lm_location.create_loc (Lm_symbol.add f) a 0 a 9
let show_loc l = Lm_location.string_of_location l
let print p =
  let b = Buffer.create 64 in
  let fmt = Lm_printf.formatter_of_buffer b in
  P.pp_print_pos fmt p;
  Lm_printf.pp_print_flush fmt ();
  str (Buffer.contents b)

let with_debug d f =
  let old = !Lm_position.debug_pos in
  Lm_position.debug_pos := d;
  match f () with
  | v -> Lm_position.debug_pos := old; v
  | exception e -> Lm_position.debug_pos := old; raise e

let chains =
  [ ("loc_exp_pos", fun () -> P.loc_exp_pos (loc "a.ml" 1));
    ("base_pos 7", fun () -> P.base_pos 7);
    ("int_pos 3 (base_pos 7)", fun () -> P.int_pos 3 (P.base_pos 7));
    ("string_pos s (cons_pos 2 (loc_exp_pos a:1))", fun () -> P.string_pos "s" (P.cons_pos 2 (P.loc_exp_pos (loc "a.ml" 1))));
    ("symbol_pos x (base_pos 4)", fun () -> P.symbol_pos (Lm_symbol.add "x") (P.base_pos 4));
    ("loc_pos b:2 (base_pos 7)", fun () -> P.loc_pos (loc "b.ml" 2) (P.base_pos 7));
    ("pos_pos (base_pos 1) (base_pos 2)", fun () -> P.pos_pos (P.base_pos 1) (P.base_pos 2));
    ("del_pos f a:3", fun () -> P.del_pos (fun buf -> Lm_printf.pp_print_string buf "delayed") (loc "a.ml" 3));
    ("del_exp_pos f (base_pos 5)", fun () -> P.del_exp_pos (fun buf -> Lm_printf.pp_print_string buf "delayed") (P.base_pos 5)) ]

let () =
  List.iter (fun d ->
      List.iter (fun (n, mk) ->
          let p = with_debug d mk in
          line (Printf.sprintf "debug_pos=%b loc_of_pos" d) n (show_loc (P.loc_of_pos p));
          line (Printf.sprintf "debug_pos=%b pp_print_pos" d) n (with_debug d (fun () -> print p))) chains)
    [ false; true ]

(* Random chains, built once with debug_pos off and once on from the same
   choices: the locations must agree. *)
let seed = ref 90
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n

let () =
  let disagree = ref 0 and first = ref "" in
  for _ = 1 to 2000 do
    let choices = List.init (1 + rnd 6) (fun _ -> (rnd 8, rnd 9)) in
    let build () =
      List.fold_left (fun p (k, a) ->
          match k with
          | 0 -> P.loc_pos (loc "c.ml" a) p
          | 1 -> P.cons_pos a p
          | 2 -> P.pos_pos (P.base_pos (a + 50)) p
          | 3 -> P.int_pos a p
          | 4 -> P.string_pos "s" p
          | 5 -> P.symbol_pos (Lm_symbol.add "y") p
          | 6 -> P.del_exp_pos (fun _ -> ()) p
          | _ -> P.base_pos a) (P.base_pos 0) choices in
    let off = with_debug false build and on = with_debug true build in
    let a = show_loc (P.loc_of_pos off) and b = show_loc (P.loc_of_pos on) in
    if a <> b then begin
      incr disagree;
      if !first = "" then first := Printf.sprintf "%s vs %s" a b
    end
  done;
  line "loc_of_pos, debug_pos off vs on" "2000 random chains"
    (if !disagree = 0 then "0 disagree" else Printf.sprintf "%d disagree (first: %s)" !disagree !first)
