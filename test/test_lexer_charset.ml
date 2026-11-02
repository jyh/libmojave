(* Lm_lexer's bracket expressions and character escapes against a model,
   from a fixed LCG.

   A regex is one bracket expression [...] or [^...], built from parts:
   plain characters, ranges a-z, classes [:name:], the escapes the lexer
   reads (\n \t ..., \xHH, \NNN octal), and the POSIX positional
   literals: a ] first, a - first or last, and a [ that does not open a
   class. The model's set is computed from the parts, not by parsing the
   string; the lexer's set is the characters 0-255 that a lexer with that
   one clause matches as a one-character string. Classes are modelled on
   ASCII (the C locale). Then the escapes outside brackets, each against
   the character it names.

   Cases where the lexer does not follow POSIX are recorded below as
   fixed lines, with what POSIX gives. *)
open Pin

module Action = struct
  type action = string
  let pp_print_action out a = Lm_printf.pp_print_string out a
  let hash = Hashtbl.hash
  let compare = compare
  let choose = min
end
module L = Lm_lexer.MakeLexer (Lm_channel.LexerInput) (Action)

let seed = ref 264
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n

let bad = Hashtbl.create 8 and first = Hashtbl.create 8 and order = ref [] and steps = Hashtbl.create 8
let check op ok d =
  if not (List.mem op !order) then order := op :: !order;
  Hashtbl.replace steps op (1 + try Hashtbl.find steps op with Not_found -> 0);
  if not ok then begin
    Hashtbl.replace bad op (1 + try Hashtbl.find bad op with Not_found -> 0);
    if not (Hashtbl.mem first op) then Hashtbl.replace first op (d ())
  end

(* The lexer's set: Ok of the matched codes, or Error of the parse failure. *)
let lexer_set re =
  match L.add_clause L.empty "hit" re with
  | exception e -> Error (Printexc.to_string e)
  | _, l ->
    Ok (List.filter (fun c ->
        let s = String.make 1 (Char.chr c) in
        match L.lex l (Lm_channel.of_string s) with
        | ("hit", _, lexeme, _) -> lexeme = s
        | _ -> false
        | exception _ -> false) (List.init 256 (fun c -> c)))

let show_codes l =
  let one c = if c >= 33 && c <= 126 then String.make 1 (Char.chr c) else Printf.sprintf "\\%03d" c in
  let n = List.length l in
  String.concat "" (List.map one (List.filteri (fun i _ -> i < 12) l)) ^ (if n > 12 then Printf.sprintf "...(%d)" n else "")
let show_set = function Ok l -> "{" ^ show_codes l ^ "}" | Error e -> "EXN " ^ e

(* ASCII classes. *)
let classes = [
  "alnum", (fun c -> (c >= 48 && c <= 57) || (c >= 65 && c <= 90) || (c >= 97 && c <= 122));
  "alpha", (fun c -> (c >= 65 && c <= 90) || (c >= 97 && c <= 122));
  "blank", (fun c -> c = 32 || c = 9);
  "cntrl", (fun c -> c < 32 || c = 127);
  "digit", (fun c -> c >= 48 && c <= 57);
  "graph", (fun c -> c >= 33 && c <= 126);
  "lower", (fun c -> c >= 97 && c <= 122);
  "print", (fun c -> c >= 32 && c <= 126);
  "punct", (fun c -> c >= 33 && c <= 126 && not ((c >= 48 && c <= 57) || (c >= 65 && c <= 90) || (c >= 97 && c <= 122)));
  "space", (fun c -> c = 32 || (c >= 9 && c <= 13));
  "upper", (fun c -> c >= 65 && c <= 90);
  "xdigit", (fun c -> (c >= 48 && c <= 57) || (c >= 65 && c <= 70) || (c >= 97 && c <= 102));
]

(* Escapes the lexer reads, with the code each names. *)
let escapes = [ "\\a", 7; "\\b", 8; "\\f", 12; "\\n", 10; "\\r", 13; "\\t", 9; "\\v", 11 ]
let gen_escape () =
  match rnd 4 with
  | 0 -> List.nth escapes (rnd (List.length escapes))
  | 1 -> let c = rnd 256 in Printf.sprintf "\\x%02x" c, c
  | 2 -> let c = rnd 256 in Printf.sprintf "\\%03o" c, c
  | _ -> let c = "]\\[-^.*" .[rnd 7] in "\\" ^ String.make 1 c, Char.code c

(* Plain characters: printable, none of ] [ \ - ^ : *)
let pool = "abcxyzABZ019!#$%&*+,./;<=>?@_{|}~ ()\"'"
let pchar () = pool.[rnd (String.length pool)]

type part = Lit of char | Range of char * char | Class of string | Esc of string * int

let render_part = function
  | Lit c -> String.make 1 c
  | Range (a, b) -> Printf.sprintf "%c-%c" a b
  | Class n -> "[:" ^ n ^ ":]"
  | Esc (s, _) -> s
let part_mem c = function
  | Lit x -> Char.code x = c
  | Range (a, b) -> c >= Char.code a && c <= Char.code b
  | Class n -> (List.assoc n classes) c
  | Esc (_, x) -> x = c

let gen_bracket () =
  let neg = rnd 4 = 0 in
  let parts = List.init (1 + rnd 4) (fun _ ->
      match rnd 5 with
      | 0 -> let a = pchar () and b = pchar () in if a <= b then Range (a, b) else Range (b, a)
      | 1 -> Class (fst (List.nth classes (rnd (List.length classes))))
      | 2 -> let s, c = gen_escape () in Esc (s, c)
      | _ -> Lit (pchar ())) in
  (* POSIX positional literals *)
  let lead = match rnd 6 with 0 -> [ Lit ']' ] | 1 -> [ Lit '-' ] | _ -> [] in
  let tail = match rnd 6 with 0 -> [ Lit '-' ] | 1 -> [ Lit '[' ] | _ -> [] in
  let parts = lead @ parts @ tail in
  (* \xHH reads every hex digit that follows (pinned below), so a hex
     escape is never followed by a part that starts with one *)
  let hex c = String.contains "0123456789abcdefABCDEF" c in
  let rec adjacent = function
    | Esc (e, _) :: p :: rest -> (e.[1] = 'x' && hex (render_part p).[0]) || adjacent (p :: rest)
    | _ :: rest -> adjacent rest
    | [] -> false in
  if adjacent parts then None else
  let re = "[" ^ (if neg then "^" else "") ^ String.concat "" (List.map render_part parts) ^ "]" in
  let mem c = List.exists (part_mem c) parts in
  Some (re, List.filter (fun c -> if neg then not (mem c) else mem c) (List.init 256 (fun c -> c)))

let () =
  for _ = 1 to 600 do
    match gen_bracket () with
    | None -> ()
    | Some (re, want) ->
      let got = lexer_set re in
      check "bracket expression vs model" (got = Ok want) (fun () ->
          Printf.sprintf "%S: model %s, lexer %s" re (show_set (Ok want)) (show_set got))
  done;
  List.iter (fun (n, f) ->
      let re = "[[:" ^ n ^ ":]]" and want = List.filter f (List.init 256 (fun c -> c)) in
      let got = lexer_set re in
      check "each class vs ASCII" (got = Ok want) (fun () ->
          Printf.sprintf "%s: model %s, lexer %s" re (show_set (Ok want)) (show_set got))) classes;
  for _ = 1 to 400 do
    let s, c = gen_escape () in
    let got = lexer_set s in
    check "escape outside brackets vs model" (got = Ok [ c ]) (fun () ->
        Printf.sprintf "%S: model %s, lexer %s" s (show_set (Ok [ c ])) (show_set got))
  done;
  List.iter (fun op ->
      let k = try Hashtbl.find bad op with Not_found -> 0 in
      line op (string_of_int (Hashtbl.find steps op) ^ " cases")
        (if k = 0 then "0 disagree" else Printf.sprintf "%d disagree (first: %s)" k (Hashtbl.find first op)))
    (List.rev !order)

(* Fixed cases: the lexer's set, then what POSIX (or the escape's own
   reading) gives. *)
let () =
  List.iter (fun (re, posix) -> line "set" (Printf.sprintf "%S" re) (show_set (lexer_set re) ^ "  posix " ^ posix))
    [ "[[]", "{[}"; "[a[]", "{[a}"; "[a-]", "{-a}"; "[]a]", "{]a}"; "[-a]", "{-a}";
      "[\\x41-\\x43]", "(not POSIX; a range would be {ABC})"; "[a-\\x63]", "(not POSIX; a range would be {abc})";
      "[\\8]", "(no octal digit)"; "[\\x]", "(no hex digit)" ];
  List.iter (fun re -> line "escape" (Printf.sprintf "%S" re) (show_set (lexer_set re)))
    [ "\\8"; "\\x"; "\\x4"; "\\x414"; "\\1234"; "\\w"; "\\W" ]
