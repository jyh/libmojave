(* Lm_string_util's token scanners against models, from a fixed LCG.

   - tokens_create_lexer / tokens_string / tokens_lex / tokens_data /
     tokens_add / tokens_atomic / tokens_break / tokens_flush: random
     event sequences against a character-at-a-time reference written from
     the interface: whitespace outside quotes ends a word; a single or double
     quote opens a quote that the same character closes; a backslash takes the next
     character of the same string verbatim; tokens_string keeps quotes in
     the text, tokens_lex drops them and wraps the quoted text as data;
     the lexer is asked only outside quotes and escapes.  Every value is
     wrapped in a tag naming its wrapper, so a value wrapped by the wrong
     function is a disagreement.
   - incremental = whole: a string fed to tokens_string in pieces gives
     the tokens of the whole string (the interface's "incremental
     parsing").  Cuts just after a backslash are excluded and recorded
     below: the escape does not reach into the next piece.
   - tokens / tokens_std: split on delimiters outside quotes, a quote
     closed by the character that opened it ("Quotations are not split"),
     a backslash in a quote taking the next character. *)
open Pin
module S = Lm_string_util

let seed = ref 263
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n

let alphabet = "ab  \t\\\"'&|;"
let rstring max = String.init (rnd (max + 1)) (fun _ -> alphabet.[rnd (String.length alphabet)])

let bad = Hashtbl.create 8 and first = Hashtbl.create 8 and order = ref [] and steps = Hashtbl.create 8
let check op ok d =
  if not (List.mem op !order) then order := op :: !order;
  Hashtbl.replace steps op (1 + try Hashtbl.find steps op with Not_found -> 0);
  if not ok then begin
    Hashtbl.replace bad op (1 + try Hashtbl.find bad op with Not_found -> 0);
    if not (Hashtbl.mem first op) then Hashtbl.replace first op (d ())
  end

let sl l = "[" ^ String.concat "; " (List.map (Printf.sprintf "%S") l) ^ "]"
let res f = match f () with v -> Ok v | exception e -> Error (Printexc.to_string e)
let show = function Ok l -> sl l | Error e -> "EXN " ^ e

(* The given functions: tags name the wrapper. *)
let wrap_string s = "S<" ^ s ^ ">"
let wrap_data s = "D<" ^ s ^ ">"
let wrap_token s = "T<" ^ s ^ ">"
let group l = "G[" ^ String.concat "," l ^ "]"
let lexer s i len =
  if i + 1 < len && s.[i] = '&' && s.[i + 1] = '&' then Some 2
  else if s.[i] = '|' || s.[i] = ';' then Some 1
  else None

type event = Str of string | Lex of string | Data of string | Add of string | Atomic of string | Break

let show_event = function
  | Str s -> "string " ^ str s | Lex s -> "lex " ^ str s | Data s -> "data " ^ str s
  | Add s -> "add " ^ str s | Atomic s -> "atomic " ^ str s | Break -> "break"

let run evs =
  let t = S.tokens_create_lexer ~lexer ~wrap_string ~wrap_data ~wrap_token ~group in
  let t = List.fold_left (fun t -> function
      | Str s -> S.tokens_string t s | Lex s -> S.tokens_lex t s | Data s -> S.tokens_data t s
      | Add x -> S.tokens_add t x | Atomic x -> S.tokens_atomic t x | Break -> S.tokens_break t) t evs in
  S.tokens_flush t

(* The reference. *)
type mode = White | Word | Quote of char

let model evs =
  let out = ref [] and cur = ref [] and mode = ref White in
  let emit () =
    (match List.rev !cur with [] -> out := wrap_data "" :: !out | [ x ] -> out := x :: !out | l -> out := group l :: !out);
    cur := []; mode := White in
  let add x = cur := x :: !cur; if !mode = White then mode := Word in
  let scan ~lex s =
    let n = String.length s and b = Buffer.create 8 in
    (* the wrapper of the text in b: tokens_string wraps all as string,
       tokens_lex wraps quoted text as data *)
    let piece () =
      if Buffer.length b > 0 then begin
        let w = (match !mode with Quote _ when lex -> wrap_data | _ -> wrap_string) in
        cur := w (Buffer.contents b) :: !cur; Buffer.clear b
      end in
    let i = ref 0 in
    while !i < n do
      let c = s.[!i] in
      (match !mode, c with
       | (White | Word), (' ' | '\t' | '\n' | '\r' | '\012') ->
         if !mode = Word then (piece (); emit ()); incr i
       | (White | Word), ('"' | '\'') ->
         if lex then (piece (); mode := Quote c) else (mode := Quote c; Buffer.add_char b c);
         incr i
       | (White | Word), '\\' ->
         mode := Word; Buffer.add_string b (String.sub s !i (min 2 (n - !i))); i := !i + 2
       | (White | Word), _ ->
         (match (if lex then lexer s !i n else None) with
          | Some k ->
            if !mode = Word then (piece (); emit ());
            out := wrap_token (String.sub s !i k) :: !out; i := !i + k
          | None -> mode := Word; Buffer.add_char b c; incr i)
       | Quote q, _ when c = q ->
         if lex then (piece (); mode := Word) else (Buffer.add_char b c; mode := Word);
         incr i
       | Quote _, '\\' -> Buffer.add_string b (String.sub s !i (min 2 (n - !i))); i := !i + 2
       | Quote _, _ -> Buffer.add_char b c; incr i)
    done;
    piece () in
  List.iter (function
      | Str s -> scan ~lex:false s
      | Lex s -> scan ~lex:true s
      | Data s -> add (wrap_data s)
      | Add x -> add x
      | Atomic x -> if !mode <> White then emit (); out := x :: !out
      | Break -> if !mode <> White then emit ()) evs;
  if !mode <> White then emit ();
  List.rev !out

let gen_events lex =
  List.init (1 + rnd 5) (fun _ ->
      match rnd 10 with
      | 0 -> Data (rstring 3)
      | 1 -> Add ("X" ^ string_of_int (rnd 10))
      | 2 -> Atomic ("A" ^ string_of_int (rnd 10))
      | 3 -> Break
      | _ -> let s = rstring 8 in if lex then Lex s else Str s)

let () =
  for _ = 1 to 3000 do
    List.iter (fun (name, lex) ->
        let evs = gen_events lex in
        let a = res (fun () -> run evs) and m = Ok (model evs) in
        check name (a = m) (fun () ->
            String.concat ", " (List.map show_event evs) ^ ": got " ^ show a ^ ", model " ^ show m))
      [ ("tokens_string + data/add/atomic/break vs model", false);
        ("tokens_lex + data/add/atomic/break vs model", true) ]
  done

(* Incremental = whole, with plain wrappers. *)
let plain () = S.tokens_create (fun s -> s) (String.concat "")
let feed f t pieces = S.tokens_flush (List.fold_left f t pieces)
let rec cuts s =
  (* random cut points, none just after a backslash *)
  let n = String.length s in
  if n = 0 then [ s ] else
    let k = rnd (n + 1) in
    if k = 0 || k = n || s.[k - 1] = '\\' then [ s ]
    else String.sub s 0 k :: cuts (String.sub s k (n - k))

let () =
  for _ = 1 to 3000 do
    let s = rstring 16 in
    let pieces = cuts s in
    List.iter (fun (name, f) ->
        let whole = res (fun () -> feed f (plain ()) [ s ]) and inc = res (fun () -> feed f (plain ()) pieces) in
        check name (whole = inc) (fun () -> sl pieces ^ ": whole " ^ show whole ^ ", in pieces " ^ show inc))
      [ ("tokens_string in pieces = whole", S.tokens_string); ("tokens_lex in pieces = whole", S.tokens_lex) ]
  done

(* tokens / tokens_std. *)
let ref_tokens quotes delims s =
  let n = String.length s and out = ref [] and b = Buffer.create 8 in
  let fin () = if Buffer.length b > 0 then (out := Buffer.contents b :: !out; Buffer.clear b) in
  let rec go i q =
    if i < n then
      let c = s.[i] in
      match q with
      | None when String.contains delims c -> fin (); go (i + 1) None
      | None when String.contains quotes c -> Buffer.add_char b c; go (i + 1) (Some c)
      | None -> Buffer.add_char b c; go (i + 1) None
      | Some q when c = q -> Buffer.add_char b c; go (i + 1) None
      | Some q when c = '\\' && i < n - 1 -> Buffer.add_string b (String.sub s i 2); go (i + 2) (Some q)
      | Some q -> Buffer.add_char b c; go (i + 1) (Some q) in
  go 0 None; fin (); List.rev !out

let () =
  for _ = 1 to 3000 do
    let s = rstring 16 in
    let delims = if rnd 2 = 0 then S.white else " &" in
    let quotes = if rnd 2 = 0 then S.quotes else "\"" in
    check "tokens vs model" (res (fun () -> S.tokens quotes delims s) = Ok (ref_tokens quotes delims s))
      (fun () -> Printf.sprintf "tokens %S %S %S: got %s, model %s" quotes delims s
          (show (res (fun () -> S.tokens quotes delims s))) (sl (ref_tokens quotes delims s)));
    check "tokens_std = tokens quotes white" (res (fun () -> S.tokens_std s) = res (fun () -> S.tokens S.quotes S.white s))
      (fun () -> str s)
  done;
  List.iter (fun op ->
      let k = try Hashtbl.find bad op with Not_found -> 0 in
      line op (string_of_int (Hashtbl.find steps op) ^ " cases")
        (if k = 0 then "0 disagree" else Printf.sprintf "%d disagree (first: %s)" k (Hashtbl.find first op)))
    (List.rev !order)

(* Fixed cases. *)
let () =
  let t f pieces = guard (fun () -> sl (feed f (plain ()) pieces)) in
  (* a backslash that ends a piece does not escape the next piece's first character *)
  line "tokens_string" "[\"a\\\\\"; \" b\"] vs whole" (t S.tokens_string [ "a\\"; " b" ] ^ " vs " ^ t S.tokens_string [ "a\\ b" ]);
  line "tokens_lex" "[\"a\\\\\"; \" b\"] vs whole" (t S.tokens_lex [ "a\\"; " b" ] ^ " vs " ^ t S.tokens_lex [ "a\\ b" ]);
  (* an open quote does carry *)
  line "tokens_string" "[\"\\\"a\"; \" b\\\"\"]" (t S.tokens_string [ "\"a"; " b\"" ]);
  line "tokens_lex" "[\"'a\"; \" b'\"; \" c\"]" (t S.tokens_lex [ "'a"; " b'"; " c" ]);
  (* a quote of the other kind inside a quote *)
  List.iter (fun s -> line "tokens_std" (str s) (guard (fun () -> sl (S.tokens_std s))))
    [ "\"it's here\" x"; "'say \"hi\" now' y"; "a\\ b"; "\"a\\\" b\" c"; "\"open quote" ];
  (* the interface's own example *)
  let e = S.tokens_create (fun s -> s) (String.concat "") in
  let e = S.tokens_string e " Foo \"bar \\\" baz" in
  let e = S.tokens_data e "  a \"b c   " in
  let e = S.tokens_string e "boo\" bum" in
  line "interface example" "" (sl (S.tokens_flush e))
