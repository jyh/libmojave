(* Lm_lexer's consuming delimiters against a model, from a fixed LCG:
   $ (a newline, a carriage return, \r\n, or the end of input) and \'
   (the end of input). Random lexers of 1-4 clauses over a, \n and \r
   (characters, ".", brackets, alternation, sequence, *, +, ?, {m,n},
   and $ and \' as atoms), then \' as a last clause. The end marker is
   read like a character that never runs out: matching it consumes no
   text but counts as one more unit of length, so a clause that matches
   up to the end and then the end marker beats one that matches the
   same text only (measured: x=a* and y=a*$ on "aa" give y). "." and
   [^...] do not match it. The model ranks a match by text length plus
   end markers consumed, then by the earliest clause (choose = min).
   One lex call, and a whole tokenization when the model never takes an
   empty match before the end. *)
open Pin

module Action = struct
  type action = string
  let pp_print_action out a = Lm_printf.pp_print_string out a
  let hash = Hashtbl.hash
  let compare = compare
  let choose = min
end
module L = Lm_lexer.MakeLexer (Lm_channel.LexerInput) (Action)

let seed = ref 101
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n

let bad = Hashtbl.create 8 and first = Hashtbl.create 8 and order = ref [] and steps = Hashtbl.create 8
let check op ok d =
  if not (List.mem op !order) then order := op :: !order;
  Hashtbl.replace steps op (1 + try Hashtbl.find steps op with Not_found -> 0);
  if not ok then begin
    Hashtbl.replace bad op (1 + try Hashtbl.find bad op with Not_found -> 0);
    if not (Hashtbl.mem first op) then Hashtbl.replace first op (d ())
  end

type re =
  | Dollar
  | Eof
  | Chr of char
  | Any
  | Set of char list
  | NotSet of char list
  | Seq of re list
  | Alt of re list
  | Star of re
  | Plus of re
  | Opt of re
  | Rep of re * int * int

let alphabet = [| 'a'; '\n'; '\r' |]
let esc c = match c with '\n' -> "\\n" | '\r' -> "\\r" | c -> String.make 1 c

(* $ and \' are generated only where they cannot repeat: never under
   *, + or an interval with a maximum above 1 (a repeated one at the end
   of input is the hang fixed in Lm_lexer, checked by the fixed cases
   below). *)
let rec gen ?(eof = true) depth =
  if depth = 0 then
    match rnd 9 with
    | 0 -> Any
    | 1 -> Set (List.sort_uniq compare [ alphabet.(rnd 3); alphabet.(rnd 3) ])
    | 2 -> NotSet [ alphabet.(rnd 3) ]
    | 3 | 4 when eof -> Dollar
    | 5 when eof -> Eof
    | _ -> Chr alphabet.(rnd 3)
  else
    match rnd 9 with
    | 0 | 1 -> Seq (List.init (2 + rnd 2) (fun _ -> gen ~eof (depth - 1)))
    | 2 -> Alt (List.init (2 + rnd 2) (fun _ -> gen ~eof (depth - 1)))
    | 3 -> Star (gen ~eof:false (depth - 1))
    | 4 -> Plus (gen ~eof:false (depth - 1))
    | 5 -> Opt (gen ~eof (depth - 1))
    | 6 -> let m = rnd 3 in let k = m + rnd 2 in Rep (gen ~eof:(eof && k <= 1) (depth - 1), m, k)
    | _ -> gen ~eof 0

let rec render r =
  match r with
  | Dollar -> "$"
  | Eof -> "\\'"
  | Chr c -> esc c
  | Any -> "."
  | Set l -> "[" ^ String.concat "" (List.map esc l) ^ "]"
  | NotSet l -> "[^" ^ String.concat "" (List.map esc l) ^ "]"
  | Seq l -> String.concat "" (List.map (fun r -> match r with Alt _ -> "(" ^ render r ^ ")" | _ -> render r) l)
  | Alt l -> String.concat "|" (List.map render l)
  | Star r -> atom r ^ "*"
  | Plus r -> atom r ^ "+"
  | Opt r -> atom r ^ "?"
  | Rep (r, m, n) -> atom r ^ (if m = n then Printf.sprintf "{%d}" m else Printf.sprintf "{%d,%d}" m n)
and atom r =
  match r with
  | Dollar | Eof | Chr _ | Any | Set _ | NotSet _ -> render r
  | _ -> "(" ^ render r ^ ")"

(* Every end of r from (i, e): text index i, e end markers consumed. *)
let rec ends r s (i, e) =
  let n = String.length s in
  let uniq l = List.sort_uniq compare l in
  let text = e = 0 && i < n in
  match r with
  | Chr c -> if text && s.[i] = c then [ (i + 1, 0) ] else []
  | Any -> if text then [ (i + 1, 0) ] else []
  | Set l -> if text && List.mem s.[i] l then [ (i + 1, 0) ] else []
  | NotSet l -> if text && not (List.mem s.[i] l) then [ (i + 1, 0) ] else []
  | Eof -> if i = n then [ (i, e + 1) ] else []
  | Dollar ->
      if i = n then [ (i, e + 1) ]
      else if e = 0 && (s.[i] = '\n' || s.[i] = '\r') then
        uniq ((i + 1, 0) :: (if s.[i] = '\r' && i + 1 < n && s.[i + 1] = '\n' then [ (i + 2, 0) ] else []))
      else []
  | Seq l -> List.fold_left (fun ps r -> uniq (List.concat_map (ends r s) ps)) [ (i, e) ] l
  | Alt l -> uniq (List.concat_map (fun r -> ends r s (i, e)) l)
  | Star r ->
      let rec close seen frontier =
        let next = List.filter (fun p -> not (List.mem p seen)) (uniq (List.concat_map (ends r s) frontier)) in
        if next = [] then seen else close (uniq (seen @ next)) next in
      close [ (i, e) ] [ (i, e) ]
  | Plus r -> ends (Seq [ r; Star r ]) s (i, e)
  | Opt r -> uniq ((i, e) :: ends r s (i, e))
  | Rep (r, m, k) ->
      let rec go j ps acc =
        let acc = if j >= m then uniq (acc @ ps) else acc in
        if j = k then acc else go (j + 1) (uniq (List.concat_map (ends r s) ps)) acc in
      go 0 [ (i, e) ] []

let eof_clause = ("eof", Eof)

(* The model's lex from i: (clause, end index), ranked by i + e. *)
let model clauses s i =
  let best = ref None in
  List.iter (fun (a, r) ->
      List.iter (fun (j, e) -> match !best with
          | Some (_, _, k) when k >= j + e -> ()
          | _ -> best := Some (a, j, j + e)) (ends r s (i, 0))) clauses;
  Option.map (fun (a, j, _) -> (a, j)) !best

let build clauses = List.fold_left (fun l (a, r) -> snd (L.add_clause l a (render r))) L.empty clauses
let show_clauses clauses = String.concat " " (List.map (fun (a, r) -> a ^ "=" ^ render r) clauses)

let lex_once lexer s =
  match L.lex lexer (Lm_channel.of_string s) with
  | (a, _, lexeme, _) -> Some (a, lexeme)
  | exception Failure _ -> None
  | exception e -> Some ("EXN", Printexc.to_string e)

let lex_all lexer s =
  let ch = Lm_channel.of_string s in
  let rec loop acc k =
    if k = 8 then List.rev ("..." :: acc)
    else match L.lex lexer ch with
      | ("eof", _, _, _) -> List.rev ("EOF" :: acc)
      | (a, _, lexeme, _) -> loop ((a ^ ":" ^ String.escaped lexeme) :: acc) (k + 1)
      | exception Failure _ -> List.rev ("FAIL" :: acc)
      | exception e -> List.rev (("EXN " ^ Printexc.to_string e) :: acc) in
  loop [] 0

let model_all clauses s =
  let rec loop acc i k =
    if k = 8 then Some (List.rev ("..." :: acc))
    else match model clauses s i with
      | Some ("eof", _) -> Some (List.rev ("EOF" :: acc))
      | Some (_, j) when j = i && i < String.length s -> None
      | Some (_, j) when j = i -> None
      | Some (a, j) -> loop ((a ^ ":" ^ String.escaped (String.sub s i (j - i))) :: acc) j (k + 1)
      | None -> Some (List.rev ("FAIL" :: acc)) in
  loop [] 0 0

let rec strings n =
  if n = 0 then [ "" ]
  else let shorter = strings (n - 1) in
    shorter @ List.concat_map (fun s -> if String.length s = n - 1 then List.map (fun c -> s ^ String.make 1 c) (Array.to_list alphabet) else []) shorter
let all_strings = List.sort_uniq compare (strings 5)

let skipped = ref 0

let compare_all tag lexer clauses =
  List.iter (fun s ->
      let want = match model clauses s 0 with Some (a, j) -> Some (a, String.sub s 0 j) | None -> None in
      let got = lex_once lexer s in
      let show = function Some (a, l) -> a ^ ":" ^ str l | None -> "no match" in
      check (tag ^ " first") (want = got) (fun () ->
          Printf.sprintf "%s on %s: model %s, lexer %s" (show_clauses clauses) (str s) (show want) (show got));
      match model_all clauses s with
      | None -> incr skipped
      | Some want ->
          let got = lex_all lexer s in
          check (tag ^ " all") (want = got) (fun () ->
              Printf.sprintf "%s on %s: model %s, lexer %s" (show_clauses clauses) (str s)
                (String.concat " " want) (String.concat " " got))) all_strings

let () =
  for _ = 1 to 300 do
    let clauses = List.init (1 + rnd 4) (fun k -> (Printf.sprintf "c%d" k, gen (rnd 3))) @ [ eof_clause ] in
    compare_all "lex" (build clauses) clauses
  done;
  Printf.printf "tokenizations skipped (an empty match before the end): %d\n" !skipped;
  List.iter (fun op ->
      let k = try Hashtbl.find bad op with Not_found -> 0 in
      line op (string_of_int (Hashtbl.find steps op) ^ " checks")
        (if k = 0 then "0 disagree" else Printf.sprintf "%d disagree (first: %s)" k (Hashtbl.find first op)))
    (List.rev !order)

(* A $ or \' that can repeat, at the end of input. Before the fix each
   of these looped forever, reading the end marker again and again;
   they are added with the fix because a test cannot pin a loop. *)
let () =
  let run re inputs =
    let l = snd (L.add_clause (snd (L.add_clause L.empty "c" re)) "eof" "\\'") in
    List.iter (fun s -> line ("lex all " ^ re) (str s) (String.concat " " (lex_all l s))) inputs in
  run "a$*" [ "a"; "a\n"; "a\n\n" ];
  run "a\\'*" [ "a" ];
  run "a($)+" [ "a"; "a\r\n" ];
  run "($|a)*" [ "a"; "a\na" ];
  run "a$$*" [ "a" ]
