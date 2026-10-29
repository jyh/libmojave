(* Lm_lexer's search, searchto and matches against a model, from a fixed
   LCG. The same random lexers as test_lexer_model.ml (1-4 random regular
   expressions over a b c, choose = min), without the end-of-input clause.
   search should find the leftmost position where some clause matches,
   then the longest match there, the earliest clause on a tie, and return
   the text it skipped; searchto is the same, but with no match it returns
   the rest of the input (LexSkipped) or LexEOF when nothing is left;
   matches is whether search finds anything. Each is called once from
   the start of every string over {a,b,c} of length <= 5, and then
   searchto repeatedly to the end (at most 8 calls) when the model never
   takes an empty match. metaprl's filter_grammar calls searchto to find
   the end of a quotation. *)
open Pin

module Action = struct
  type action = string
  let pp_print_action out a = Lm_printf.pp_print_string out a
  let hash = Hashtbl.hash
  let compare = compare
  let choose = min
end
module L = Lm_lexer.MakeLexer (Lm_channel.LexerInput) (Action)

let seed = ref 89
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

let alphabet = [| 'a'; 'b'; 'c' |]

let rec gen depth =
  if depth = 0 then
    match rnd 6 with
    | 0 -> Any
    | 1 -> Set (List.sort_uniq compare [ alphabet.(rnd 3); alphabet.(rnd 3) ])
    | 2 -> NotSet [ alphabet.(rnd 3) ]
    | _ -> Chr alphabet.(rnd 3)
  else
    match rnd 9 with
    | 0 | 1 -> Seq (List.init (2 + rnd 2) (fun _ -> gen (depth - 1)))
    | 2 -> Alt (List.init (2 + rnd 2) (fun _ -> gen (depth - 1)))
    | 3 -> Star (gen (depth - 1))
    | 4 -> Plus (gen (depth - 1))
    | 5 -> Opt (gen (depth - 1))
    | 6 -> let m = rnd 3 in Rep (gen (depth - 1), m, m + rnd 2)
    | _ -> gen 0

let rec render r =
  match r with
  | Chr c -> String.make 1 c
  | Any -> "."
  | Set l -> "[" ^ String.init (List.length l) (List.nth l) ^ "]"
  | NotSet l -> "[^" ^ String.init (List.length l) (List.nth l) ^ "]"
  | Seq l -> String.concat "" (List.map (fun r -> match r with Alt _ -> "(" ^ render r ^ ")" | _ -> render r) l)
  | Alt l -> String.concat "|" (List.map render l)
  | Star r -> atom r ^ "*"
  | Plus r -> atom r ^ "+"
  | Opt r -> atom r ^ "?"
  | Rep (r, m, n) -> atom r ^ (if m = n then Printf.sprintf "{%d}" m else Printf.sprintf "{%d,%d}" m n)
and atom r =
  match r with
  | Chr _ | Any | Set _ | NotSet _ -> render r
  | _ -> "(" ^ render r ^ ")"

(* Every end position of r matched from i in s. *)
let rec ends r s i =
  let n = String.length s in
  let uniq l = List.sort_uniq compare l in
  match r with
  | Chr c -> if i < n && s.[i] = c then [ i + 1 ] else []
  | Any -> if i < n then [ i + 1 ] else []
  | Set l -> if i < n && List.mem s.[i] l then [ i + 1 ] else []
  | NotSet l -> if i < n && not (List.mem s.[i] l) then [ i + 1 ] else []
  | Seq l -> List.fold_left (fun ps r -> uniq (List.concat_map (ends r s) ps)) [ i ] l
  | Alt l -> uniq (List.concat_map (fun r -> ends r s i) l)
  | Star r ->
      let rec close seen frontier =
        let next = List.filter (fun p -> not (List.mem p seen)) (uniq (List.concat_map (ends r s) frontier)) in
        if next = [] then seen else close (uniq (seen @ next)) next in
      close [ i ] [ i ]
  | Plus r -> ends (Seq [ r; Star r ]) s i
  | Opt r -> uniq (i :: ends r s i)
  | Rep (r, m, k) ->
      let rec go j ps acc =
        let acc = if j >= m then uniq (acc @ ps) else acc in
        if j = k then acc else go (j + 1) (uniq (List.concat_map (ends r s) ps)) acc in
      go 0 [ i ] []

let eof_clause = ("eof", None)

(* The model's lex from i: the longest end over the clauses, the first
   clause on a tie. At the end of the input the eof clause wins over an
   empty match by any clause, earlier or not: the lexer treats \' as
   matching the end marker, so it is the longer match (a nullable clause
   would otherwise match the empty string at the end forever). *)
let model clauses s i =
  let best = ref None in
  if i = String.length s && List.mem eof_clause clauses then best := Some ("eof", i) else
  List.iter (fun (a, r) ->
      let es = match r with None -> if i = String.length s then [ i ] else [] | Some r -> ends r s i in
      List.iter (fun e -> match !best with
          | Some (_, e') when e' >= e -> ()
          | _ -> best := Some (a, e)) es) clauses;
  !best

let build clauses =
  List.fold_left (fun l (a, r) -> snd (L.add_clause l a (match r with None -> "\\'" | Some r -> render r))) L.empty clauses

let show_clauses clauses =
  String.concat " " (List.map (fun (a, r) -> a ^ "=" ^ (match r with None -> "\\'" | Some r -> render r)) clauses)

(* The model's search from i: (clause, skipped, lexeme, next position). *)
let model_search clauses s i =
  let n = String.length s in
  let rec from j =
    if j > n then None
    else match model clauses s j with
      | Some (a, e) -> Some (a, String.sub s i (j - i), String.sub s j (e - j), e)
      | None -> from (j + 1) in
  from i

let show_search = function
  | Some (a, sk, lx) -> Printf.sprintf "%s skipped %S lexeme %S" a sk lx
  | None -> "None"

let show_to = function
  | L.LexEOF -> "LexEOF"
  | L.LexSkipped (_, sk) -> Printf.sprintf "LexSkipped %S" sk
  | L.LexMatched (a, _, sk, lx, _) -> Printf.sprintf "%s skipped %S lexeme %S" a sk lx

let model_to clauses s i =
  match model_search clauses s i with
  | Some (a, sk, lx, e) -> (Printf.sprintf "%s skipped %S lexeme %S" a sk lx, e)
  | None -> if i = String.length s then ("LexEOF", i) else (Printf.sprintf "LexSkipped %S" (String.sub s i (String.length s - i)), String.length s)

let rec strings n =
  if n = 0 then [ "" ]
  else let shorter = strings (n - 1) in
    shorter @ List.concat_map (fun s -> if String.length s = n - 1 then List.map (fun c -> s ^ String.make 1 c) (Array.to_list alphabet) else []) shorter
let all_strings = List.sort_uniq compare (strings 5)

let skipped = ref 0

let () =
  for _ = 1 to 300 do
    let clauses = List.init (1 + rnd 4) (fun k -> (Printf.sprintf "c%d" k, Some (gen (rnd 3)))) in
    let lexer = build clauses in
    List.iter (fun s ->
        let want = Option.map (fun (a, sk, lx, _) -> (a, sk, lx)) (model_search clauses s 0) in
        let got = match L.search lexer (Lm_channel.of_string s) with
          | Some (a, _, sk, lx, _) -> Some (a, sk, lx) | None -> None | exception e -> Some ("EXN", Printexc.to_string e, "") in
        check "search" (want = got) (fun () ->
            Printf.sprintf "%s on %S: model %s, search %s" (show_clauses clauses) s (show_search want) (show_search got));
        let gotm = try L.matches lexer (Lm_channel.of_string s) with _ -> not (want <> None) in
        check "matches" (gotm = (want <> None)) (fun () ->
            Printf.sprintf "%s on %S: model %b" (show_clauses clauses) s (want <> None));
        let wt, _ = model_to clauses s 0 in
        let gt = try show_to (L.searchto lexer (Lm_channel.of_string s)) with e -> "EXN " ^ Printexc.to_string e in
        check "searchto" (wt = gt) (fun () ->
            Printf.sprintf "%s on %S: model %s, searchto %s" (show_clauses clauses) s wt gt);
        (* repeated searchto to the end *)
        let rec mloop acc i k =
          if k = 8 then Some (List.rev ("..." :: acc))
          else match model_to clauses s i with
            | ("LexEOF", _) -> Some (List.rev ("LexEOF" :: acc))
            | (r, e) when e = i -> ignore r; None
            | (r, e) -> mloop (r :: acc) e (k + 1) in
        match mloop [] 0 0 with
        | None -> incr skipped
        | Some want ->
            let ch = Lm_channel.of_string s in
            let rec gloop acc k =
              if k = 8 then List.rev ("..." :: acc)
              else match L.searchto lexer ch with
                | L.LexEOF -> List.rev ("LexEOF" :: acc)
                | r -> gloop (show_to r :: acc) (k + 1)
                | exception e -> List.rev (("EXN " ^ Printexc.to_string e) :: acc) in
            let got = gloop [] 0 in
            check "searchto all" (want = got) (fun () ->
                Printf.sprintf "%s on %S: model %s, searchto %s" (show_clauses clauses) s
                  (String.concat "; " want) (String.concat "; " got))) all_strings
  done

let () =
  Printf.printf "repeated searches skipped (the model takes an empty match before the end): %d\n" !skipped;
  List.iter (fun op ->
      let k = try Hashtbl.find bad op with Not_found -> 0 in
      line op (string_of_int (Hashtbl.find steps op) ^ " checks")
        (if k = 0 then "0 disagree" else Printf.sprintf "%d disagree (first: %s)" k (Hashtbl.find first op)))
    (List.rev !order)

(* Fixed cases, under choose = max as metaprl's filter_grammar has it:
   repeated searchto over quotation-like delimiters. *)
module AMax = struct include Action let choose = max end
module LM = Lm_lexer.MakeLexer (Lm_channel.LexerInput) (AMax)

let () =
  let run name cl inputs =
    let l = List.fold_left (fun l (a, r) -> snd (LM.add_clause l a r)) LM.empty cl in
    List.iter (fun s ->
        let ch = Lm_channel.of_string s in
        let rec go k acc =
          if k = 6 then List.rev ("..." :: acc)
          else match LM.searchto l ch with
            | LM.LexEOF -> List.rev ("LexEOF" :: acc)
            | LM.LexSkipped (_, x) -> go (k + 1) (Printf.sprintf "LexSkipped %S" x :: acc)
            | LM.LexMatched (a, _, sk, lx, _) -> go (k + 1) (Printf.sprintf "%s[%S|%S]" a sk lx :: acc) in
        line ("searchto " ^ name) (str s) (String.concat " " (go 0 []))) inputs in
  run ">>" [ ("end", ">>") ] [ "x >> y >>"; ">>>"; "a>>>>b" ];
  run "<< and >>" [ ("open", "<<"); ("close", ">>") ] [ "a << b >> c >>"; "<<<x>>>" ];
  run "a{2,3}" [ ("c", "a{2,3}") ] [ "aaaa"; "baaab" ];
  run "ab|bcd" [ ("c", "ab|bcd") ] [ "abcd" ]
