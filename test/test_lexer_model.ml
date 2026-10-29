(* Lm_lexer against a model, from a fixed LCG.

   Random lexers of 1-4 clauses, each a random regular expression over the
   alphabet a b c (characters, ".", brackets, negated brackets,
   alternation, sequence, *, +, ?, {m,n}), then the end-of-input clause
   \' last. A brute-force matcher computes every end position of each
   clause from a start; the lexer should take the longest match, the
   earliest clause on a tie (choose = min).

   For every lexer and every string over {a,b,c} of length <= 5:
   - first: one lex call from the start: the clause and the lexeme, or
     no match;
   - all: lex repeatedly to end of input (at most 8 calls), when the model
     never takes an empty match before the end.
   Then the same after remove_clause and union, against the lexer built
   directly from the resulting clauses. *)
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
  List.fold_left (fun l (a, r) ->
      snd (L.add_clause l a (match r with None -> "\\'" | Some r -> render r))) L.empty clauses

let show_clauses clauses =
  String.concat " " (List.map (fun (a, r) -> a ^ "=" ^ (match r with None -> "\\'" | Some r -> render r)) clauses)

let lex_once lexer s =
  let ch = Lm_channel.of_string s in
  match L.lex lexer ch with
  | (a, _, lexeme, _) -> Some (a, lexeme)
  | exception Failure _ -> None

let lex_all lexer s =
  let ch = Lm_channel.of_string s in
  let rec loop acc k =
    if k = 8 then List.rev ("..." :: acc)
    else match L.lex lexer ch with
      | ("eof", _, _, _) -> List.rev ("EOF" :: acc)
      | (a, _, lexeme, _) -> loop ((a ^ ":" ^ lexeme) :: acc) (k + 1)
      | exception Failure _ -> List.rev ("FAIL" :: acc) in
  loop [] 0

(* The model's tokenization, or None if it would take an empty match
   before the end. *)
let model_all clauses s =
  let rec loop acc i k =
    if k = 8 then Some (List.rev ("..." :: acc))
    else match model clauses s i with
      | Some ("eof", _) -> Some (List.rev ("EOF" :: acc))
      | Some (_, e) when e = i -> None
      | Some (a, e) -> loop ((a ^ ":" ^ String.sub s i (e - i)) :: acc) e (k + 1)
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
      let want = match model clauses s 0 with Some (a, e) -> Some (a, String.sub s 0 e) | None -> None in
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
    let clauses = List.init (1 + rnd 4) (fun k -> (Printf.sprintf "c%d" k, Some (gen (rnd 3)))) @ [ eof_clause ] in
    let lexer = build clauses in
    compare_all "lex" lexer clauses;
    (* remove one non-eof clause *)
    let victim = fst (List.nth clauses (rnd (List.length clauses - 1))) in
    let rest = List.filter (fun (a, _) -> a <> victim) clauses in
    compare_all "remove_clause" (L.remove_clause lexer victim) rest;
    (* union with clauses that have new names, added after *)
    let extra = List.init (1 + rnd 2) (fun k -> (Printf.sprintf "d%d" k, Some (gen (rnd 3)))) in
    let lexer2 = build extra in
    compare_all "union" (L.union lexer lexer2) (clauses @ extra)
  done

let () =
  Printf.printf "tokenizations skipped (the model takes an empty match before the end): %d\n" !skipped;
  List.iter (fun op ->
      let k = try Hashtbl.find bad op with Not_found -> 0 in
      line op (string_of_int (Hashtbl.find steps op) ^ " checks")
        (if k = 0 then "0 disagree" else Printf.sprintf "%d disagree (first: %s)" k (Hashtbl.find first op)))
    (List.rev !order)
