(* Lm_string_util against models, from a fixed LCG: random strings over
   a b . - space tab newline backslash and a double quote. Each function
   is compared with a reference written from its interface comment or
   from Stdlib; one line per function. Behaviour the pin test
   (test_string_util.ml) already records as odd (unescape has no hex
   escapes, parse_args_list) is not modelled here. *)
open Pin
module S = Lm_string_util

let seed = ref 104
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n

let alphabet = "ab.- \t\n\\\""
let rstring max = String.init (rnd (max + 1)) (fun _ -> alphabet.[rnd (String.length alphabet)])

let bad = Hashtbl.create 8 and first = Hashtbl.create 8 and order = ref [] and steps = Hashtbl.create 8
let check op ok d =
  if not (List.mem op !order) then order := op :: !order;
  Hashtbl.replace steps op (1 + try Hashtbl.find steps op with Not_found -> 0);
  if not ok then begin
    Hashtbl.replace bad op (1 + try Hashtbl.find bad op with Not_found -> 0);
    if not (Hashtbl.mem first op) then Hashtbl.replace first op (d ())
  end

let res f = match f () with v -> Ok v | exception e -> Error (Printexc.to_string e)
let show_res show = function Ok v -> show v | Error e -> "EXN " ^ e
let sl l = "[" ^ String.concat "; " (List.map (Printf.sprintf "%S") l) ^ "]"

(* References. *)
let ref_split delims s =
  (* split on any char of delims; empty pieces kept *)
  let out = ref [] and cur = Buffer.create 8 in
  String.iter (fun c -> if String.contains delims c then (out := Buffer.contents cur :: !out; Buffer.clear cur) else Buffer.add_char cur c) s;
  List.rev (Buffer.contents cur :: !out)

let ref_split_string delim s =
  (* split on each occurrence of delim, left to right *)
  let n = String.length delim in
  let rec go i start acc =
    if i + n > String.length s then List.rev (String.sub s start (String.length s - start) :: acc)
    else if String.sub s i n = delim then go (i + n) (i + n) (String.sub s start (i - start) :: acc)
    else go (i + 1) start acc in
  go 0 0 []

let ref_index_set s set =
  let r = ref None in
  String.iteri (fun i c -> if !r = None && String.contains set c then r := Some i) s;
  match !r with Some i -> i | None -> raise Not_found

let ref_rindex_set s set =
  let r = ref None in
  String.iteri (fun i c -> if String.contains set c then r := Some i) s;
  match !r with Some i -> i | None -> raise Not_found

let ref_trim s =
  let w c = String.contains S.white c in
  let n = String.length s in
  let i = ref 0 and j = ref n in
  while !i < n && w s.[!i] do incr i done;
  while !j > !i && w s.[!j - 1] do decr j done;
  String.sub s !i (!j - !i)

let () =
  for _ = 1 to 4000 do
    let s1 = rstring 8 and s2 = rstring 4 in
    let c = alphabet.[rnd (String.length alphabet)] in
    let off = rnd 10 - 1 in
    let cmp a b = match a, b with Ok x, Ok y -> x = y | Error _, Error _ -> true | _ -> false in
    let go name show got want =
      let g = res got and w = res want in
      check name (cmp g w) (fun () -> Printf.sprintf "%s on %S %S: lib %s, model %s" name s1 s2 (show_res show g) (show_res show w)) in
    (* string_compare is the "faster" order: length first, then the
       characters (not compare's lexicographic order) *)
    go "string_compare (sign)" string_of_int (fun () -> compare (S.string_compare s1 s2) 0)
      (fun () -> compare (compare (String.length s1, s1) (String.length s2, s2)) 0);
    go (Printf.sprintf "equal_substring (off %s)" (if off < 0 then "<0" else "0-8")) string_of_bool
      (fun () -> S.equal_substring s1 off s2)
      (fun () -> off >= 0 && off + String.length s2 <= String.length s1 && String.sub s1 off (String.length s2) = s2);
    go "is_string_prefix" string_of_bool (fun () -> S.is_string_prefix s2 s1)
      (fun () -> String.length s2 <= String.length s1 && String.sub s1 0 (String.length s2) = s2);
    go "hexify then unhexify" (Printf.sprintf "%S") (fun () -> S.unhexify (S.hexify s1)) (fun () -> s1);
    go "strchr" string_of_int (fun () -> S.strchr s1 c) (fun () -> String.index s1 c);
    go "contains" string_of_bool (fun () -> S.contains s1 c) (fun () -> String.contains s1 c);
    go "contains_any" string_of_bool (fun () -> S.contains_any s1 s2) (fun () -> String.exists (fun c -> String.contains s2 c) s1);
    go "index_set" string_of_int (fun () -> S.index_set s1 s2) (fun () -> ref_index_set s1 s2);
    go "rindex_set" string_of_int (fun () -> S.rindex_set s1 s2) (fun () -> ref_rindex_set s1 s2);
    let delims = if s2 = "" then "." else s2 in
    go "split" sl (fun () -> S.split delims s1) (fun () -> ref_split delims s1);
    go "split_string" sl (fun () -> S.split_string delims s1) (fun () -> ref_split_string delims s1);
    go "trim" (Printf.sprintf "%S") (fun () -> S.trim s1) (fun () -> ref_trim s1);
    go "is_white" string_of_bool (fun () -> S.is_white s1) (fun () -> String.for_all (fun c -> String.contains S.white c) s1);
    (* unescape reads OCaml's escapes (decimal \ddd), as String.escaped
       writes them; c_escaped writes C's (octal), recorded below *)
    go "String.escaped then unescape (quoted)" (Printf.sprintf "%S") (fun () -> S.unescape ("\"" ^ String.escaped s1 ^ "\"")) (fun () -> s1)
  done;
  List.iter (fun op ->
      let k = try Hashtbl.find bad op with Not_found -> 0 in
      line op (string_of_int (Hashtbl.find steps op) ^ " cases")
        (if k = 0 then "0 disagree" else Printf.sprintf "%d disagree (first: %s)" k (Hashtbl.find first op)))
    (List.rev !order)

(* Fixed cases. c_escaped writes C escapes (octal \042 for a double
   quote, \b for backspace) and unescape reads OCaml's (decimal \ddd, no
   \b), so they do not round-trip; recorded, not changed. And
   equal_substring with a negative offset. *)
let () =
  List.iter (fun s -> line "unescape (quoted (c_escaped s))" (str s) (str (S.unescape ("\"" ^ S.c_escaped s ^ "\""))))
    [ "\""; "\b"; "'"; "\001" ];
  List.iter (fun (s1, off, s2) ->
      line "equal_substring" (Printf.sprintf "%S %d %S" s1 off s2) (guard (fun () -> string_of_bool (S.equal_substring s1 off s2))))
    [ ("ab", -1, ""); ("ab", 0, "ab"); ("ab", 2, ""); ("ab", 3, "") ]
