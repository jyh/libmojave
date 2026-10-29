(* Lm_marshal_buf.Buf: the word buffer under Lm_marshal_shared (whose
   XMarshal exports nothing, so this is the module's only reachable use).
   Nothing is written here: in this version every write to a fresh buffer
   loops forever (see the commit message). A fresh block's contents are
   uninitialised, so they are not read. *)
open Pin
module B = Lm_marshal_buf.Buf

let () =
  line "word_size" "" (string_of_int B.word_size);
  line "create, to_static, to_read, free" "" (guard (fun () ->
      B.free (B.to_read (B.to_static (B.create ()))); "ok"))

(* Round trip: random sequences of words and byte strings (some longer
   than a block, so writes and reads cross blocks), read back in order.
   A word holds an int below 2^31, an int2 a one-bit tag and an int below
   2^29, a tagged value a byte tag and an int below 2^22; read_value_type
   must name the kind. Per check: sequences that fail, and the first
   mismatch. *)
let seed = ref 71
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 10) mod n

type op = Int of int | Int2 of int * int | Tag of int * int | Raw of string

let gen () =
  match rnd 10 with
  | 0 | 1 | 2 -> Int (if rnd 4 = 0 then (1 lsl 31) - 1 - rnd 3 else rnd (1 lsl 20) * rnd 2048)
  | 3 | 4 -> Int2 (rnd 2, if rnd 4 = 0 then (1 lsl 29) - 1 else rnd (1 lsl 29))
  | 5 | 6 -> Tag (rnd 256, if rnd 4 = 0 then (1 lsl 22) - 1 else rnd (1 lsl 22))
  | 7 | 8 -> Raw (String.init (rnd 7) (fun _ -> Char.chr (rnd 256)))
  | _ -> Raw (String.init (if rnd 8 = 0 then 70000 + rnd 70000 else rnd 3000) (fun _ -> Char.chr (rnd 256)))

let kind = function
  | Lm_marshal_sig.IntValue -> "IntValue"
  | Lm_marshal_sig.TaggedValue -> "TaggedValue"
  | Lm_marshal_sig.Int2Value -> "Int2Value"

let show = function
  | Int i -> Printf.sprintf "Int %d" i
  | Int2 (t, i) -> Printf.sprintf "Int2 (%d, %d)" t i
  | Tag (t, i) -> Printf.sprintf "Tag (%d, %d)" t i
  | Raw s -> Printf.sprintf "Raw (%d bytes)" (String.length s)

let () =
  let n = 300 and bad = ref 0 and first = ref "" and ops_total = ref 0 in
  for _ = 1 to n do
    let ops = List.init (1 + rnd 400) (fun _ -> gen ()) in
    ops_total := !ops_total + List.length ops;
    let w = B.create () in
    List.iter (function
        | Int i -> B.write_int w i
        | Int2 (t, i) -> B.write_int2 w t i
        | Tag (t, i) -> B.write_tag w t i
        | Raw s -> B.write w (Bytes.of_string s) 0 (String.length s)) ops;
    let r = B.to_read (B.to_static w) in
    let rec go k = function
      | [] -> None
      | op :: rest ->
          let got =
            try
              match op with
              | Int i ->
                  let t = kind (B.read_value_type r) in
                  let v = B.read_int r in
                  if t = "IntValue" && v = i then None else Some (Printf.sprintf "%s %d" t v)
              | Int2 (tg, i) ->
                  let t = kind (B.read_value_type r) in
                  let tg' = B.read_int2_tag r in
                  let v = B.read_int2_value r in
                  if t = "Int2Value" && tg' = tg && v = i then None else Some (Printf.sprintf "%s (%d, %d)" t tg' v)
              | Tag (tg, i) ->
                  let t = kind (B.read_value_type r) in
                  let tg' = B.read_value_tag r in
                  let v = B.read_value_value r in
                  if t = "TaggedValue" && tg' = tg && v = i then None else Some (Printf.sprintf "%s (%d, %d)" t tg' v)
              | Raw s ->
                  let b = Bytes.create (String.length s) in
                  B.read r b 0 (String.length s);
                  if Bytes.to_string b = s then None else Some "different bytes"
            with e -> Some ("EXN " ^ Printexc.to_string e) in
          (match got with
           | None -> go (k + 1) rest
           | Some g -> Some (Printf.sprintf "op %d, %s: read %s" k (show op) g)) in
    match go 0 ops with
    | None -> ()
    | Some d -> incr bad; if !first = "" then first := d
  done;
  line "round trip: sequences that fail" (Printf.sprintf "%d sequences, %d values" n !ops_total)
    (Printf.sprintf "%d%s" !bad (if !bad = 0 then "" else " (first: " ^ !first ^ ")"));
  (* outside the ranges: what comes back *)
  let back write read =
    guard (fun () -> let w = B.create () in write w; read (B.to_read (B.to_static w))) in
  line "write_int -1, write_int 2^31: read back" "" (back
      (fun w -> B.write_int w (-1); B.write_int w (1 lsl 31))
      (fun r -> let a = B.read_int r in let b = B.read_int r in Printf.sprintf "%d %d" a b));
  line "write_int2 1 2^29: read back (tag, value)" "" (back
      (fun w -> B.write_int2 w 1 (1 lsl 29))
      (fun r -> let t = B.read_int2_tag r in let v = B.read_int2_value r in Printf.sprintf "%d %d" t v));
  line "write_tag 7 2^22: read back (tag, value)" "" (back
      (fun w -> B.write_tag w 7 (1 lsl 22))
      (fun r -> let t = B.read_value_tag r in let v = B.read_value_value r in Printf.sprintf "%d %d" t v));
  line "write_tag 300 1" "" (back (fun w -> B.write_tag w 300 1) (fun _ -> "written"));
  line "read_int past the end" "" (back (fun w -> B.write_int w 1)
      (fun r -> ignore (B.read_int r); string_of_int (B.read_int r)))
