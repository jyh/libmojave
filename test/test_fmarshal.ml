open Pin

(* Lm_fmarshal over a string: the bytes it writes, and what it reads back. *)
module IO = struct
  type t = int
  type in_channel = { s : string; mutable pos : int }
  type out_channel = Buffer.t
  let magic_of_int i = i
  let int_of_magic i = i
  let input_byte ic = let c = ic.s.[ic.pos] in ic.pos <- ic.pos + 1; Char.code c
  let input_buffer ic b off len = Bytes.blit_string ic.s ic.pos b off len; ic.pos <- ic.pos + len
  let output_byte oc i = Buffer.add_char oc (Char.chr (i land 0xff))
  let output_buffer oc s off len = Buffer.add_substring oc s off len
end

module M = Lm_fmarshal.Make (IO)

let hex s = String.concat "" (List.map (fun c -> Printf.sprintf "%02x" (Char.code c)) (List.of_seq (String.to_seq s)))

let rec show = function
  | Lm_fmarshal.Bool b -> "Bool " ^ string_of_bool b
  | Lm_fmarshal.Int i -> "Int " ^ string_of_int i
  | Lm_fmarshal.Magic m -> "Magic " ^ string_of_int m
  | Lm_fmarshal.Float f -> "Float " ^ Printf.sprintf "%h" f
  | Lm_fmarshal.String s -> "String " ^ str s
  | Lm_fmarshal.List l -> "List " ^ list show l
  | Lm_fmarshal.Lm_rawint _ -> "Lm_rawint _"
  | Lm_fmarshal.Lm_rawfloat _ -> "Lm_rawfloat _"

let case name item =
  let b = Buffer.create 16 in
  let written = guard (fun () -> M.marshal b item; hex (Buffer.contents b)) in
  let back = guard (fun () ->
      show (M.unmarshal { IO.s = Buffer.contents b; pos = 0 })) in
  line "marshal" name written;
  line "unmarshal∘marshal" name back

let () =
  case "Bool true" (Lm_fmarshal.Bool true);
  case "Bool false" (Lm_fmarshal.Bool false);
  case "Int 0" (Lm_fmarshal.Int 0);
  case "Int 300" (Lm_fmarshal.Int 300);
  case "Int (-1)" (Lm_fmarshal.Int (-1));
  case "Int max_int" (Lm_fmarshal.Int max_int);
  case "Magic 5" (Lm_fmarshal.Magic 5);
  case "Float 1.5" (Lm_fmarshal.Float 1.5);
  case "Float (-0.)" (Lm_fmarshal.Float (-0.));
  case "String \"\"" (Lm_fmarshal.String "");
  case "String \"metaprl\"" (Lm_fmarshal.String "metaprl");
  case "List []" (Lm_fmarshal.List []);
  case "List [Int 1; String \"a\"; List [Bool true]]"
    (Lm_fmarshal.List [Lm_fmarshal.Int 1; Lm_fmarshal.String "a"; Lm_fmarshal.List [Lm_fmarshal.Bool true]])
