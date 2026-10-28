(* Lm_ctype's six C primitives (five here; set_locale runs at load), checked on every ASCII character against
   the C locale's definitions of the <ctype.h> classes (ASCII classes do
   not change between the C locale and the usual UTF-8 locales), and
   Lm_ctype.quote on a few strings. *)
let digit c = c >= '0' && c <= '9'
let upper c = c >= 'A' && c <= 'Z'
let lower c = c >= 'a' && c <= 'z'
let alnum c = digit c || upper c || lower c
let graph c = Char.code c >= 0x21 && Char.code c <= 0x7e
let print c = Char.code c >= 0x20 && Char.code c <= 0x7e

(* set_locale is not in the interface; the module calls it when loaded. *)
let () =
  let bad = ref 0 in
  let check name f g c =
    if f c <> g c then begin
      incr bad;
      Printf.printf "MISMATCH %s %d: C says %b\n" name (Char.code c) (f c)
    end
  in
  for i = 0 to 127 do
    let c = Char.chr i in
    check "is_digit" Lm_ctype.is_digit digit c;
    check "is_upper" Lm_ctype.is_upper upper c;
    check "is_alnum" Lm_ctype.is_alnum alnum c;
    check "is_graph" Lm_ctype.is_graph graph c;
    check "is_print" Lm_ctype.is_print print c
  done;
  Printf.printf "ascii: %d mismatches over 128 characters x 5 classes\n" !bad;
  List.iter (fun s -> Printf.printf "quote %S = %s\n" s (Lm_ctype.quote s))
    [ "abc"; "a b"; ""; "x\"y"; "tab\there" ];
  (* No bytes above 127 here: whether they print depends on the locale. *)
  Printf.printf "is_capitalized %b %b\n"
    (Lm_ctype.is_capitalized "Abc") (Lm_ctype.is_capitalized "abc")
