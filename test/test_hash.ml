open Pin
module H = Lm_hash

let code f =
  let b = H.HashCode.create () in
  f b;
  string_of_int (H.HashCode.code b)

let digest f =
  let b = H.HashDigest.create () in
  f b;
  Digest.to_hex (H.HashDigest.digest b)

let () =
  line "HashCode" "empty" (code (fun _ -> ()));
  line "HashCode" "add_int 0" (code (fun b -> H.HashCode.add_int b 0));
  line "HashCode" "add_int 42" (code (fun b -> H.HashCode.add_int b 42));
  line "HashCode" "add_int (-1)" (code (fun b -> H.HashCode.add_int b (-1)));
  line "HashCode" "add_int max_int" (code (fun b -> H.HashCode.add_int b max_int));
  line "HashCode" "add_bits 0x7ff" (code (fun b -> H.HashCode.add_bits b 0x7ff));
  line "HashCode" "add_int32 7l" (code (fun b -> H.HashCode.add_int32 b 7l));
  line "HashCode" "add_int64 7L" (code (fun b -> H.HashCode.add_int64 b 7L));
  line "HashCode" "add_float 1.5" (code (fun b -> H.HashCode.add_float b 1.5));
  line "HashCode" "add_string \"\"" (code (fun b -> H.HashCode.add_string b ""));
  line "HashCode" "add_string \"metaprl\"" (code (fun b -> H.HashCode.add_string b "metaprl"));
  line "HashCode" "1 then 2" (code (fun b -> H.HashCode.add_int b 1; H.HashCode.add_int b 2));
  line "HashCode" "2 then 1" (code (fun b -> H.HashCode.add_int b 2; H.HashCode.add_int b 1));
  line "HashDigest" "empty" (digest (fun _ -> ()));
  line "HashDigest" "add_int 42" (digest (fun b -> H.HashDigest.add_int b 42));
  line "HashDigest" "add_bool true" (digest (fun b -> H.HashDigest.add_bool b true));
  line "HashDigest" "add_char 'x'" (digest (fun b -> H.HashDigest.add_char b 'x'));
  line "HashDigest" "add_string \"metaprl\"" (digest (fun b -> H.HashDigest.add_string b "metaprl"));
  line "HashDigest" "add_substring \"metaprl\" 4 3" (digest (fun b -> H.HashDigest.add_substring b "metaprl" 4 3));
  line "HashDigest" "add_string \"prl\"" (digest (fun b -> H.HashDigest.add_string b "prl"));
  line "HashDigest" "add_float 1.5" (digest (fun b -> H.HashDigest.add_float b 1.5));
  line "HashDigest" "add_int64 7L" (digest (fun b -> H.HashDigest.add_int64 b 7L));
  line "hash_combine" "1 2" (string_of_int (H.hash_combine 1 2));
  line "hash_combine" "2 1" (string_of_int (H.hash_combine 2 1));
  line "hash_int_list" "0 [1; 2; 3]" (string_of_int (H.hash_int_list 0 [1; 2; 3]));
  line "hash_int_list" "0 []" (string_of_int (H.hash_int_list 0 []));
  line "compare_int_list" "[1; 2] [1; 3]" (string_of_int (H.compare_int_list [1; 2] [1; 3]));
  line "compare_int_list" "[1; 2] [1]" (string_of_int (H.compare_int_list [1; 2] [1]));
  line "compare_int_list" "[] []" (string_of_int (H.compare_int_list [] []))
