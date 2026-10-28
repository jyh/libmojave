(*
 * Lm_config.code says whether the program is bytecode or native. The C
 * stubs lm_byte_detect and lm_native_detect return it as an OCaml value.
 * This test runs in both modes: the answer must match Sys.backend_type,
 * must be an immediate (not a pointer), and must survive a full GC, which
 * on OCaml 5 scans module globals such as Lm_config.code.
 *)
let name = function
   Lm_config.ByteCode -> "ByteCode"
 | Lm_config.NativeCode -> "NativeCode"

let () =
   let expected =
      match Sys.backend_type with
         Sys.Native -> Lm_config.NativeCode
       | Sys.Bytecode -> Lm_config.ByteCode
       | Sys.Other _ -> Lm_config.ByteCode
   in
   let immediate = Obj.is_int (Obj.repr Lm_config.code) in
      Printf.printf "immediate: %b\n%!" immediate;
      Gc.full_major ();
      Printf.printf "survived a full GC\n%!";
      Printf.printf "matches backend: %b\n" (immediate && Lm_config.code = expected);
      Printf.printf "expected: %s\n" (name expected)
