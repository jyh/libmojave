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
