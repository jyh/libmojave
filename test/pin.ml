(* Helpers for pinning tests: each line records an input and the library's
   answer, so the .expected file is the behaviour as it stood when written. *)
let str s = Printf.sprintf "%S" s
let list f l = "[" ^ String.concat "; " (List.map f l) ^ "]"
let opt f = function None -> "None" | Some x -> "Some " ^ f x
let line name args result = Printf.printf "%s %s = %s\n%!" name args result
let guard f = try f () with e -> "EXN " ^ Printexc.to_string e
