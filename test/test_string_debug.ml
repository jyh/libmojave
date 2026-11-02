(* Lm_string_util's bounds-checked primitives with the "string" debug flag
   on: create, make, sub, blit, set, get against Stdlib's own String /
   Bytes functions on every small argument (buffers of length 0-3, offsets
   and lengths -1..4). A call agrees when both raise or both return the
   same thing. The debug messages they print go to stderr, which is sent to
   the null device for the run. *)
open Pin
module S = Lm_string_util

let () =
  Lm_debug.set_debug "string" true;
  let null = Unix.openfile Filename.null [ Unix.O_WRONLY ] 0 in
  Unix.dup2 null Unix.stderr

let res f = match f () with v -> Some v | exception _ -> None
let tried = Hashtbl.create 8 and ok = Hashtbl.create 8 and first = Hashtbl.create 8 and order = ref []
let note name b what =
  if not (List.mem name !order) then order := name :: !order;
  Hashtbl.replace tried name (1 + try Hashtbl.find tried name with Not_found -> 0);
  if b then Hashtbl.replace ok name (1 + try Hashtbl.find ok name with Not_found -> 0)
  else if not (Hashtbl.mem first name) then Hashtbl.replace first name (what ())

let range = [ -1; 0; 1; 2; 3; 4 ]
let strs = [ ""; "a"; "ab"; "abc" ]

let () =
  List.iter (fun n ->
      note "create" (res (fun () -> Bytes.length (S.create "t" n)) = res (fun () -> Bytes.length (Bytes.create n))) (fun () -> string_of_int n);
      note "make" (res (fun () -> S.make "t" n 'x') = res (fun () -> String.make n 'x')) (fun () -> string_of_int n)) range;
  List.iter (fun s ->
      List.iter (fun i ->
          let what () = Printf.sprintf "%S %d" s i in
          note "get" (res (fun () -> S.get "t" s i) = res (fun () -> String.get s i)) what;
          let b1 = Bytes.of_string s and b2 = Bytes.of_string s in
          let r1 = res (fun () -> S.set "t" b1 i 'z') and r2 = res (fun () -> Bytes.set b2 i 'z') in
          note "set" (r1 = r2 && Bytes.equal b1 b2) what;
          List.iter (fun len ->
              let what () = Printf.sprintf "%S %d %d" s i len in
              note "sub" (res (fun () -> S.sub "t" s i len) = res (fun () -> String.sub s i len)) what;
              List.iter (fun d ->
                  List.iter (fun j ->
                      let what () = Printf.sprintf "%S %d into %S at %d, %d" s i d j len in
                      let f1 = Bytes.of_string s and t1 = Bytes.of_string d in
                      let f2 = Bytes.of_string s and t2 = Bytes.of_string d in
                      let r1 = res (fun () -> S.blit "t" f1 i t1 j len) and r2 = res (fun () -> Bytes.blit f2 i t2 j len) in
                      note "blit" (r1 = r2 && Bytes.equal t1 t2) what) range) strs) range) range) strs;
  List.iter (fun name ->
      let t = Hashtbl.find tried name and o = try Hashtbl.find ok name with Not_found -> 0 in
      line ("with the string debug flag on, " ^ name) "vs Stdlib" (Printf.sprintf "%d of %d agree" o t);
      match Hashtbl.find_opt first name with None -> () | Some w -> Printf.printf "  first difference: %s\n" w)
    (List.rev !order)
