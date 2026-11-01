(* Lm_readline.readline without GNU readline (the dune build never defines
   READLINE) reads stdin with fgets. The test runs itself as a child whose
   stdin is a pipe (Unix.dup2 onto fd 0 does not reach the C runtime's
   stdin on Windows); the parent writes lines of chosen lengths into it.
   Each answer is printed as its length, the letter it is made of, and
   whether it ends in a newline. The last line has no newline and is
   followed by end of file. *)
open Pin

let lengths = [0; 10; 1021; 1022; 1023; 1024; 3000]

let describe s =
  let n = String.length s in
  let nl = n > 0 && s.[n - 1] = '\n' in
  let body = if nl then String.sub s 0 (n - 1) else s in
  let letters = String.to_seq body |> List.of_seq |> List.sort_uniq compare |> List.map (String.make 1) in
  Printf.sprintf "length %d, letters [%s], newline %b" (String.length body) (String.concat "" letters) nl

let child () =
  let rec loop i =
    match Lm_readline.readline "" with
    | s -> line "readline" (string_of_int i) (describe s); loop (i + 1)
    | exception End_of_file -> line "readline" (string_of_int i) "End_of_file"
  in
  loop 1

let parent () =
  let buf = Buffer.create 8192 in
  List.iteri (fun i n ->
      Buffer.add_string buf (String.make n (Char.chr (Char.code 'a' + i)));
      Buffer.add_char buf '\n') lengths;
  Buffer.add_string buf "tail";
  let s = Buffer.contents buf in
  let r, w = Unix.pipe ~cloexec:true () in
  let pid = Unix.create_process Sys.executable_name [| Sys.executable_name; "child" |] r Unix.stdout Unix.stderr in
  Unix.close r;
  let k = Unix.write_substring w s 0 (String.length s) in
  assert (k = String.length s);
  Unix.close w;
  match Unix.waitpid [] pid with
  | _, Unix.WEXITED 0 -> ()
  | _ -> print_endline "child failed"

let () = if Array.length Sys.argv > 1 && Sys.argv.(1) = "child" then child () else parent ()
