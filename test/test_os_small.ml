(* Small modules with C or OS parts, by property, with output that is the
   same on every platform (nothing prints a host name, a pid or a path):
   - Lm_nocompare: comparing, hashing and marshaling its two values;
   - Lm_id: ids are distinct, carry this process's pid, and count up;
   - Lm_fs_case_sensitive: where available (macOS, which CI does not
     run), its answer for a directory agrees with a probe (create a file,
     look for it in other case);
   - Lm_unix_util: mkdirhier (and again, on what exists), copy_file,
     really_read, openfile. *)
open Pin

let () =
  let open Lm_nocompare in
  line "compare nocompare nocompare; = ; Hashtbl.hash" "" (String.concat " | " [
      guard (fun () -> string_of_int (compare nocompare nocompare));
      guard (fun () -> string_of_bool (nocompare = nocompare));
      guard (fun () -> ignore (Hashtbl.hash nocompare); "hashed") ]);
  line "Marshal nomarshal; Marshal nocompare" "" (String.concat " | " [
      guard (fun () -> ignore (Marshal.to_string nomarshal []); "marshaled");
      guard (fun () -> ignore (Marshal.to_string nocompare []); "marshaled") ]);
  line "it == nocompare" "" (string_of_bool (it == nocompare))

let () =
  let ids = List.init 5 (fun _ -> Lm_id.string_of_id (Lm_id.create ())) in
  let parts s = match List.rev (String.split_on_char ':' s) with
    | n :: pid :: _ -> int_of_string_opt pid, int_of_string_opt n
    | _ -> None, None in
  let pids = List.map (fun s -> fst (parts s)) ids and ns = List.map (fun s -> snd (parts s)) ids in
  line "Lm_id: 5 ids distinct; pid field is getpid; index counts by 1" "" (String.concat " " [
      string_of_bool (List.length (List.sort_uniq compare ids) = 5);
      string_of_bool (List.for_all (( = ) (Some (Unix.getpid ()))) pids);
      string_of_bool (match ns with
          | Some a :: rest -> List.for_all2 (fun k n -> n = Some (a + k)) (List.init (List.length rest) succ) rest
          | _ -> false) ])

let () =
  let dir = "oscs" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let f = Filename.concat dir "Probe" in
  close_out (open_out f);
  let probe = not (Sys.file_exists (Filename.concat dir "pROBE")) in
  (* the same line everywhere: true where it is not available (Linux, Windows) *)
  line "Lm_fs_case_sensitive: not available, or agrees with a file probe" "" (guard (fun () ->
      string_of_bool (not Lm_fs_case_sensitive.available || Lm_fs_case_sensitive.case_sensitive dir = probe)));
  Sys.remove f

let () =
  let module U = Lm_unix_util in
  let d = Filename.concat "osu" (Filename.concat "a" (Filename.concat "b" "c")) in
  line "mkdirhier osu/a/b/c, then is a directory; again" "" (guard (fun () ->
      U.mkdirhier d;
      let ok = Sys.is_directory d in
      U.mkdirhier d;
      string_of_bool ok ^ " " ^ string_of_bool (Sys.is_directory d)));
  let src = Filename.concat "osu" "src.bin" and dst = Filename.concat "osu" "dst.bin" in
  let data = String.init 70000 (fun i -> Char.chr ((i * 7) land 255)) in
  let oc = open_out_bin src in output_string oc data; close_out oc;
  line "copy_file of a 70,000-byte file: same bytes" "" (guard (fun () ->
      U.copy_file src dst 0o644;
      let ic = open_in_bin dst in
      let s = really_input_string ic (in_channel_length ic) in
      close_in ic; string_of_bool (s = data)));
  line "openfile + really_read 10 bytes at offset 5" "" (guard (fun () ->
      let fd = U.openfile src [ Unix.O_RDONLY ] 0 in
      let b = Bytes.make 15 '.' in
      U.really_read fd b 5 10;
      Unix.close fd;
      string_of_bool (Bytes.sub_string b 5 10 = String.sub data 0 10 && Bytes.sub_string b 0 5 = ".....")));
  line "really_read past the end of a 3-byte file" "" (guard (fun () ->
      let f3 = Filename.concat "osu" "three" in
      let oc = open_out_bin f3 in output_string oc "abc"; close_out oc;
      let fd = U.openfile f3 [ Unix.O_RDONLY ] 0 in
      let b = Bytes.create 5 in
      Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> U.really_read fd b 0 5; "read 5")));
  Sys.remove src; Sys.remove dst
