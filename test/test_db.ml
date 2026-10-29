(* Lm_db against a model, over a scratch file, from a fixed LCG. On one
   host every entry carries this host's name, so add (which first removes
   any entry with the same tag from this host) keeps at most one entry per
   tag, and find needs the tag, the magic and the digest to match. Random
   add / remove / find steps, both host modes; then the predicate API
   (append_entry, find_entry, remove_entry) on a fixed history. *)
open Pin
module D = Lm_db

let seed = ref 109
let rnd n = seed := (!seed * 1103515245 + 12345) land 0x3fffffff; (!seed lsr 4) mod n

(* digests are 16 bytes (MD5) *)
let d1 = Digest.string "1" and d2 = Digest.string "2"

(* tags for the predicate API start at first_entry_tag; below it an
   entry is read in the old format, one magic string *)
let e5 = D.first_entry_tag + 5 and e6 = D.first_entry_tag + 6 and e7 = D.first_entry_tag + 7

let with_file f =
  let name = Filename.temp_file "test_db" ".db" in
  let fd = Unix.openfile name [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC ] 0o600 in
  let r = try f fd name with e -> Unix.close fd; Sys.remove name; raise e in
  Unix.close fd; Sys.remove name; r

let () =
  let n = 600 and bad_find = ref 0 and bad_size = ref 0 and first = ref "" in
  with_file (fun fd name ->
      let m = Hashtbl.create 4 and h = Buffer.create 64 in
      for _ = 1 to n do
        let tag = rnd 3 and magic = if rnd 2 = 0 then "m1" else "m2" and digest = if rnd 2 = 0 then d1 else d2 in
        let mode = if rnd 2 = 0 then D.HostIndependent else D.HostDependent in
        (match rnd 4 with
         | 0 | 1 -> let v = rnd 1000 in D.add fd name (tag, mode) magic digest v; Hashtbl.replace m tag (magic, digest, v);
             Buffer.add_string h (Printf.sprintf "add %d %s %s %d; " tag magic (if digest = d1 then "d1" else "d2") v)
         | 2 -> D.remove fd name (tag, mode) magic; Hashtbl.remove m tag; Buffer.add_string h (Printf.sprintf "remove %d %s; " tag magic)
         | _ -> ());
        (* find every (tag, magic, digest) *)
        List.iter (fun tag -> List.iter (fun magic -> List.iter (fun digest ->
            let want = match Hashtbl.find_opt m tag with Some (mg, dg, v) when mg = magic && dg = digest -> Some v | _ -> None in
            let got = try Some (D.find fd name (tag, D.HostIndependent) magic digest : int) with Not_found -> None in
            if got <> want then begin incr bad_find; if !first = "" then first := Buffer.contents h end)
            [ d1; d2 ]) [ "m1"; "m2" ]) [ 0; 1; 2 ];
        (* the file holds one entry per live tag: count them with find_entry *)
        let count = ref 0 in
        (try ignore (D.find_entry fd name (fun _ _ _ _ -> incr count; false) : int) with Not_found -> ());
        if !count <> Hashtbl.length m then begin incr bad_size; if !first = "" then first := Buffer.contents h end
      done);
  line "steps where a find disagrees with the model" (string_of_int n ^ " steps") (string_of_int !bad_find);
  line "steps where the file holds other than one entry per live tag" "" (string_of_int !bad_size);
  line "first disagreement" "" (if !first = "" then "none" else str (String.sub !first 0 (min 200 (String.length !first))));
  (* the predicate API *)
  with_file (fun fd name ->
      D.append_entry fd name e5 [ ("A", "1") ] d1 "first";
      D.append_entry fd name e5 [ ("A", "2") ] d1 "second";
      D.append_entry fd name e6 [] d1 "third";
      let find p = guard (fun () -> D.find_entry fd name p) in
      line "find_entry tag 5 with A=2" "" (find (fun t s _ _ -> t = e5 && s = [ ("A", "2") ]));
      line "find_entry tag 5 (first match)" "" (find (fun t _ _ _ -> t = e5));
      line "find_entry tag 7" "" (find (fun t _ _ _ -> t = e7));
      D.remove_entry fd name (fun t s _ _ -> t = e5 && s = [ ("A", "1") ]);
      line "remove_entry tag 5 with A=1, then find_entry tag 5" "" (find (fun t _ _ _ -> t = e5));
      line "  then find_entry tag 6" "" (find (fun t _ _ _ -> t = e6));
      D.remove_entry fd name (fun _ _ _ _ -> false);
      line "remove_entry of nothing, then find_entry tag 6" "" (find (fun t _ _ _ -> t = e6)))
;;
(* the same entries after a remove has written the file's header *)
let () =
  with_file (fun fd name ->
      D.remove fd name (0, D.HostIndependent) "m";
      D.append_entry fd name e5 [ ("A", "1") ] d1 "first";
      D.append_entry fd name e5 [ ("A", "2") ] d1 "second";
      line "after a remove: find_entry tag 5 with A=2" "" (guard (fun () -> D.find_entry fd name (fun t s _ _ -> t = e5 && s = [ ("A", "2") ]))));
  (* on a new file, append_entry, then remove of an absent tag *)
  with_file (fun fd name ->
      D.append_entry fd name e5 [ ("A", "1") ] d1 "first";
      let before = (Unix.fstat fd).Unix.st_size in
      D.remove fd name (0, D.HostIndependent) "m";
      (* entries carry the host name, so sizes are host-dependent: print
         only whether more than the 4-byte header is left *)
      line "new file: append_entry, then remove of another tag: entries before, after" ""
        (Printf.sprintf "%b %b" (before > 4) ((Unix.fstat fd).Unix.st_size > 4)))
