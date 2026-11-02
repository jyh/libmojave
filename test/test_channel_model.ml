(* Lm_channel on strings, against a model (2026-11-02 bisect_ppx run:
   lm_channel.ml at 42 %).  300 random texts of letters, spaces and "\n",
   each read through of_string (and of_substring of a random slice) by a
   random sequence of input_char, input_byte, input_line,
   input_entire_line, read, input_buffer and seek, compared step by
   step with a cursor on the string (End_of_file where it ends); and 300
   random sequences of output_char, output_byte, output_string,
   output_buffer and write into create_string, compared with a Buffer
   through to_string.  The generator is a fixed linear congruential
   sequence. *)
let state = ref 2468
let next () = state := (!state * 1103515245 + 12345) land 0x7fffffff; !state lsr 8
let pick n = next () mod n

let text () = String.init (pick 60) (fun _ -> match pick 8 with 0 -> '\n' | 1 -> ' ' | _ -> Char.chr (97 + pick 4))

let tried = Hashtbl.create 16 and ok = Hashtbl.create 16 and first = Hashtbl.create 16 and order = ref []
let note name b what =
  if not (Hashtbl.mem tried name) then order := name :: !order;
  Hashtbl.replace tried name (1 + try Hashtbl.find tried name with Not_found -> 0);
  if b then Hashtbl.replace ok name (1 + try Hashtbl.find ok name with Not_found -> 0)
  else if not (Hashtbl.mem first name) then Hashtbl.replace first name (what ())

let eof f = try Some (f ()) with End_of_file -> None

(* The model's answers on string s from position p: (answer, new position). *)
let m_line s p =
  let n = String.length s in
  if p >= n then None, p
  else match String.index_from_opt s p '\n' with
    | Some i -> Some (String.sub s p (i - p)), i + 1
    | None -> Some (String.sub s p (n - p)), n

let run_input name s ch =
  let p = ref 0 and steps = ref [] and good = ref true in
  let n = String.length s in
  for _ = 1 to 12 do
    if !good then begin
      let k = pick 7 in
      let step, ok_ =
        match k with
        | 0 -> let got = eof (fun () -> Lm_channel.input_char ch) in
               let want = if !p < n then Some s.[!p] else None in
               if want <> None then incr p; "input_char", got = want
        | 1 -> let got = eof (fun () -> Lm_channel.input_byte ch) in
               let want = if !p < n then Some (Char.code s.[!p]) else None in
               if want <> None then incr p; "input_byte", got = want
        | 2 -> let got = eof (fun () -> Lm_channel.input_line ch) in
               let want, p' = m_line s !p in p := p'; "input_line", got = want
        | 3 -> let got = eof (fun () -> Lm_channel.input_entire_line ch) in
               (* the line with its newline *)
               let want, p' = m_line s !p in
               let want = Option.map (fun l -> if p' > !p + String.length l then l ^ "\n" else l) want in
               p := p'; "input_entire_line", got = want
        | 4 -> let len = pick 10 in
               let b = Bytes.make 10 '.' in
               let got = Lm_channel.read ch b 0 len in
               let got_s = Bytes.sub_string b 0 got in
               (* read may return fewer bytes than asked, never more than remain, and 0 only at the end *)
               let okr = got >= 0 && got <= len && got_s = String.sub s !p got && (got > 0 || len = 0 || !p >= n) in
               p := !p + got; "read", okr
        | 5 -> let len = pick 10 in
               let b = Bytes.make 10 '.' in
               let got = eof (fun () -> Lm_channel.input_buffer ch b 0 len; Bytes.sub_string b 0 len) in
               let want = if !p + len <= n then Some (String.sub s !p len) else None in
               (match want with Some _ -> p := !p + len | None -> p := n);
               "input_buffer", got = want
        | _ -> (* recorded as it is: a string channel has no descriptor, so
                  seek and tell raise EINVAL, and reading goes on where it was *)
               let einval f = (try ignore (f ()); false with Unix.Unix_error (Unix.EINVAL, _, _) -> true) in
               let r = einval (fun () -> Lm_channel.seek ch (pick (n + 1)) Unix.SEEK_SET) && einval (fun () -> Lm_channel.tell ch) in
               "seek and tell raise EINVAL", r
      in
      steps := step :: !steps;
      note (name ^ ": " ^ step) ok_ (fun () -> Printf.sprintf "%S after %s" s (String.concat " " (List.rev !steps)));
      if not ok_ then good := false
    end
  done

let () =
  for _ = 1 to 300 do
    let s = text () in
    run_input "of_string" s (Lm_channel.of_string s);
    let off = pick (String.length s + 1) in
    let len = pick (String.length s - off + 1) in
    run_input "of_substring" (String.sub s off len) (Lm_channel.of_substring s off len)
  done;
  for _ = 1 to 300 do
    let ch = Lm_channel.create_string () and b = Buffer.create 16 in
    let steps = ref [] in
    for _ = 1 to 1 + pick 10 do
      let t = text () in
      (match pick 5 with
       | 0 -> let c = if t = "" then 'x' else t.[0] in Lm_channel.output_char ch c; Buffer.add_char b c; steps := "output_char" :: !steps
       | 1 -> let c = 97 + pick 26 in Lm_channel.output_byte ch c; Buffer.add_char b (Char.chr c); steps := "output_byte" :: !steps
       | 2 -> Lm_channel.output_string ch t; Buffer.add_string b t; steps := "output_string" :: !steps
       | 3 -> let off = pick (String.length t + 1) in let len = pick (String.length t - off + 1) in
              Lm_channel.output_buffer ch t off len; Buffer.add_string b (String.sub t off len); steps := "output_buffer" :: !steps
       | _ -> let by = Bytes.of_string t in
              let k = ref 0 in
              let stuck = ref false in
              while !k < Bytes.length by && not !stuck do
                let w = Lm_channel.write ch by !k (Bytes.length by - !k) in
                if w <= 0 then stuck := true else k := !k + w
              done;
              Buffer.add_string b (if !stuck then String.sub t 0 !k ^ "<write returned 0>" else t); steps := "write" :: !steps)
    done;
    let got = Lm_channel.to_string ch in
    note "create_string: to_string = what was written" (got = Buffer.contents b)
      (fun () -> Printf.sprintf "after %s: %S vs %S" (String.concat " " (List.rev !steps)) got (Buffer.contents b))
  done;
  List.iter (fun name ->
      let t = Hashtbl.find tried name and o = try Hashtbl.find ok name with Not_found -> 0 in
      Printf.printf "%-48s %4d of %4d\n" name o t;
      match Hashtbl.find_opt first name with None -> () | Some w -> Printf.printf "  first difference: %s\n" w)
    (List.sort compare !order)
