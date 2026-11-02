(* Lm_channel over descriptors, against a model (the 2026-11-02 coverage
   re-measure: lm_channel.ml at 45 %, the descriptor half unreached).

   - file: 120 random texts up to 3 buffers long (buf_size is 4096),
     written to a file channel by a random sequence of output_char,
     output_string, output_buffer, write and flush, with tell checked
     against the bytes written; closed; the file's bytes compared with the
     text; then read back through a file channel by a random sequence of
     input_char, input_line, input_entire_line, read, input_buffer, seek
     and tell, step by step against a cursor on the text.
   - pipe: 60 texts written through a pipe channel and read back through
     another, the same way (no seek on a pipe); a text fits the pipe's buffer.
   - select: over pipes, against what each case must give.

   Unix only: Windows pipes go through PeekNamedPipe and writer threads. *)
open Pin
module C = Lm_channel

let state = ref 270
let next () = state := (!state * 1103515245 + 12345) land 0x7fffffff; !state lsr 8
let pick n = next () mod n

let text max = String.init (pick (max + 1)) (fun _ -> match pick 9 with 0 -> '\n' | 1 -> ' ' | _ -> Char.chr (97 + pick 4))

let tried = Hashtbl.create 16 and ok = Hashtbl.create 16 and first = Hashtbl.create 16 and order = ref []
let note name b what =
  if not (Hashtbl.mem tried name) then order := name :: !order;
  Hashtbl.replace tried name (1 + try Hashtbl.find tried name with Not_found -> 0);
  if b then Hashtbl.replace ok name (1 + try Hashtbl.find ok name with Not_found -> 0)
  else if not (Hashtbl.mem first name) then Hashtbl.replace first name (what ())

let eof f = try Some (f ()) with End_of_file -> None

let m_line s p =
  let n = String.length s in
  if p >= n then None, p
  else match String.index_from_opt s p '\n' with
    | Some i -> Some (String.sub s p (i - p)), i + 1
    | None -> Some (String.sub s p (n - p)), n

(* Write t through ch by random operations; tell checked when [tell]. *)
let write_text ~tell name ch t =
  let n = String.length t and k = ref 0 and steps = ref [] in
  while !k < n do
    let rest = n - !k in
    let len = 1 + pick (min rest 5000) in
    (match pick 5 with
     | 0 -> C.output_char ch t.[!k]; k := !k + 1; steps := "output_char" :: !steps
     | 1 -> C.output_string ch (String.sub t !k len); k := !k + len; steps := "output_string" :: !steps
     | 2 -> C.output_buffer ch t !k len; k := !k + len; steps := "output_buffer" :: !steps
     | 3 ->
       let b = Bytes.of_string (String.sub t !k len) in
       let w = ref 0 and stuck = ref false in
       while !w < len && not !stuck do
         let r = C.write ch b !w (len - !w) in
         if r <= 0 then stuck := true else w := !w + r
       done;
       note (name ^ ": write takes every byte") (not !stuck) (fun () -> Printf.sprintf "%d of %d" !w len);
       k := !k + !w; if !stuck then k := n; steps := "write" :: !steps
     | _ -> C.flush ch; steps := "flush" :: !steps);
    if tell then begin
      let tl = (try C.tell ch with e -> -1 - String.length (Printexc.to_string e)) in
      note (name ^ ": tell = bytes written") (tl = !k)
        (fun () -> Printf.sprintf "tell %d, wrote %d after %s" tl !k (String.concat " " (List.rev !steps)))
    end
  done

(* Read t back through ch by random operations, against a cursor. *)
let read_text ~seekable name ch t =
  let n = String.length t and p = ref 0 and steps = ref [] and good = ref true in
  for _ = 1 to 25 do
    if !good then begin
      let step, ok_ =
        match pick (if seekable then 7 else 5) with
        | 0 -> let got = eof (fun () -> C.input_char ch) in
          let want = if !p < n then Some t.[!p] else None in
          if want <> None then incr p; "input_char", got = want
        | 1 -> let got = eof (fun () -> C.input_line ch) in
          let want, p' = m_line t !p in p := p'; "input_line", got = want
        | 2 -> let got = eof (fun () -> C.input_entire_line ch) in
          let want, p' = m_line t !p in
          let want = Option.map (fun l -> if p' > !p + String.length l then l ^ "\n" else l) want in
          p := p'; "input_entire_line", got = want
        | 3 -> let len = pick 6000 in
          let b = Bytes.make len '.' in
          let got = C.read ch b 0 len in
          let okr = got >= 0 && got <= len && Bytes.sub_string b 0 got = String.sub t !p (min got (n - !p))
                    && (got > 0 || len = 0 || !p >= n) in
          p := !p + got; "read", okr
        | 4 -> let len = pick 6000 in
          let b = Bytes.make len '.' in
          let got = eof (fun () -> C.input_buffer ch b 0 len; Bytes.sub_string b 0 len) in
          let want = if !p + len <= n then Some (String.sub t !p len) else None in
          (match want with Some _ -> p := !p + len | None -> p := n);
          "input_buffer", got = want
        | 5 -> let q = pick (n + 1) in
          let r = (try C.seek ch q Unix.SEEK_SET with _ -> -1) in
          p := q; "seek", r = q
        | _ -> let tl = (try C.tell ch with _ -> -1) in
          if tl <> !p then steps := Printf.sprintf "(tell %d, model %d)" tl !p :: !steps;
          "tell", tl = !p
      in
      steps := step :: !steps;
      note (name ^ ": " ^ step) ok_ (fun () -> Printf.sprintf "text of %d after %s" n (String.concat " " (List.rev !steps)));
      if not ok_ then good := false
    end
  done

let tmp = Filename.temp_file "lm_channel_fd" ".txt"

let () =
  for _ = 1 to 120 do
    let t = text 12288 in
    let fd = Unix.openfile tmp [ Unix.O_WRONLY; Unix.O_TRUNC; Unix.O_CREAT ] 0o644 in
    let ch = C.create tmp C.FileChannel C.OutChannel true (Some fd) in
    write_text ~tell:true "file out" ch t;
    C.close ch;
    let back = In_channel.with_open_bin tmp In_channel.input_all in
    note "file out: the file holds the text" (back = t) (fun () -> Printf.sprintf "%d bytes vs %d" (String.length back) (String.length t));
    let fd = Unix.openfile tmp [ Unix.O_RDONLY ] 0 in
    let ch = C.create tmp C.FileChannel C.InChannel true (Some fd) in
    read_text ~seekable:true "file in" ch t;
    C.close ch
  done;
  Sys.remove tmp

let () =
  for _ = 1 to 60 do
    let t = text 12288 in
    let r, w = Unix.pipe () in
    (* a text fits in the pipe's buffer (16 KB on macOS), so the writer
       runs to the end before the reader starts, in one process *)
    let o = C.create "pipe" C.PipeChannel C.OutChannel true (Some w) in
    write_text ~tell:false "pipe out" o t;
    C.close o;
    let ch = C.create "pipe" C.PipeChannel C.InChannel true (Some r) in
    read_text ~seekable:false "pipe in" ch t;
    C.close ch
  done;
  List.iter (fun name ->
      let k = Hashtbl.find tried name and g = try Hashtbl.find ok name with Not_found -> 0 in
      line name (string_of_int k ^ " cases")
        (if g = k then "0 disagree" else Printf.sprintf "%d disagree (first: %s)" (k - g) (Hashtbl.find first name)))
    (List.rev !order)

(* Select over pipes. *)
let () =
  let r1, w1 = Unix.pipe () and r2, w2 = Unix.pipe () in
  let c1 = C.create "ra" C.PipeChannel C.InChannel true (Some r1) in
  let c2 = C.create "rb" C.PipeChannel C.InChannel true (Some r2) in
  let o1 = C.create "wa" C.PipeChannel C.OutChannel true (Some w1) in
  let names l = list C.name l in
  let sel rs ws timeout =
    let t0 = Unix.gettimeofday () in
    let r, w, _ = C.select rs ws [] timeout in
    names r ^ " " ^ names w, Unix.gettimeofday () -. t0 in
  let s, dt = sel [ c1; c2 ] [] 0.1 in
  line "select" "nothing written, timeout 0.1" (s ^ (if dt >= 0.09 then " (waited)" else Printf.sprintf " (returned after %.3f s)" dt));
  ignore (Unix.write_substring w2 "ab" 0 2);
  line "select" "\"ab\" written to rb's pipe" (fst (sel [ c1; c2 ] [] 0.1));
  line "select" "rb reads one char" (str (String.make 1 (C.input_char c2)));
  line "select" "rb's pipe empty, \"b\" still buffered" (fst (sel [ c1; c2 ] [] 0.1));
  line "select" "write side" (fst (sel [] [ o1 ] 0.1));
  C.output_string o1 "xyz"; C.flush o1;
  line "select" "\"xyz\" through wa" (fst (sel [ c1 ] [] 0.1));
  line "select" "ra reads what is there" (guard (fun () -> let b = Bytes.create 10 in let k = C.read c1 b 0 10 in str (Bytes.sub_string b 0 k)));
  Unix.close w2;
  ignore (C.input_char c2);
  line "select" "rb drained, its writer closed (end of file)" (fst (sel [ c2 ] [] 0.1));
  line "select" "rb then" (guard (fun () -> str (String.make 1 (C.input_char c2))));
  List.iter C.close [ c1; c2; o1 ]
