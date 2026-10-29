(* Lm_trace: trees whose nodes are elements or headed sub-traces. The
   interface documents no meaning for header_nodes and special_nodes, so
   this records what they return on one tree of three levels. *)
open Pin
open Lm_trace

(* a; [h1: b; [h2: c]; d]; e; [h3: f] *)
let t = [ Elem "a"; Lm_trace ("h1", [ Elem "b"; Lm_trace ("h2", [ Elem "c" ]); Elem "d" ]); Elem "e"; Lm_trace ("h3", [ Elem "f" ]) ]

let rec show t = list show_node t
and show_node = function Elem x -> x | Lm_trace (x, t) -> x ^ ":" ^ show t

let () =
  line "the tree" "" (show t);
  line "to_list" "" (list str (to_list t));
  line "of_list [x; y]" "" (show (of_list [ "x"; "y" ]));
  line "to_list []" "" (list str (to_list []));
  line "map uppercase" "" (show (map String.uppercase_ascii t));
  line "map_depth" "" (show (map_depth (fun d x -> Printf.sprintf "%s%d" x d) t));
  let acc = ref [] in
  iter (fun x -> acc := x :: !acc) t;
  line "iter order" "" (list str (List.rev !acc));
  let acc = ref [] in
  iter_depth (fun d x -> acc := Printf.sprintf "%s@%d" x d :: !acc) t;
  line "iter_depth" "" (list str (List.rev !acc));
  line "fold (concat)" "" (str (fold ( ^ ) "" t));
  let n, t' = fold_map (fun n x -> n + 1, Printf.sprintf "%d%s" n x) 0 t in
  line "fold_map (number nodes)" "" (string_of_int n ^ " " ^ show t');
  line "header_nodes" "" (list str (header_nodes t));
  line "special_nodes" "" (list str (special_nodes t));
  line "header_nodes / special_nodes, no sub-traces" "" (list str (header_nodes (of_list [ "x" ])) ^ " " ^ list str (special_nodes (of_list [ "x" ])));
  let b = Buffer.create 64 in
  let ppf = Lm_printf.formatter_of_buffer b in
  Lm_printf.fprintf ppf "@[<v 0>%a@]@." (fun ppf t -> pp_print ppf Lm_printf.pp_print_string t) t;
  line "pp_print" "" (str (Buffer.contents b))
