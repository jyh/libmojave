(* Lm_attribute pinned: the down attribute and the core are strings, a
   down is applied by prefixing it, and two downs compose by
   concatenation (not commutative, so the order shows). The up attribute
   is the core's length, and a down adds its length. Applying a then b
   must equal applying b to (a applied to the core). *)
open Pin
module A = Lm_attribute

let union = ( ^ )
let push d c = d ^ c
let up1 c = String.length c
let up2 d u = String.length d + u

let () =
  let t = A.wrap_core "L" "core" in
  let t1 = A.apply union "a" t in
  let t2 = A.apply union "b" t1 in
  line "label" "" (A.label_of_term t2);
  line "apply a, then b: dest_up, then dest_core" "" (Printf.sprintf "%d %s" (A.dest_up up1 up2 t2) (A.dest_core push t2));
  line "  the same as b applied to (a applied to the core)" "" (string_of_bool (A.dest_core push t2 = push "b" (push "a" "core")));
  line "  dest_up after dest_core (cached)" "" (string_of_int (A.dest_up up1 up2 t2));
  line "t1 is not changed by applying b to it: dest_core t1" "" (A.dest_core push t1);
  line "the original term: dest_core, dest_up" "" (Printf.sprintf "%s %d" (A.dest_core push t) (A.dest_up up1 up2 t));
  let t3 = A.apply union "c" t1 in
  line "apply c to t1 after t1's dest_core" "" (A.dest_core push t3);
  let s = A.wrap_simple_core "S" 42 in
  line "simple term: label, core" "" (Printf.sprintf "%s %d" (A.label_of_simple_term s) (A.dest_simple_core s))
