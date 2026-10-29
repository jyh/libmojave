(* Lm_parse_id pinned on CVS $Id$ strings: well formed, with doubled
   spaces, short, and empty. *)
open Pin
module P = Lm_parse_id

let () =
  List.iter (fun s ->
      line "parse_id" (str s) (guard (fun () ->
          let name, rev, date, time, user, ty = P.parse_id s in
          String.concat " | " [ name; rev; date; time; user; ty ]));
      line "parse_id_revision" (str s) (str (P.parse_id_revision s)))
    [ "$Id: lm_list_util.ml,v 1.12 2003/01/15 18:32:01 jyh Exp $";
      "$Id: f.ml,v 1.1  2003/01/15 18:32:01 jyh Exp $";
      "$Id: f.ml,v 1.1 2003/01/15 $";
      "$Id$";
      "" ]
