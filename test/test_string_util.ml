open Pin
module S = Lm_string_util

let inputs = [ ""; "a"; "abc"; "a b\tc\n"; " lead"; "trail "; "a,b,,c"; "x\"y'z\\"; "<a&b>"; "caf\xc3\xa9"; "\x00\x7f" ]

let () =
  List.iter (fun s -> line "hexify" (str s) (guard (fun () -> str (S.hexify s)))) inputs;
  List.iter (fun s -> line "unhexify∘hexify" (str s) (guard (fun () -> str (S.unhexify (S.hexify s))))) inputs;
  List.iter (fun s -> line "unhexify" (str s) (guard (fun () -> str (S.unhexify s)))) [ "41"; "4"; "zz"; "" ];
  List.iter (fun s -> line "unhexify_int" (str s) (guard (fun () -> string_of_int (S.unhexify_int s)))) [ "ff"; "10"; "0"; "g" ];
  List.iter (fun s -> line "c_escaped" (str s) (guard (fun () -> str (S.c_escaped s)))) inputs;
  List.iter (fun s -> line "sql_escaped" (str s) (guard (fun () -> str (S.sql_escaped s)))) inputs;
  List.iter (fun s -> line "js_escaped" (str s) (guard (fun () -> str (S.js_escaped s)))) inputs;
  List.iter (fun s -> line "html_escaped" (str s) (guard (fun () -> str (S.html_escaped s)))) inputs;
  List.iter (fun s -> line "unescape" (str s) (guard (fun () -> str (S.unescape s)))) [ "a\\nb"; "\\t"; "\\\\"; "\\x41"; "\\101"; "\\" ];
  List.iter (fun s -> line "is_white" (str s) (string_of_bool (S.is_white s))) inputs;
  List.iter (fun s -> line "tokens_std" (str s) (guard (fun () -> list str (S.tokens_std s)))) inputs;
  List.iter (fun s -> line "split \",\"" (str s) (guard (fun () -> list str (S.split "," s)))) inputs;
  List.iter (fun s -> line "split_string \",,\"" (str s) (guard (fun () -> list str (S.split_string ",," s)))) [ "a,,b"; "a,b"; ",,"; "" ];
  List.iter (fun s -> line "parse_args_list" (str s) (guard (fun () -> list (list str) (S.parse_args_list s))))
    [ "a b c"; "a \"b c\" d"; "a 'b c'"; "a\\ b"; ""; "x;y" ];
  List.iter (fun (a, b) -> line "is_string_prefix" (str a ^ " " ^ str b) (string_of_bool (S.is_string_prefix a b)))
    [ ("", "a"); ("a", "abc"); ("abc", "a"); ("b", "abc") ];
  List.iter (fun (a, b) -> line "string_compare" (str a ^ " " ^ str b) (string_of_int (compare (S.string_compare a b) 0)))
    [ ("a", "b"); ("b", "a"); ("a", "a"); ("ab", "a"); ("", "a"); ("B", "a") ];
  List.iter (fun (s, c) -> line "strchr" (str s ^ " " ^ Printf.sprintf "%C" c) (guard (fun () -> string_of_int (S.strchr s c))))
    [ ("abc", 'b'); ("abc", 'z'); ("", 'a') ];
  List.iter (fun (s, set) -> line "index_set" (str s ^ " " ^ str set) (guard (fun () -> string_of_int (S.index_set s set))))
    [ ("hello world", " o"); ("abc", "xyz") ]
