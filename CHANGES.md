# Changes

## Unreleased

### Fixed

- `Lm_rformat.format_quoted_string` printed only the first character of a
  string that needs quoting (`"two words"` was shown as `"t"`), and escaped a
  tab as `\r` (#23). It now prints the whole string, escaping a newline, a
  carriage return, a tab and a double quote. metaprl's display forms use it
  for variable and operator names (`refiner/reflib/dform.ml`,
  `simple_print.ml`).
- `Lm_list_util.remove_suffix` returned `[]` where the prefix is expected
  (`remove_suffix [1; 2; 3] [3]` was `[]`); it now returns `[1; 2]` (#25).
- `Lm_list_util.splitup n` made chunks of n+1 elements, last chunk first. It
  now makes chunks of at most n elements, in order (`splitup 2 [1; 2; 3]` is
  `[[1; 2]; [3]]`), and raises `Invalid_argument` for n <= 0 (#25). Nothing
  in libmojave or metaprl calls either function.
- `Lm_location.union_loc l1 l2` took `l1`'s start and `l2`'s end whatever
  their order, so the result need not cover both: `union_loc (2:4-2:9)
  (1:0-1:5)` was the backwards span `2:4-1:5`. It now takes the earlier start
  and the later end (#27). Its caller, `Lm_parser`'s semantic actions,
  unions a reduction's symbols from the last one down, so every reduction's
  location ran backwards: for `1+2` the `add` action received columns 2-1. It
  now receives 0-3 (`test/test_parser.ml` records each action's location).
- `%b` in `Lm_rprintf`/`Lm_format` (`Lm_make_printf`) passed the format
  `"%b"` and the string `"true"` or `"false"` to C's `snprintf`: glibc read
  `%b` as a binary unsigned int and printed the pointer's bits (undefined
  behaviour), macOS printed a literal `b`. The conversion is now formatted as
  `%s`, keeping flags and width: `"%7b|"` gives `"   true|"` (#32).
- `Lm_imp_dag.ImpDag.delete` never removed the node: it built the shrunk
  array without storing it, yet renumbered every edge list as if the node
  were gone, so the graph was corrupted (deleting `b` from `a->b->c, a->d`
  left `a` pointing at `c` and lost `d` from `sort`). It now removes the node
  and renumbers the remaining edges (#42). metaprl does not call `delete`.
- `Lm_interval_set.DenseIntervalSet` gave wrong answers for most set
  operations (#45), found by checking it against a brute-force model:
  `isect` always returned the empty set, `subtract` returned pieces of the
  second set, `union` kept duplicate and overlapping intervals (its
  `normalize` had been commented out by an unclosed comment), comparing a
  closed right bound with an open one returned "equal" for different
  points, and `of_interval` kept an empty interval such as `[14,6]` as if it
  had members. It now follows `CountableIntervalSet`, which agreed with the
  model throughout. Nothing in libmojave or metaprl instantiates either
  functor.
- `Lm_splay_linear_set` (#46), found by checking it against a list model
  beside `Lm_array_linear_set`, which agreed throughout:
  `append_list t1 l t2` used `t2` in place of `t1` and numbered the new
  elements one too low (`[1;2;3] [10;11] [7;8]` gave `[7;8;10;11;7;8]`);
  `lazy_sub_map f t i len` dropped element `i`; and the right part of
  `split` could not be indexed (`get` raised) because its offset had the
  wrong sign, while `fold` reported the indices of such a part unshifted.
- `Lm_flags`: setting a flag did not check its type, so an integer could be
  stored on a Boolean flag, which then failed every `get_bool`; it now
  raises `Failure` naming the flag's type. `flags_set_expr "nope=3"` on an
  undefined flag reported an invalid value; it now reports that the flag is
  not defined, as `"nope=true"` already did (#47).
- `Lm_file_util.parse_path dir name` reversed `dir`:
  `parse_path ["usr"; "lib"] "x"` was `["lib"; "usr"; "x"]`, and `..` removed
  the first directory instead of the last. It now keeps `dir` in order (#48).
  Nothing in libmojave, metaprl or omake passes a `dir`.
- `Lm_splay_set` (#49): `intersectp` answered `true` for any two sets of
  more than one element when the first was the smaller (it searched the
  first set for its own root), and `union` and `intersectp` compared keys
  with the polymorphic `compare` instead of `Ord.compare`, so under any other
  order they went wrong. `Lm_small_set` (#49): `add` and `of_list` kept
  duplicates while the set was a list (`add 25, 25, 9` had cardinal 3).
  Found by checking every `MinimalSetSig` implementation against a model;
  nothing in metaprl uses these three modules.
- `Lm_rformat_tex` did not escape `~`, which TeX reads as a non-breaking
  space, so a literal tilde disappeared; it is now written `\char126` (#30).
- `Lm_format.bprintf` (and so `Lm_rprintf.bprintf`) wrote nothing: it printed
  to a formatter on the buffer and never flushed it. It now flushes at the
  end, as `Format.bprintf` did (#33).
- `Lm_format.sprintf` discarded the string a `%a` or `%t` printer returns
  (`sprintf "pair=%a" pp x` gave `"pair="`); it now prints it, as
  `Printf.sprintf` does (#33). A new functor `Lm_make_printf.MakePrintfResult`
  takes the arguments of `MakePrintf` plus `print_result`; `MakePrintf` is
  unchanged.
- `%S`, `%C` and `%B` raised `Invalid_argument "parse_spec"`. They now print
  as `Printf` does: an OCaml string or character literal, and a Boolean
  (#33). Nothing in libmojave or metaprl calls these with `%a`, `%t`, `%S`,
  `%C` or `%B`, and none calls `Lm_format.bprintf`.
- `Lm_thread_event`: a `choose` that blocked returned the value of its
  outermost `send` arm even when a different arm completed:
  `choose [receive c2; send c1 ()]`, completed by a sender on `c2`, returned
  the send arm's value and lost the received one. A send arm now sets the
  result only when its own write was taken (#34). metaprl's ensemble code
  chooses between events (`remote_ensemble.ml`).
- `Lm_thread_event.sync` and `poll` wrote two debugging lines to stderr
  (`Lm_thread_event.lock`, `Lm_thread_event.unlock`) on every call; they no
  longer do (#34).
- `Lm_thread_event`: a `sync` that failed (it raises `Invalid_argument
  "Event.sync"`) left its queued send or receive live, so a later peer
  completed against it: the value of a failed send could still be received,
  and a later send could hand its value to a receiver that had already given
  up. A finished `sync` now marks its entries performed (#39).
- `Lm_thread_event` waited on its condition variable once, so a spurious
  wakeup (which `Condition` allows) made `sync` raise. Under system threads
  it now waits until a peer has performed the communication (#39). No test
  can produce a spurious wakeup, so this half is not pinned; under null
  threads `sync` still waits once and raises, as pinned.
- `Lm_thread_shell.create_or_find id i`, when `(id, i)` did not exist,
  created `(id, n)` with `n` the first free number, so it never found what it
  created (`create_or_find "c" 5` twice gave `(c,1)` and `(c,2)`). It now
  creates `(id, i)` (#35). metaprl restores saved shell sessions with it
  (`support/shell/shell_current.ml`), so a saved session's number changed
  on restart.

### Tests

- `Lm_list_util` is pinned (`test/test_list_util.ml`, #21): slicing,
  searching, maps and folds, two- and three-list operations, set-like
  operations and association lists, including their exceptions on empty
  lists, out-of-range indexes and mismatched lengths.
- `Lm_rformat` is pinned (`test/test_rformat.ml`, #22): documents built from
  each zone and break kind, rendered as text at widths 80, 20, 10 and 4, the
  calls a backend printer receives (including tags and invisible text),
  buffer cloning and clearing, zone depth, and a marshal round trip.
- `Lm_array_util` is pinned (`test/test_array_util.ml`, #24): searching,
  `replace`, appends, `sub_map`, `collect` and `distinct`, including their
  exceptions on empty arrays and out-of-range indexes.
- `Lm_location` is pinned (`test/test_location.ml`, #26): creation, `dest_loc`,
  `bogus_loc`, printing (one line and several), `compare` and `union_loc`.
- `Lm_splay_table` is pinned (`test/test_splay_table.ml`, #28), the table
  metaprl's refiner uses for term tables: every operation of `TableSig` on
  several insertion orders and repeated keys, `union`, and a 200-key table
  after insertions and removals.
- The `Lm_rformat` HTML and TeX backends are pinned
  (`test/test_rformat_backends.ml`, #29): documents with special characters,
  breaks, indentation, tags and invisible zones, rendered as HTML (with and
  without a tagger) and as TeX at two widths, and `Lm_rformat_html.escape`.
- `Lm_rprintf` and `Lm_ref_util` are pinned (`test/test_rprintf.ml`, #31):
  `sprintf`/`bprintf` on each conversion, compared with `Printf.sprintf`, the
  string channel `stdstr`, the list printers, `output_rbuffer` and boxes.
  #33 adds `bprintf` without `%a`, `%t`, a `%a` printer that calls `sprintf`,
  and padded `%S`/`%B`, pinned before the fix.
- `Lm_thread_event` is pinned under the `threads` profile
  (`test/test_thread_event.ml`, #34): channels, `poll`, `sync`, `wrap`,
  `choose`, `select`, `sequence` and `sequence_final`, including a blocked
  `choose` completed by a sender and by a receiver, with each arm order.
- `Lm_thread_shell` is pinned (`test/test_thread_shell.ml`, #35): job
  creation, hidden and visible jobs, `get_pids`, `make_pid`, `with_pid`,
  `with_current`, `set_pid` and `create_or_find`, in both thread profiles.
- `Lm_thread_event` is pinned under null threads too
  (`test/test_thread_event_null.ml`, #39): a blocking `sync` raises, and a
  failed `sync` leaves nothing for a later peer.
- `Lm_debug` is pinned (`test/test_debug.ml`, #36): creating, loading,
  setting and listing named flags, a flag set before it is created
  (`set_possible_debug`), `debug_level`, `set_debug_flags` and `timing_wrap`.
- `Lm_string_set` is pinned (`test/test_string_set.ml`, #37): both string
  orders (length first, and `Stdlib.compare`), and the multi-value tables
  (`Lm_map.LmMakeList`), which no test exercised before. `mapi_all` can leave
  a key bound to an empty list, where `mem` is true and `find` raises
  `Not_found`; it is pinned as it stands.
- `Lm_trace` is pinned (`test/test_trace.ml`, #38): conversions, the maps,
  folds and iterations with and without depth, `fold_map`, `header_nodes`,
  `special_nodes` and `pp_print`, on one tree of three levels.
- `Lm_array_linear_set` and `Lm_splay_linear_set` are checked against a list
  model (`test/test_linear_set.ml`, #46): every operation, and indexing the
  results of `split`, `concat`, `append_list` and `lazy_sub_map`.
- `Lm_flags` is pinned (`test/test_flags.ml`, #47): registering, getting and
  setting flags, and `name=value` expressions with their errors.
- `Lm_file_util` is pinned (`test/test_file_util.ml`, #48): path parsing and
  building, and reading, writing and finding files. A path list cannot
  express an absolute path, so `path_dir "/usr/lib/x"` is `"usr/lib"`; pinned
  as it stands.
- The `MinimalSetSig` implementations are checked against a model
  (`test/test_min_sets.ml`, #49): `Lm_set`, `Lm_splay_set` (in two orders),
  `Lm_hash_set` and `Lm_small_set` over two bases.
- `Lm_dlist` is checked against a model (`test/test_dlist.ml`, #50): 30,000
  random inserts and deletes over 8 handles, the list read four ways after
  each step.
- `Lm_int_set` is pinned (`test/test_int_set.ml`, #40): sets, tables and
  multi-value tables over keys that include `min_int` and `max_int`.
- `Lm_printf_rbuffer` is pinned (`test/test_printf_rbuffer.ml`, #41): a
  document printed at the default width of 80.
- `Lm_imp_dag` is pinned (`test/test_imp_dag.ml`, #42): building, roots,
  sort, the sweeps, `node_rel` and `delete`. `node_rel` caches a derived
  relation as an edge in its first node's lists, so a query changes what
  `node_out_edges`, `node_in_edges` and the sweeps return; pinned as it stands.
- `Lm_symbol_matrix` is pinned (`test/test_symbol_matrix.ml`, #43): the pair
  set and the symmetric and asymmetric pair tables, including `copy`,
  `filter_add`, `filter_remove`, `map` and `fold`. `create 0` makes a table
  whose first `add` raises `Division_by_zero`; pinned as it stands.
- `Lm_inet` is pinned over loopback (`test/test_inet.ml`, #44): serving on
  an ephemeral port, connecting, accepting, data both ways, the peer and
  local ports, and connecting to a closed port.
- `Lm_interval_set` is checked against a brute-force model
  (`test/test_interval_set.ml`, #45): both functors, every set operation,
  12,000 generated cases; the test prints the number of disagreements per
  operation.

### Behaviour that the tests record and that may be a bug

- The printf engine behind `Lm_rprintf` and `Lm_format` (`Lm_make_printf`, an
  untyped interpreter of the format string) disagrees with `Printf`:
  - `%S`, `%C` and `%B` raise `Invalid_argument "parse_spec"`;
  - `%a` drops the user printer's output: `sprintf "pair=%a"` gives
    `"pair="`, and `bprintf b "[%a]"` writes nothing, not even the brackets.
  None of metaprl's `Lm_rprintf` or `Lm_format` users uses these conversions
  (counted on single-line `printf` calls, with a positive control).

## 1.0.0 (2026-10-28)

The first release. It is the 2017 library, refreshed for current OCaml.

libmojave builds again, on a current OCaml. Nothing in the public API was
removed, except `Lm_dll` (see below).

### Building

- Builds on OCaml 4.14 and 5.4 (#3, #4). OCaml 5 removed or renamed parts
  of the standard library and the C runtime API that the 2017 code used:
  - `Pervasives` is now `Stdlib`.
  - The C stubs use the `caml_`-prefixed runtime names; `uerror` is
    `caml_uerror` from OCaml 5 on.
  - `Format.bprintf` and `get_all_formatter_output_functions` are restored in
    `Lm_printf` with their OCaml 4.14 definitions, and the out-functions
    record carries OCaml 5's `out_indent`.
  - `lm_mmap` writes its out-of-heap string header with
    `Caml_out_of_heap_header`.
- A dune build (`lib/`, `config/`) alongside OMake, producing the same 108
  modules and the same C stubs (#4). `config/discover.ml` repeats OMake's C
  configuration checks. The OMake build remains, because MetaPRL builds
  libmojave as an OMake subdirectory.
- `dune build --profile threads` selects the system thread implementation;
  the default is the null implementation, as in OMake (#7).
- Builds on Windows with MinGW (#8). That build found a 64-bit Windows bug:
  custom-block callbacks used `long`, which is 32 bits on Win64, where OCaml's
  `intnat` is 64. `ssl_info_hash` truncated the pointer it hashed, and the
  Win32 `socklen_t` did not match Winsock.
- Builds on Windows with MSVC too (#13). `WIN32` is now defined on Windows, as
  OMake does, so MSVC compiles the Win32 branches.
- Warning flags in numeric form. Warnings added in OCaml 4.13 and later (67,
  68, 69, 70) are not errors.

### Fixed

- `Lm_config.code` was a NULL pointer instead of `ByteCode` or `NativeCode`:
  its C stubs returned `Int_val(0)` and `Int_val(1)` where `Val_int` was
  meant, and both are a raw 0. OCaml 4 ignored a pointer outside its heap;
  OCaml 5's garbage collector follows it and crashes the first full
  collection in any program that links `Lm_config` (all of libmojave is
  linked with `-linkall`, as MetaPRL's camlp5 drivers do). Before the fix,
  the value was never equal to either constructor. `test/test_config.ml`
  checks it as bytecode and as native code.

- `Lm_ctype`'s six C primitives (`is_alnum`, `is_digit`, `is_graph`,
  `is_print`, `is_upper`, `set_locale`) were declared but defined nowhere in
  libmojave, so a program linking `Lm_ctype` failed to link. They are now in
  `cutil/lm_ctype.c`, newly written from the C standard's `<ctype.h>` and
  `setlocale`; `test_ctype` checks every ASCII character. They follow the C
  library's locale tables, so platforms can differ: under MSVC's C runtime
  `is_print '\t'` is true (MinGW, Linux and macOS say false), and the MSVC
  build compares against its own expected file.

### Tests

- Pinning tests in `test/` (#5, #6, #7, #9, #11, #16) for
  `Lm_string_util`, `Lm_filename_util`, `Lm_glob`, `Lm_symbol`, `Lm_set`,
  `Lm_map`, `Lm_printf`, `Lm_lexer`, `Lm_parser`, `Lm_num`, `Lm_channel`,
  `Lm_thread`, `Lm_hash` and `Lm_fmarshal`.
  `Lm_marshal_buf` and `Lm_marshal_shared` (C stubs) are not pinned.
  - Each test records a function's output on chosen inputs, as generated from
    the code at the time; the tests do not specify what the output should be.
  - Each test was shown to fail when the code it covers was changed.
  - They run on Linux (OCaml 4.14 and 5.4) and Windows. The filename and glob
    tests have separate expected output for Windows.

### Not built

- `Lm_dll`. Its C side redirects `caml_local_roots` for stub code, and on
  OCaml 5 `CAMLparam` no longer reads that variable.

### Behaviour that the tests record and that may be a bug

None of these was changed. A fix will show up as a change to an expected file.

- `Lm_string_util.unescape` has no hex escapes (`"\x41"` becomes `"x41"`).
- `Lm_string_util.parse_args_list` does not group single quotes, and raises on
  `a\ b`.
- `Lm_glob.is_glob_string "{a,b}"` is false, although `glob` expands braces.
- `Lm_glob.glob` raises `Failure "…: bad match"` when nothing matches.
- `Lm_symbol.to_string (add "x_1")` is `"x_"`.
- `Lm_num.is_integer_num` is false for an integer too large for an `int`.
- `Lm_num.to_int32` wraps silently.
- `Lm_channel.tell` and `seek` raise `Unix_error(EINVAL, …)` on a string
  channel.
- `Lm_parser` accepts only when the start production consumes the
  end-of-input token itself.
- `Lm_fmarshal` writes an `Int` as 32 bits, so `Int (-1)` reads back as
  `Int 4294967295` and `Int max_int` is truncated.
- `Lm_hash.HashCode` gives `add_int (-1)` and `add_int max_int` the same
  code.

### Repository

- Commit, branch-name and pull-request checks run on every push (#1, #2).
