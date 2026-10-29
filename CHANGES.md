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
- `Lm_index` (#52): `mem_list` raised `Not_found` when an intermediate key was
  missing; it now answers `false`. `is_empty` was `false` for an index whose
  every key had been removed; it now checks the table. Nothing in metaprl
  uses `Lm_index` or the symbol indexes built on it.
- `Lm_serial_map.remove` of an absent key raised `Not_found`; it now returns
  the map unchanged, as `Lm_map` does (#54). Nothing uses `Lm_serial_map`.
- `Lm_splay_linear_set`, after #46 (#56): `collect` placed a part taken from
  another set without shifting its indices (whole parts, and parts that run
  to the end of their set), and `append_list` with an empty middle list
  (`join`) shifted the second set one place too far, so `get`, `split` and
  `fold` on such sets read the wrong element or raised `Invalid_argument
  "Linear_set.splay - index out of bounds"`. That exception is the one
  metaprl's proof census raised with `SEQ_SET = Lm_splay`.
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
- `Lm_digraph.coalesce_nodes n m` lost a self-loop on `m` and left the
  deleted `m` in `n`'s predecessors, so `in_degree n` counted it and `pred n`
  returned it (#57). `delete_node` removes the loop from `m`'s successor list
  but not its predecessor list, and coalesce read both after the deletion. It
  now reads them first and maps `m` to `n`: a loop on `m`, like an edge
  between the two, becomes a loop on `n`. Nothing in metaprl or omake uses
  `Lm_digraph`.
- `Lm_large_array.LargeArray.set` at an index past the end grew the array
  and dropped the value, so `set a 5 x` on a new array left `get a 5` the
  default (#58). In both `LargeArray` and `Lm_large_array_weak`, `set` and
  `get` at exactly `length a` raised `Invalid_argument "index out of
  bounds"` (or `"Weak.set"`), because the bound test was `i > length`; so
  `set a 0 x` raised on every new array. `set` now grows the array when
  `i >= length` and then stores; `get` past the end raises
  `Invalid_argument "LargeArray.get"`. The one user is
  `Lm_marshal_shared` (not in metaprl or omake). Not tested: the second
  level, used only for arrays longer than `Sys.max_array_length`; its bucket
  count `(i + max_length - 1) lsr max_shift` has the same boundary at an
  `i` that is a multiple of `max_length`.
- `Lm_splay_table` (#59): `union` put the second table's data for a shared
  key first when the second table had one node, and the first table's data
  first in every other case, so `find` and `find_all` on a union depended
  on the tables' shapes. The first table's data now always comes first.
  `union` also compared keys with polymorphic `compare` at two sites, not
  the table's own order: under a descending order 170 of 500 random unions
  lost or misordered keys. And `replace t k []` left a node with no data,
  so the table's next lookup or add raised `Invalid_argument
  "Splay_table.new_node : empty data"`; it now removes `k`, as `remove`
  does when a key's list empties. metaprl's users (`supinf.ml`,
  `itt_omega.ml`, `term_eq_table.ml`) are not affected: supinf's keys are
  non-negative integers ordered as `compare` orders them, and its `append`
  adds the two coefficients.
- `Lm_parser.remove_production`, removing a name's last production, left
  the name bound to no productions, so the grammar went on treating it as a
  nonterminal: a syntax-error message no longer listed it among the
  possible tokens (#63). The name is now unbound again, and the grammar
  equals the one that never had the production. `Lm_map`'s `mapi_all`,
  which keeps an emptied binding, is unchanged. Nothing in metaprl calls
  `remove_production`.
- `Lm_imp_dag.node_rel` cached a relation it found by search as an edge in
  its first node's own lists (#64). So a query changed what
  `node_out_edges`, `node_in_edges`, the sweeps and `sort` return, and the
  cached edge outlived a `delete`: after `node_rel a c` (a < c through b)
  and `delete b`, `node_rel a c` still answered `LessThan`. The cache now
  has its own fields, cleared whenever an edge is added or a node deleted.
  `node_rel`'s answers are unchanged otherwise; metaprl's callers
  (`mllib/precedence.ml`, `auto_tactic.ml`) use only `node_rel`.
- `Lm_big_int`, the bignum under `Lm_num` (#65):
  - `div_big_int` was one too small in magnitude when the divisor was 1 or
    -1 (`-32565696657 / -1` gave `32565696656`): its binary search ran
    below the dividend, which is the quotient for those divisors.
  - `mod_big_int` gave the remainder the sign of the quotient, so
    `a = b * (a / b) + a mod b` failed whenever the divisor was negative
    (`-34063 mod -8` was `7`). The remainder now has the dividend's sign,
    as OCaml's `mod` does and as `div_big_int` truncates.
  - A zero result could be negative (`-5 * 0`, and zero quotients and
    remainders), which prints `0` but is not equal to zero and compares
    below it. Zero results are now `+0`.
  - `to_int32` dropped the sign, and `of_int32 Int32.min_int` did not
    terminate.
  `Lm_num` computes small operands with OCaml's `/` and `mod` and hands
  the rest to `Lm_big_int`, and it makes any product with an operand of
  2^15 or more a `Lm_big_int`. So before this it disagreed with itself at
  small sizes: `(-17) mod (-10)` was `-7`, while `(40000 * 3) mod (-7)` was
  `-6` where OCaml's `mod` gives `6`, and a large value times `0` was not
  equal to `0`. Both paths now agree. Not changed: `integer_big_int` raises for magnitudes of 2^30
  and above, on any word size.
- `Lm_int_util.log2` did not terminate for arguments of 2^61 and above (on
  a 64-bit system): it searched for the first `1 lsl j` above the
  argument, and `1 lsl 62` is `min_int` (#66). The search now stops at the
  largest power of two. Its callers are `Lm_large_array` and
  `Lm_large_array_weak`, with `Sys.max_array_length` and with the index
  given to `set`, so `set a (1 lsl 61) x` reached it.
- `Lm_cycle_dag` (#67): `equate` with one known name added the other to
  the class's names but not to its table, so `find` on the new name raised
  `Not_found`, and a later `insert` could close a cycle through it without
  raising `Cycle`. An `insert` that raised `Cycle` had already added leaves
  for some of its children. And `equate x x` on a new name listed `x`
  twice. Now the new name is added to the table, `insert` checks every
  child before changing anything, and `equate x x` lists `x` once. Nothing
  in metaprl or omake uses `Lm_cycle_dag`.
- `Lm_hashtbl_util.add_hashtbl dst src` gave `dst` src's oldest value for a
  key with shadowed bindings, not its current one (#68): it walked every
  binding with `Hashtbl.iter`, which presents a key's bindings newest
  first, removing and re-adding each. It now copies each key's current
  value. Nothing in libmojave, metaprl or omake calls it.
- `Lm_loop` (#70): an edge into the root made `create` raise
  `Invalid_argument "index out of bounds"`, and a listed node that the
  root does not reach gave wrong immediate dominators (with `0 > 1`,
  `1 > 3` and an unreachable `2 > 3`, the idom of `3` was `0`, not `1`),
  and on some graphs overflowed the stack. The depth-first walk took
  `sdno = 0` to mean "not visited", which the root and every unreached
  node also have. It now keeps its own visited marks, the vertex array
  holds only the nodes it reached, and predecessors it did not reach are
  ignored. A brute-force model over 1,500 random graphs agrees, with
  edges into the root and unreachable nodes listed.
- `Lm_loop.loop_nest` (#73) raised `Invalid_argument "build_trace"` when
  no node hung under the root (a root with no successors), kept only
  each node's innermost back edge (so a node closing two loops left the
  outer one out), and built a loop's body along the latch's
  immediate-dominator chain (so the nodes on a branch inside a loop were
  put outside it). It now computes natural loops: the body of a header is
  the header and every node that reaches one of its latches without
  passing it, and each node sits in the innermost body containing it. A
  model of natural loops over 1,500 random graphs agrees.
- `Lm_db.append_entry` on a new file wrote its entries with no file header
  (#71). `find_entry` then found none of them, and the next `remove` or
  `remove_entry`, seeing no header, truncated the file to the header
  alone, discarding them. `append_entry` now writes the header first on an
  empty file. omake, the user, calls only `add` and `find`, which were
  right.
- `Lm_parser` ended a parse early (#77). A state holding one complete item
  whose lookahead is only end-of-input is reduced without reading a token,
  and the parse returned that reduction's value. That is right for the
  start production `prog -> S $eof`, but with `S -> a` the parser accepted
  `aa` and `ab` as `S(a)`, reading one token and never reducing `prog`; and
  with `prog -> a prog | b $eof` it returned the inner `prog` for `ab`. The
  parse now ends only when the start symbol is reduced onto an empty stack.
  metaprl's `filter_grammar` runs theory grammars through `Parser.parse`.
- `Lm_lexer.union` numbered the second lexer's clauses in reverse order
  (#78), so between two of its own clauses that match the same text the
  other one won. With `choose = max`, which metaprl's `filter_grammar`
  uses, a lexer adding `[a-z]+` and then `let` lexed `let` as the keyword
  alone and as the identifier after a union. The second lexer's clauses
  now keep their order.
- `Lm_lexer.search` and `searchto` did not find the first occurrence of a
  match, as the interface says (#80). Among the matches that started
  before the first one ended, they took the one that ended last: `>>` in
  `>>>` was found at offset 1, `ab|bcd` in `abcd` gave `bcd`, and `<<` in
  `<<<x>>>` at offset 1. They now try a match at each offset from the
  start and take the first offset where one is found, the longest match
  there. metaprl's `filter_grammar` uses `searchto` to find the end of a
  quotation.
- `Lm_lexer`'s `\( \)` arguments (#82). The branches of an alternation
  number their groups from the same start (they must have equal counts,
  and the arity counts the first), but `lex` reported each group by its
  position in the whole expression: `\(a\)\(b\)|\(c\)\(d\)` (arity 2)
  gave `["", "", "c", "d"]` on `cd`. And a group that took no part in the
  match was left out, so `\(a\)*b` (arity 1) gave `[]` on `b`. The i'th
  group of the branch that matched is now argument i, and the arguments
  are padded with `""` to the arity, in `lex`, `search` and `searchto`.
  metaprl's one lexer group (a quoted string) is unchanged.

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
- Both linear sets are checked on composed operations
  (`test/test_linear_set_compose.ml`, #56): sets built by random histories
  of every operation, as the refiner builds them.
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
- `Lm_bitmatrix` is checked against a model (`test/test_bitmatrix.ml`, #51):
  30,000 random adds, removes and queries on matrices of random size.
- `Lm_index` is pinned (`test/test_index.ml`, #52): chains, lookups at one
  and several levels, removal, `filter_add` and persistence.
- `Lm_listbuf` is checked against a list model (`test/test_listbuf.ml`, #53):
  5,000 random histories of every operation.
- `Lm_serial_map` is checked against a model (`test/test_serial_map.ml`, #54):
  15,000 random adds and removes against an association list in insertion
  order.
- `Lm_graph` is checked against a model (`test/test_graph.ml`, #55): 10,000
  random node and edge additions and deletions, with degrees, neighbours,
  queries and depth-first reachability after each.
- `Lm_int_set` is pinned (`test/test_int_set.ml`, #40): sets, tables and
  multi-value tables over keys that include `min_int` and `max_int`.
- `Lm_printf_rbuffer` is pinned (`test/test_printf_rbuffer.ml`, #41): a
  document printed at the default width of 80.
- `Lm_imp_dag` is pinned (`test/test_imp_dag.ml`, #42): building, roots,
  sort, the sweeps, `node_rel` and `delete`. `node_rel` cached a derived
  relation as an edge in its first node's lists, so a query changed what
  `node_out_edges`, `node_in_edges` and the sweeps return; fixed in #64.
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
- `Lm_field_table` is checked against models (`test/test_field_table.ml`,
  #61): 4,500 random steps for each of `FieldTable` and `FieldMTable`, with
  lookups, the index order of `to_list`, the iterators, `replace_ext` and
  `map` after each.
- The rest of `Lm_hash` is checked against models (`test/test_hash_cons.ml`,
  #62): `MakeHashCons` (dense indices, `get`, `fold`, `map_array`),
  `MakeHashMarshalEq` with a fine and a coarse equality (items marshalled
  before use included), `compare_int_list` and `hash_int_list`.
- `Lm_big_int` is checked against native ints (`test/test_big_int.ml`,
  #65): 4,000 random pairs for each operation, the division identity,
  identities beyond native ints, and `Lm_num`'s two paths.
- `Lm_symbol_hash` is checked against `Lm_symbol` (`test/test_symbol_hash.ml`,
  #66) on 3,000 random names, marshalled symbols included, and
  `Lm_int_util.log2` against a floor log2 at every power of two and its
  neighbours.
- `Lm_cycle_dag` is checked against a model (`test/test_cycle_dag.ml`, #67):
  4,800 random insert and equate steps, with the cycle refusals, `find` and
  the order `sort` gives.
- `Lm_hashtbl_util` is checked against a model (`test/test_hashtbl_util.ml`,
  #68), on tables with shadowed bindings.
- `Lm_arg` is pinned (`test/test_arg.ml`, #69): 25 argument vectors under
  both option modes, with every effect of the spec and every exception.
- `Lm_loop`'s dominators are checked against a brute-force model
  (`test/test_loop.ml`, #70), and the loop nest of one graph is pinned.
- `Lm_db` is checked against a model over a scratch file (`test/test_db.ml`,
  #71), with the predicate API on fixed histories.
- `Lm_attribute` is pinned (`test/test_attribute.ml`, #72): two downward
  attributes compose in the order they are applied, and `dest_core` and
  `dest_up` cache their results.
- `Lm_parse_id` is pinned (`test/test_parse_id.ml`, #74): a CVS `$Id$`
  string with a doubled space is malformed, as is a short one.
- `Lm_format_util` and `Lm_print_util` are pinned (`test/test_print_utils.ml`,
  #76): paragraphs at a narrow margin, the y formatter, and separated lists.
- `Lm_digraph` is checked against a model (`test/test_digraph.ml`, #57):
  8,000 random steps, coalescing and multi-edges included, with degrees,
  successors, predecessors, queries, paths and the depth-first orders after
  each.
- `Lm_large_array` and `Lm_large_array_weak` are checked against a model
  (`test/test_large_array.ml`, #58): 3,000 random sets each, with indices at
  the sizes where the arrays grow.
- `Lm_splay_table.MakeTable` (through `Lm_table_util.MakeTable`) is checked
  against a model (`test/test_table_compose.ml`, #59): 6,000 random steps
  over a pool of tables, older versions re-checked after later steps, and
  500 unions under a descending order.
- `Lm_string_hash` (`Lm_hash.MakeHashMarshal` over strings) is checked
  against a model (`test/test_string_hash.ml`, #60): 3,000 random strings,
  some marshalled before use, and 8 pairs of distinct strings with the same
  `Hashtbl.hash`, with intern, equal, compare, sets and tables after each.
- `Lm_parser` is checked against an Earley recognizer
  (`test/test_parser_model.ml`, #77): 300 random grammars (145 build, the
  rest are refused with a conflict), every string of up to 5 tokens,
  acceptance and the derivation the actions build, the same after
  `remove_production` and `union`, and 2,000 random expressions against a
  precedence-climbing evaluator.
- `Lm_lexer`'s zero-width delimiters are checked against a model
  (`test/test_lexer_anchors.ml`, #83): `^ \< \> \y \B \``` over `a`,
  space and newline in the random lexers of `test_lexer_model.ml`. With
  only `^` and `` \` `` the model agrees on every check; the delimiters that
  look at the next character disagree, as below.
- `Lm_lexer` is checked against a brute-force matcher
  (`test/test_lexer_model.ml`, #78): 300 random lexers of up to 4 random
  regular expressions and `\'`, every string of up to 5 characters, one
  call and a whole tokenization, the same after `remove_clause` and
  `union`. At the end of the input `\'` wins over an empty match by any
  clause; the model does the same.
- `Lm_position` is pinned (`test/test_position.ml`, #79): each
  constructor's location and printed form with `debug_pos` off and on, and
  2,000 random chains whose `loc_of_pos` is the same either way. No
  defect.
- `Lm_lexer.search`, `searchto` and `matches` are checked against a model
  (`test/test_lexer_search.ml`, #80): the random lexers of
  `test_lexer_model.ml` on every string of up to 5 characters, one call
  and `searchto` repeated to the end, and quotation-like delimiters under
  `choose = max`.
- `Lm_parser`'s precedence and associativity are checked against a model
  (`test/test_parser_prec.ml`, #81): 300 random tables of 1-4 levels
  (left, right or nonassoc) over six binary operators, 40 random token
  strings each, the parse tree against a precedence-climbing parser,
  where two operators of one nonassoc level side by side are a syntax
  error. No defect.
- `Lm_lexer`'s arguments are pinned (`test/test_lexer_args.ml`, #82).

### Behaviour that the tests record and that may be a bug

- `Lm_lexer`: a group under `?` (`x\(a\)?\(b\)`) is accepted by
  `add_clause` and fails at the first `lex` with "mismatched argument
  counts": `?` is an alternation with the empty string (#82).
- `Lm_lexer`: a delimiter that looks at the next character (`\>`, `\y`,
  `\B`) at the end of a clause never matches (#83): `a\>` matches
  neither `a` nor `a b`, while `a\> ` matches `a ` in `a b`. The lexer
  reaches the clause's final state only after it has read the next
  character, and does not count it as a match.
- `Lm_lexer`: a delimiter that looks at the previous character reached
  right after one that looks at the next, with no character between
  (`a\>*`, `a\>\<`), raises `Invalid_argument "close_next"` when it is
  lexed (#83).
- `Lm_field_table.FieldMTable` assumes each internal name is added once. Two
  entries with one internal name make `to_list` raise `Failure
  "FieldMTable.to_list: some fields are missing"`, and `find_int` returns the
  later entry (#61).
- `Lm_arg` (#69): an option argument that is not a number (`-n x` for an
  `Int` option) escapes as `Failure "int_of_string"` (or
  `"float_of_string"`), not as `BogusArg`, so a caller that catches
  `BogusArg` to print its usage gets an uncaught exception instead. A lone
  `-`, often a name for standard input, is refused as an ambiguous option.
  `Lm_arg`'s user is omake, which carries its own copy.

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
