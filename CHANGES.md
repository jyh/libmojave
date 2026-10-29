# Changes

## Unreleased

### Fixed

- `Lm_rformat.format_quoted_string` printed only the first character of a
  string that needs quoting (`"two words"` was shown as `"t"`), and escaped a
  tab as `\r` (#23). It now prints the whole string, escaping a newline, a
  carriage return, a tab and a double quote. metaprl's display forms use it
  for variable and operator names (`refiner/reflib/dform.ml`,
  `simple_print.ml`).

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

### Behaviour that the tests record and that may be a bug

- `Lm_list_util.remove_suffix` returns `[]` where the prefix is expected:
  `remove_suffix [1; 2; 3] [3]` is `[]`. It can only return `[]` or raise.
- `Lm_list_util.splitup n` makes chunks of n+1 elements, last chunk first
  (`splitup 2 [1; ...; 7]` is `[[7]; [4; 5; 6]; [1; 2; 3]]`), although its
  interface says "size no more than n".

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
