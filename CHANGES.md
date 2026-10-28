# Changes

## Unreleased (2026 refresh)

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
- Warning flags in numeric form. Warnings added in OCaml 4.13 and later (67,
  68, 69, 70) are not errors.

### Tests

- Pinning tests in `test/` (#5, #6, #7, #9, #11) for `Lm_string_util`,
  `Lm_filename_util`, `Lm_glob`, `Lm_symbol`, `Lm_set`, `Lm_map`,
  `Lm_printf`, `Lm_lexer`, `Lm_parser`, `Lm_num`, `Lm_channel` and
  `Lm_thread`.
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
- `Lm_ctype` declares C primitives (`is_alnum` and five others) that
  libmojave does not define.

### Repository

- Commit, branch-name and pull-request checks run on every push (#1, #2).
