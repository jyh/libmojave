# libmojave

**libmojave** ("lm") is the OCaml support library that grew out of the MetaPRL
proof assistant, extracted as a standalone library in 2003. It provides an
extended standard library, portable Unix/system bindings with C stubs, and a
suite of data-structure and utility modules that predate (and in places
anticipate) much of what later landed in the OCaml ecosystem. The same library
underpins the OMake build system, which vendors a copy of it.

**Status: archived in 2017; refreshed in 2026 (see below). LGPL-2.1.** Unlike its sibling MetaPRL, libmojave is
camlp-free and structurally simple — a modern-OCaml/dune port is feasible and
is the natural "this lineage still compiles" demonstrator. Of historical
interest as a snapshot of large-scale OCaml systems practice from the 2000s.

## The 2026 refresh: building on a current OCaml

Since October 2026 libmojave builds again, on OCaml 4.14 and 5.4. CI covers
Linux (both versions) and Windows (MinGW and MSVC, OCaml 5.4).

```sh
dune build                     # the library: _build/default/lib/lm.cma, lm.cmxa
dune build --profile threads   # the same, with system threads (as MetaPRL uses)
dune runtest                   # the pinning tests in test/
```

The OMake build still works (`omake`, with OMake 0.10.7 from opam) and
produces the same 108 modules. It stays because MetaPRL builds libmojave
as an OMake subdirectory. Do not mix the two in one tree: an OMake build
writes `stdlib/lm_thread_{core,pool}.ml`, and dune then refuses to build
until they are removed.

The tests in `test/` pin behaviour rather than specify it. Each prints a
function's answers on chosen inputs, and its `.expected` file is the output
as generated when the test was written, so any change shows up as a diff.
Some of the pinned behaviour may be a bug. The filename and glob tests have
separate expected files for Windows.

Known gaps:

- `Lm_dll` (the dynamic-loading foreign-function runtime) is not built. Its
  C side relies on redirecting `caml_local_roots`, which no longer works on
  OCaml 5.
- `Lm_parser` accepts only when the start production consumes the end-of-input
  token itself (for example `prog -> e $eof`).

## Layout

- `stdlib/`, `util/` — the extended standard library and data-structure /
  utility modules.
- `unix/`, `system/` — portable Unix and system bindings.
- `cutil/` — the C stubs backing the system bindings.
- `lib/`, `config/` — the dune build of the library, and its C configuration
  (the same checks OMake's configure step makes).
- `test/` — the pinning tests.

## License

GNU Lesser General Public License, version 2.1. See
[`LICENSE.libmojave`](./LICENSE.libmojave) for the libmojave-specific terms
(including the OpenSSL / OCaml-runtime linking exceptions) and
[`LICENSE.txt`](./LICENSE.txt) for the full LGPL-2.1 text.
