# libmojave

**libmojave** ("lm") is the OCaml support library that grew out of the MetaPRL
proof assistant, extracted as a standalone library in 2003. It provides an
extended standard library, portable Unix/system bindings with C stubs, and a
suite of data-structure and utility modules that predate (and in places
anticipate) much of what later landed in the OCaml ecosystem. The same library
underpins the OMake build system, which vendors a copy of it.

**Status: archived (2017), LGPL-2.1.** Unlike its sibling MetaPRL, libmojave is
camlp-free and structurally simple — a modern-OCaml/dune port is feasible and
is the natural "this lineage still compiles" demonstrator. Of historical
interest as a snapshot of large-scale OCaml systems practice from the 2000s.

## Layout

- `stdlib/`, `util/` — the extended standard library and data-structure /
  utility modules.
- `unix/`, `system/` — portable Unix and system bindings.
- `cutil/` — the C stubs backing the system bindings.

## License

GNU Lesser General Public License, version 2.1. See
[`LICENSE.libmojave`](./LICENSE.libmojave) for the libmojave-specific terms
(including the OpenSSL / OCaml-runtime linking exceptions) and
[`LICENSE.txt`](./LICENSE.txt) for the full LGPL-2.1 text.
