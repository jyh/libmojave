(lang dune 3.0)
(name libmojave)

(generate_opam_files true)

(source (github jyh/libmojave))
(license "LGPL-2.1-only") ; plus the linking exception in LICENSE.libmojave
(authors "The Mojave Group, Caltech" "Jason Hickey")
(maintainers "Jason Hickey")

(package
 (name libmojave)
 (synopsis "General-purpose OCaml library from the MetaPRL and OMake projects")
 (depends
  (ocaml (>= 4.14))
  (dune-configurator :build)))
