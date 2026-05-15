(lang dune 3.22)

(name wadi)

(using menhir 3.0)

(generate_opam_files true)

(source
 (github desumn/wadi))

(authors "desumn <desumn@posteo.net>")

(maintainers "desumn <desumn@posteo.net>")

(license BSD-3-Clause)

(package
 (name wadi)
 (synopsis
  "A capability-based scripting language embeddable in OCaml programs.")
 (description
  "A capability-based scripting language embeddable in OCaml programs.")
 (depends ocaml fmt menhir menhirLib))
