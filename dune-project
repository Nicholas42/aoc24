(lang dune 3.16)

(name aoc24)

(generate_opam_files true)

(source
 (github Nicholas42/aoc24))

(authors "Nicholas Schwab")

(maintainers "Maintainer Name")

(license GPL-3.0-or-later)

(package
 (name aoc24)
 (synopsis "Solving aoc24")
 (description "Learning ocaml by solving the Advent of Code 2024")
 (depends ocaml dune angstrom batteries containers containers-data iter zarith)
 (tags
  (topics "advent of code")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
