(lang dune 3.6)
(using menhir 2.1)

(generate_opam_files true)

(package
  (name optitrust)
  (depends
    (dune (>= 2.4.0))
    (ocaml (>= 4.08.0))
    (clangml (= 4.8.0))
    (pprint (>= 20200410))))

