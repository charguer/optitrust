

# Build system

Dune is used to build a binary program called `optitrust-runner`
and a library with the compiled versions of the transformations
implemented in `src/`.

A user transformation script is compiled using dune as a `.cmxs` file,
whose path is provided as argument to `optitrust-runner`.

[TODO: put a template_minimalist folder with a minimalist working demo, with optitrust installed.]
[TODO: put a template_developer folder with a minimalist working demo, with compilation of optitrust.]

[TODO: test whether bytecode compilation can be faster]




# Test system

Dune is also used to build a binary program called `tester`.

- A call of the form `./tester all` executes all the unit tests.
- A call of the form `./tester foo1 .. fooN` executes only the unit tests `foo1` .. `fooN`.

For efficiency reasons, the unit tests are not compiled one by one.
Instead, the tester generates a file called `tests/batch/batch.ml`,
which essentially contains the concatenation of the requested unit tests.

The tester then executes the program `batch.ml` and produces a summary of
the test results, indicating :

- tests that failed to run
- tests whose output differs from the expected output
- tests that succeeded.

[TODO: in the future, there will be a caching mechanism for parsed ASTs.]

More details are available in the source `tests/tester.ml`.
[TODO: migrate the doc here.]




# WORK IN PROGRESS


NOT NEEDED?   ENABLED_IF    OR  rm -f tests/batch/batch.ml

plus simple : rm -f dune  voir tester.ml



./tester  should work


==> ocaml 4.14.1


make setup
setup:
	opam install \
	  "dune>=2.0" "cppo" "pprint>=20200410" "seq" "odoc" \
	  "monolith" "ocamlfind"


add .PHONY

merge all in master


remove:
  do_or_die "dune build -p optitrust @install";
  do_or_die "dune install";

  with rm batch.ml
  or   make clean_batch



TODO:  séparer script_cpp from batching_script_cpp



   => test of transfo inline




   tests/loop/inline ?
      inline.ml
        => let _ = script_cpp
           let _ = script_ml
           let _ = script {both}

      inline_src.cpp
      inline_exp.cpp
      inline_src.ml
      inline_exp.ml


  tests/loop/inline/_build/
      inline_out.ml
      inline_out_enc.ml
      inline_out_ast.ml
      inline_out_ser.ml
      inline_out.cpp
      inline_out_enc.cpp
      inline_out_ast.cpp
      inline_out_ser.cpp
         .js
         _trace.js



