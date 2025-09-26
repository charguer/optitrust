
OptiTrust is a tool for user-guided, source-to-source transformations.

It is described in this draft paper:

http://www.chargueraud.org/research/2024/optitrust/optitrust.pdf

The project webpage is:

http://optitrust.inria.fr

OptiTrust is a research prototype, under active development.

Warning: in the current version, generating fully detailed HTML reports (with information about every substep) for our case studies requires a very large amount of memory, possibly more than available on your machine. We are working on making typechecking incremental to avoid this problem.

If you are interested in a demo, please get in touch with @charguer.

# Steps for using OptiTrust

- See `INSTALL.md` for installation procedure, to set up the right version of OCaml/Clang/ClangML and configure VSCode/Codium, in particular.
- See `INSTALL_EXTRA.md` for a list of additional useful tools for program optimization.
- See `VSCODE_CUSTOMIZE.md` for useful tips for using VScode or VScodium.
- Read the text below for high level comments on the organization of the repository.
- Check out `case_studies/matmul/matmul_check.ml` and `matmul_check.cpp` to begin with---to produce a full trace you may need a lot of RAM.

# Overview of the implementation

Folders:

- `lib`: implementation of the library
- `tests`: for unit tests, one file per transformation
- `tools`: utilities for parsing, loading traces, execution (runner), testing (generalized runner)
- `include`: header files that need to be included in the C files that are processed using OptiTrust
- `case_studies`: the case studies for OptiTrust
- `doc`: tooling for extracting documentation from the .ml files and the minimal unit tests illustrating each transformation. See `doc/doc.md` for details on documentation generation.
- `_build` (not committed): where generated files are produced
- `_doc` (not committed): where documentation files are produced

In the `lib` folder:
- `utils`: extensions to the standard library of OCaml
- `ast`: definition of data types for representing the AST (in particular the parser should only depend on the AST module)
- `transfo`:  implementation of transformations, organized by categories; by convention takes a target as last argument; most often, transformations are of `unit` type, and modify the AST by side-effect.
- `framework`:
  + `c`: files specific to C, including the translation from C to lambda-calculus; and the C printer
  + `target`: for locating program points
  + `runtime`: infrastructure for executing a script and producing output file and traces, or producing the diff for a specific line
  + `ppx_transfo`: implement support for the `let%transfo` declaration
  + `resources`: description of resource objects, and implementation in the typechecker
  + `no_brace`: special tooling for flattening sequences that appear inside sequences.

In the `include` folder, the main file is `optitrust.h`. This file:
- contains all the functions used to parse annotations; they start with `..` (these annotations functions are nothing but a parsing hack, for attaching annotations in the code)
- contains ghost functions, which are no-op operations used to change resources to alter the view on a memory (think of ghost functions as entailments in Separation Logic)
- contains logical functions, which are pure functions used only for stating specifications (think of logical functions as pure Coq functions).

In the `tests` and `case_studies` folders, for each unit test and each case study, we have several files:
- `.cpp` : contains unoptimized code, plus annotations starting with `__` with arguments in strings; with use of the MINDEX macro for handling multidimensional arrays.
- `.ml`: contains the transformation script, each transformation step starts with `!!`; on this file the user can request via the shortcut a "diff" for a step or a "trace".
- `_out.cpp` (not committed): contains the optimized code, produced when executing the `.ml` script.
- `_exp.cpp`: is a git-versioned copy of `_out.cpp`; the tester claims success if the `_out.cpp` matches the `_exp.cpp`; if legitimate changes are applied to the test, the `_exp.cpp` file should be manually updated to match the `_out.cpp` file (e.g., using `cp` or using the dedicated `./tester fixexp` command).

When producing a trace (see shortcuts in `INSTALL.md`), the trace opens in a browser. In the trace display, on a given step, there are options to control the display:
- mode: `diff` between before-step and after-step, `code before` and `code after` for seeing the code in full before or after the step. If the diff for a step is empty, only the code is displayed.
- `decode`: to see the internal representation of the AST
- `print types`: to see all the type details associated with AST nodes
- `show`: control whether to show only code, or code plus annotations, or code plus annotations plus resources available at every step, or same with all details on what resources each function call is consuming and producing. The detailed display should only be useful for debugging the typechecker
- `full` (on the left) shows all the details of the trace structure.
See `doc/interact.md` for details on the working of the interactive shortcuts.

OptiTrust includes a commit hook to execute the unit tests (not the case studies, currently) before a commit.
To bypass the verification, use `git commit --no-verify "my message"`.










