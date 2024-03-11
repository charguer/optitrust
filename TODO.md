

# Cleanup

- Deprecate the use of `optitrust_flags.sh`.

- Rename functions in trm.ml and typ.ml so that they all have the trm_ and typ_ prefix.
  Replace all trm_ and typ_ prefixes with Trm. and Typ, after making sure to rename
  reserved names, such as `fun` (rename to `func`) or `let` (rename to `bind`).

- Remove file internal.ml.

- Clean up path.ml. A bunch of functions should be migrated from internal.ml and target.ml
  All functions operating on path should be named in the form `Path.foo`.

- Remove file ast_data.ml, moving stuff into typing.ml and cpp.ml.

- Cleanup tools/*
  Declare  List.ml and inside place include Stdlib.List  and add extra functions

- TO REMOVE:
../src/internal.ml
../src/convention.ml


# Features

- `Trm_files` do deal with multiple files, and simplify includes
- encode Module/namespace the same way?
- term comparison function
  - compare modulo names?
  - compare marks?
  - save diffs?
  - compare annots?
    - compare pragmas / cstyle ?
  - compare types?
  - compare computed resources?
  - compare contracts?

# Typechecking

- (AC) Implement an incremental typechecker, able to retypecheck only the parts that are changed.

- Resource checker should complain if a function without a contract contains ghost or contracts;
  in any case, should not make resources available in any subterm of a function without a contract.
  to see..

- Support higher order contracts

- Add Ghost_pair.fusion


# Shortcuts

- Bind a shortcut for compiling the /src folder
```
   "when": " config.workspaceKeybindings.OptiTrust.enabled && (resourceDirname =~ /^.*\/verified_transfo\/src\/src\/.*$/ || resourceDirname =~ /^.*\/verified_transfo\/src\/src\/transfo\/.*$/)"
   // There is probably a way to query whether we are at a subdir of a given dir
```

- Bind a shortcut for replaying the last `view_result` action
  (including recompiling the /src folder)

 ```
   {
      "label": "Compile the last-tried test(s)",
      "type": "shell",
      "command": "./tester",
      "args": [
        "__last"
      ]
      "options": {
        "cwd": "${workspaceFolder}"
      }
    },
```


- Bind a shortcut for executing F6 under the assumption
  that /src folder has not changed (skipping dependency tests)


- Modify the behavior of dump-full-trace shortcut (shift+F5),
  so that, by default, it stores only diffs in the JS file and not all ASTs
  (it's too expensive).

- Bind a shortcut for executing the tester on the current test
```
    {
      "label": "Compile the current test",
      "type": "shell",
      "command": "./tester",
      "args": [
        "${relativeFile}"
      ]
      "options": {
        "cwd": "${workspaceFolder}"
      }
    },
    {
      "label": "Compile the current test with -dump-trace",
      "type": "shell",
      "command": "./tester",
      "args": [
        "${relativeFile}",
        "-dump-trace"
      ]
      "options": {
        "cwd": "${workspaceFolder}"
      }
    },
```

- Add a shortcut for killing the current task.
  Document the following entry into `keybindings.json`
```
   {
    "key": "ctrl+shift+alt+t",
    "command": "workbench.action.tasks.terminate"
   }
```


# Tester

- tester should call gcc on test outputs if they don't match the expected outputs

- Serialize the expected output files to enable comparisons at the AST-level
  rather than at the text level. Need to check dependencies to ensure that
  serialized files are up-to-date wrt text files.

- Reimplement in OCaml the generation of the batch.ml file.

- Implement a command `tester -fix-diff` for directly opening in meld
  all the tests that don't produce expected result. (Better than having
  a command line to copy-paste). The list of relevant files would be
  produced in a file `tester_diff.txt`  at a prior run of `tester`.

- Implement a command `tester -fix-fail` for directly opening in VSCode
  all the tests whose execution failed. For each, open both the .ml and
  the .cpp file. The list of relevant files would be produced in a file
  `tester_failed.txt` at a prior run of `tester`.

- Implement a command `tester -accept-all-diff` for copying all `_out.cpp`
  onto the `_exp.cpp`. Useful when changing the CPP printer code.

- Implement a command `tester -fix-ignored` for running all the ignored
  tests. The tests considered should be located in the targeted folders,
  or in the subdirectories of the current `pwd`. The list of relevant files
  could be produced in a file `tester_ignored.txt` at a prior run of `tester`.
  Running this command would not modify the file `tester_ignored.txt`,
  however it would update the files `tester_diff.txt` and `tester_failed.txt`.


# Tests

- For every test in `basic` that conflicts with a test in `combi`, add `_basic`
  as suffic to the test name. Then, merge `basic` into `combi`.

- Create one subfolder for each test or for each group of tests (to be determined).
  For example `tests/loop/loop_swap.ml`.

- Add a shortcut for jumping from the implementation of a transformation to
  the unit tests that goes with it. If the unit test does not exist, it should
  be created on-the-fly, including the git-add command.

- Fix as many ignored tests as possible.

- When tests are fixed, revive the PIC demo (requires a typechecker, otherwise
  it is too slow).

- "failure_expected: the operation was supposed to fail but it didn't"
  does not report a useful backtrace

- "failure_expected (fun _e -> true)" should be replaced with a catch of the
  relevant exception, e.g. RessourceError, etc.

# VSCode

- Investigate whether there is a better way to avoid the sandboxing of VSCode
  task execution, to avoid the watch.

- Figure out whether it is possible to execute `watch.sh` at the session start.
  I tried `gnome-session-properties`, add an entry with the name `optitrust watcher`
  and the path to the watch script (e.g. `~/optitrust/src/.vscode/watch.sh`)
  but it did not seem to work.


# Documentation

- apply changes in tree.md and remove this file

- dune build @doc on a single file / file subset
  - exclude compcert from doc

- Complete the `/doc/*.md` files that present the structure of OptiTrust.
  Move them to *.ml documentation.

- Extend the tester program to generate a version of batch.ml that executes
  only the `doc_script_cpp` and ignores the `script_cpp` calls. Follow the
  patterns in `Tests.Makefile` for producing `%_doc.js` files and `doc.html`.
  When done, delete `Tests.Makefile`

- Bind a shortcut (previously `ctrl+F6`) for viewing the documentation
  generated by the current test file.

- Integrate the `doc.html` page into the sphinx doc.

- Create a script for automatically uploading the documentation on the website.

- Create a sandbox case study, that users can modify in place to try things out.
  Use e.g. the contents of matmul_mini.

- Complete cpp.ml with the exact list of features that are supposed to be supported.
  And list the common C features that are not supported.


# Parser

- When invalid C code is provided, we should use clangml in a mode that raises
  an error, not a mode that silently deletes the boggus lines.

- Serialization does not work yet with CPP member functions; eg cpp_varids.ml
  and see comment in class_member_elim

- Keep marks on reparse: tests same ASt and reuse marks at paths

- Eliminate `Trm_arbitrary` on the fly
  - on creation, set flag to 'partial/full/none'
  - after step, full or partial reparse on functions that contain arbitrary terms
  - also have 'disabled' flag to prevent reparse temporarily for combis (e.g. insert_multi)

- Decide whether we want to preserve support for CompCert parser. The benefits
  was fast parsing of simple code snippets, such as rewriting rules.

- It required the command:
```
 sudo mkdir -p /usr/local/lib/compcert; cp ~/verified_transfo/src/src/cparser/include/* /usr/local/lib/compcert"
```

- See how we want to parse integer literals.
  Currently we have: `(expr "2")` vs `(lit "2")`,
  see e.g. loop shift transformation.
  Maybe use of piece of code such as:

```ocaml
  let lit l =
       try trm_int (int_of_string l)
       with _ ->
       try trm_double (float_of_string l)
       with _ ->
       code (Lit l)
```

- (GB) Add a conversion from OCaml parsetree (via compiler libs) into OptiTrust AST.
  Requires the typechecker to be implemented.
  Requires extensions to the AST to represent type arguments, type applications,
  and OCaml style type definitions, in particular.

- (GB) Add infrastructure for testing transformations on OCaml files.


# Printer

- Add a CPP code printer that directly outputs well-indented code.

- Add a printer for OCaml AST. Requires the integration of the OCaml parser.
  We can use ocaml-style annotation, e.g., to know whether the prefered layout
  for a conditional is one line, on 3 lines, or on 4 lines.

# Debug

- reparse and retype all intermediate steps up to depth d
- allow disabing for invalid intermediate steps

# Repo

- (AC) Move `src/` one level up. Migrate several folders from the root to another repo.

- Add a LICENSE file, such as "gnu gpl 3"


# Install

- See if we can implement a `make setup` command for installing all necessary software.
  See example in:
  https://gitlab.inria.fr/fpottier/sek/-/blob/master/Makefile


# Transfo implementation

- Follow the loop_swap pattern of having one file per transformation.

- Perform renaming following the pattern; swap

```
   // function on subterm (ast local)
   let swap_on (index : int) (t : trm) : trm =
      // only these functions produce new pieces of ast

   // function on paths (ast global)
   let swap_at (index : int) : Target.Transfo.local = // trm -> path -> trm
     Target.apply_on_path (swap_on index)

   // function for end-user (ast global)
   let swap (tg : target) : unit =
     apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
       (fun t (p,i) -> swap_at_path i t p) tg
```

- implement a transfo 'delay_write': `WR(X = Y); RD(X); --> RD(Y); WR(X = Y)`

# Trace

- related:
  - Justifications Variable.inline/Function.inline/Matrix.elim_mindex
  - pre/postprocessing (1. infer contracts; then elim header)
  - pretty-print Group and range
  - renaming '__sreads', '__xreads', ..

- add expand/collapse feature on trace steps
  - green checks are displayed separately on the right

- remove line display by default

- replace pink script box with entire script, clickable

- display: minimal diff (default), diff with all code (click to expand, or button), collapse one side and/or change vertical split (gridify)

- make scrolling independant between left steps and right diffs (gridify)

- Add infrastructure for executing a test with reparsing after every step,
  or after every big-step (can be controlled by a per-test flag).

- Revive the trace.ml code for generating a cpp file for every small/big step.

- Verify that a ghost/contract modifiying transformation indeed modifies only ghosts and contracts


# Flags

- Reorganize the `flag.ml` file to organize flags in records.
  What matters is to separate the test-dependend flags, so that we can reset
  them at every step of the `batch.ml` execution.


# Marks

- Refine the implementation of MList as a:  `type mlist = (mark * trm * mark) list`,
  to store marks before and after each statement.
  This will behave better for e.g. loop splitting.

- Refine the implementation of MList to replace the list by a sequence type
  implemented using a balanced binary tree, to improve asymptotic performance
  of update.

- For top-level sequences, we may want to also include an index, that is, a table
  mapping function names to their index. This index could be computed lazily,
  at the first query, so that the interface would be transparent for the user.


# Stats

- Cleanup the `stats.ml` file. The timing-related mechanism might be no
  longer needed if we can fit everything into steps, however it is not
  trivial to view initial-parse and final-dump-trace as steps of the trace.
  Implement a record of counters, for keeping track of the number of:
  term allocated, number of recursive calls in target resolution,
  total cost of string allocated for trm representation for target resolution.
  When opening a state, we would save the current values of all the counters
  (by taking a copy of the record).
  When closing the state, we could compute the difference of the current
  values with the values at the time of opening the step.
  We can then save the stats together with the step description.

- Make the stats optionnally available in the trace view.


# Target mechanism

- (AC) Reimplement the computation of string representation for subexpressions.
  These should be computed lazily, and stored via a side effect into the AST.

- Optimize the insertion/deletion of marks at a list of path: this should be done
  in a single pass.

- Introduce a caching mechanism for marks used in Target.iter, when multiple marks
  are placed. The path to each mark could be saved. When looking up the i-th mark,
  we can look (1) at the saved path, and (2) only through newly created nodes, i.e.
  nodes that have been allocated posterior to the initial target resolution.
  We can use timestamps on every node to know whether they are more recent or not.

- Replace calls to the deprecated function target_iter by Target.iter.
  Likewise for apply and its variants.

- A combinator `Target.group_by prefix ?drop_prefix=false tg` that resolves a target `tg` to paths `ps`,
  then partition the paths according to the result of `prefix p` of each path `p`.
  Returns a list of the form (prefix, paths_with_that_prefix|suffixes)
  from the decomposition of the path,
  of type `path * path list`. The list should be ordered with smallest prefixes first.
- Another combinator `Target.apply_with_group_by prefix tg (fun prefix ps -> transfo)`
  that iterates the transfo on the list.

- resolve_path does not yet include Dir_contract

# Case study

- (AC) Fix pic_demo and remove `Loop.hoist_old`

- (TK) Add a case study in the style of BLAS code

- Add a case study for optimizing a Hashtbl implementation,
  showing data structure refinement from abstract sequences to arrays or lists or vectors.


# Rendering

- Augment the trace view with performance figures for every small-step.

- Augment the trace view with floating-point divergence figures for every small-step.

- Create a mechanism for scripting the generation of interactive demos, for the webiste.

# AST representation

- Add a checker for AST invariants


# Debugger

- Try usage of ocamldebug
  https://github.com/hackwaly/ocamlearlybird
  https://github.com/ocamllabs/vscode-ocaml-platform#debugging-ocaml-programs-experimental


# Pending Issues to Track

To learn more, just grep for the tag in the source code:
```sh
grep -r '[tag]' .
```

- #unop-inc-dec
  - Looks like we have "Unop_post_inc *(var x)" which does not work for resources?
- #advanced-scoping-check
- #var-id
  - hook to let builtin var with id and set correct id in code after parsing
  - Some APIs need to evolve, picking between string or var
  - because we have var-id, can replace combi cVar/cFor/cFun name queries with id queries cVarId/cForId/cFunId
  - #var-id-dir-name
  - #this-id
  - #type-id
- #hyp
- #odoc


# Debugging functions

```ocaml
  (* tg:target, could denote one or several targets *)
  (* all functions below include an optional argument [~subterms:true]
     that can be used to include printing for every subterm of the targeted term *)

  Show.trm tg;      (* prints a targeted term, in C syntax *)
  Show.trm_enc tg;  (* prints a targeted term, in the internal (encoded) OptiTrust syntax *)
  Show.trm_ast tg;  (* prints a targeted term, in raw AST syntax *)
  Show.typ tg;      (* prints the type of a targeted term, in C syntax *)
  Show.typ_ast tg;  (* prints the type of a targeted term, in raw AST syntax *)
  Show.typ_ctx tg;  (* prints the type context *)
  Show.res tg;      (* prints the resource usage of a targeted term *)
  Show.res_ast tg;  (* prints the resource usage of a targeted term, in raw syntax *)   (* need res_enc? *)
  Show.res_ctx tg;  (* prints the full ctx available for typing a targeted term *)
  Show.path tg;     (* prints the path reaching a targeted term *)
  Show.path ~relative:tg2 tg;  (* prints the path reaching a targeted term, removing the prefix reaching tg2 *)
  Show.arith tg;    (* prints the AST obtained by reification in the Arith module for arithemtic expressions *)
  Show.marks tg;    (* prints the list of marks carried by the term (for sequences, prints in-between marks too) *)
  Show.annot tg;    (* prints the list of annotations associated with a term *)
  Show.desc tg;     (* prints the trm_desc of the term, i.e. like raw AST syntax but without subterms *)
  Show.stmt tg;     (* prints the is_statement information of the term *)
  Show.info tg;     (* combination of show_desc; show_annot; show_stmt; show_typ; show_res; show_marks *)
```

# Generalized uninit

```c

void f(int* p, bool b) {
  __consumes("Uninit(p ~> Cell)"); // Init(i, p ~> Cell)
  __produces("Uninit(p ~> Cell)"); // Init(i, p ~> Cell)
  if (b) {
    *p = 3;
  }
}

void g(int* p, bool b) {
  __consumes("Uninit(p ~> Cell)");
  __produces("p ~> Cell");
  *p = 6;
  f(p, b);
}

void f2(int* p, bool b) {
  __consumes("Uninit(p ~> Cell)");
  __produces("p ~> Cell");
  int z = 3;
  if (b) {
    *p = z;
  }
  // requires annotation: if b then p ~> Cell else Uninit(p ~> Cell)
  z++;
  if (!b) {
    *p = z;
  }
}

```
