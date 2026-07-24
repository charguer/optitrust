# OptiTrust Transformation Knowledge

This file is a compact orientation map for prompt generation. It is not a full
API reference. The AI can only use API details, examples, source code, traces,
and errors included in the current request. Do not rely on unstated files or
examples.

## Common Modules

- `Loop`: loop-level transformations such as unrolling, tiling, fission,
  fusion, hoisting, shifting ranges, reordering, and parallelization helpers.
- `Loop_basic`: lower-level loop transformations used directly in some tests.
- `Function`: function inlining and related function transformations.
- `Variable`: variable folding, unfolding, inlining, insertion, renaming, and
  binding.
- `Sequence` and `Sequence_basic`: sequence introduction, insertion, deletion,
  and grouping of instructions.
- `Instr`: instruction movement, copy, read-last-write, and accumulation.
- `Matrix` and `Matrix_basic`: matrix/local storage transformations,
  delocalization, tiling, storage folding, and simplifications.
- `Omp` and `Omp_basic`: OpenMP pragmas such as parallel, parallel_for, simd,
  task, target, target_data, and related clauses.
- `Cleanup`: cleanup passes usually applied after larger transformations.

## Prompt Policy

For command-to-script and code-to-script prompts:

- map user words to a known module/function only when the mapping is clear;
- use the examples in this knowledge to choose function names and argument
  order;
- include required non-target arguments such as tile sizes, names, clauses, or
  destination targets;
- ask for missing parameters when no safe default exists;
- do not invent transformations from compiler terminology alone;
- state when a generated full-file script is a best-effort proposal rather than
  a proven optimization.

## Known Transformation Shapes

Function transformations:

```ocaml
!! Function.inline [cCall "f"];
!! Function.inline [cTopFunDef "main"; cCall "f"];
!! Function.inline ~delete:true [nbMulti; cCall "f"];
!! Function.inline_def [cFunDef "helper"];
```

Use `Function.inline` when targeting call sites. Use `Function.inline_def` when
the user asks to inline a helper function definition into its callers and the
definition target is clear.

Loop transformations:

```ocaml
!! Loop.unroll [cFor "i"];
!! Loop.unroll [nbMulti; cFor "i"];
!! Loop.unroll ~nest_of:2 [nbMulti; cFor "i"];
!! Loop_basic.unroll [cFor "i"];
```

Use `Loop.unroll` for normal scripts. Use `Loop_basic.unroll` only when a basic
version is explicitly requested or shown in examples. Add `nbMulti` only when
the user wants all matching loops.

Tile a loop with a literal tile size:

```ocaml
!! Loop.tile (lit "32") ~index:"bi" ~bound:TileDivides [cFor "i"];
!! Loop_basic.tile (lit "32") ~index:"bi" ~bound:TileDivides [cFor "i"];
```

Loop reorder, swap, fission, fusion, and shift examples:

```ocaml
!! Loop.reorder_at ~order:["bi"; "bj"; "bk"; "i"; "k"; "j"] [cPlusEq ~lhs:[cVar "sum"] ()];
!! Loop.reorder ~order:["j"; "i"] [cFor "i"];
!! Loop.swap [cFor "j"];
!! Loop.fission [cForBody "i"; tBetweenAll];
!! Loop.fission [tBefore; cFor "i"; cWriteVar "s"];
!! Loop.fusion_targets [cFor "y" ~body:[cArrayWrite "out"]];
!! Loop.shift StartAtZero [cFor "y"];
```

Use loop reordering only when the order is known. Use fission when the user asks
to split a loop or separate statements. Use fusion when the source has adjacent
loops with compatible iteration domains and the user asks to fuse or optimize
that pattern.

Insert a parsed statement before a target:

```ocaml
!! Sequence_basic.insert ~reparse:true (stmt "a++;") [tBefore; cVarDef "x"];
```

Sequence transformations:

```ocaml
!! Sequence_basic.delete [cVarDef "tmp"];
!! Sequence_basic.split [tAfter; sInstr "b = 0"];
!! Sequence_basic.intro 2 [cFor "i"];
!! Sequence_basic.intro_between [tBefore; cVarDef "a"] [tAfter; cVarDef "b"];
!! Sequence.intro ~on:[cVarDef "a"] ();
!! Sequence.elim [cSeq ~instrs:[[cVarDef "tmp"]] ()];
```

Use sequence targets for grouping, splitting, inserting, or deleting
instructions. For insertion, the target should usually be positional
(`tBefore`, `tAfter`, `tFirst`, or `tLast`).

Rename a local variable inside a marked scope:

```ocaml
!! Variable.local_name ~var:"s" ~local_var:"t" [cMark "t_scope"];
```

Variable transformations:

```ocaml
!! Variable.inline [cVarDef "x"];
!! Variable.unfold ~delete:true [cVarDef "x"];
!! Variable.fold ~at:[cVarDef "dst"] [cVarDef "src"];
!! Variable.insert ~reparse:true ~typ:(ty "int") ~name:"b" ~value:(lit "2") [tAfter; cVarDef "a"];
!! Variable.insert_and_fold ~typ:(ty "const int") ~name:"a" ~value:(expr "x*y") [tBefore; cVarDef "r"];
!! Variable.rename ~into:"new_name" [cVarDef "old_name"];
!! Variable.renames (AddSuffix "1") [cFunBody "main"];
!! Variable.reuse (var "x") [cVarDef "y"];
!! Variable.elim_redundant [cVarDef "tmp"];
```

Use variable transformations only when the variable definition or scope is
clear. Prefer `Variable.inline` for replacing uses by the definition value.
Prefer `Variable.insert` or `Variable.insert_and_fold` when creating a new named
intermediate value.

Instruction transformations:

```ocaml
!! Instr.move ~dest:[tBefore; cVarDef "dst"] [cVarDef "src"];
!! Instr.copy ~dest:[tAfter; cVarDef "dst"] [cVarDef "src"];
!! Instr.delete [cVarDef "tmp"];
!! Instr.inline_last_write [cReadVar "x"];
!! Instr.accumulate ~nb:8 [nbMulti; sInstrRegexp "res.*\\[0\\]"];
```

Use `Instr.move` and `Instr.copy` only with an explicit destination. Use
`Instr.inline_last_write` when a read should be replaced by its most recent
write and the read target is clear.

Matrix/locality transformations:

```ocaml
!! Matrix.stack_copy ~var:"sum" ~copy_var:"s" ~copy_dims:1 [cFor "j"];
!! Matrix.storage_folding ~dim:0 ~size:(int 4) [cVarDef "buffer"];
!! Matrix.elim [cVarDef "tmp_matrix"];
!! Matrix.elim_mops [];
!! Matrix.local_name_tile ~var:"a" ~local_var:"a_local" [cFor "i"];
```

Use matrix transformations only when arrays/matrices and dimensions are clear.
If dimensions, local variable names, or allocation targets are not clear, state
the missing details or choose a simpler local transformation first.

OpenMP transformations:

```ocaml
!! Omp.parallel_for [tBefore; cFor "i"];
!! Omp.parallel_for [cFor "i"];
!! Omp.simd [nbMulti; cFor "j"];
!! Omp.simd ~clause:[Simdlen 8] [cFor "x"];
!! Omp.parallel [Private ["tmp"]] [tBefore; cSeq ()];
!! Omp.for_ ~clause:[Collapse 2] [cFor "j"];
!! Omp.target [Map_c (To, ["a[:N]"]); Map_c (From, ["out[:N]"])] [tBefore; cFor "i"];
```

Use `Omp.parallel_for` for a loop-level parallel-for pragma. Use `Omp.simd` for
vectorization. Include clauses only when the user asks for them or the source
clearly implies them.

Run cleanup after transformations that may leave simplifiable code:

```ocaml
!! Cleanup.std ();
```

## Transformation Choice Rules

- For explicit commands, generate exactly the requested transformation when the
  API shape is known.
- For vague full-file optimization requests, choose conservative local
  transformations first: inline obvious helper calls, unroll small static loops,
  then cleanup.
- For tiling, require a tile size, tile index name, and a clear loop target. If
  these are missing, either choose a conservative default and state it or ask for
  clarification when no safe default exists.
- For loop reordering, require the full intended order and a stable target
  inside the loop nest.
- For insertion, use `Sequence_basic.insert ~reparse:true` with `stmt "..."`
  and a positional target such as `[tBefore; ...]` or `[tAfter; ...]`.
- For OpenMP, only emit an `Omp` or `Omp_basic` call when the requested pragma
  and target are clear.
- If the visible source has repeated names, include target context or occurrence
  selectors to avoid accidental matches.

## Full-File Script Strategy Rules

- Start from the visible code, not from a guessed benchmark identity.
- If the file contains a simple named helper function called from one or more
  kernels, a reasonable first step is `Function.inline` or
  `Function.inline_def`.
- If the file contains small statically bounded loops, a reasonable first step
  is `Loop.unroll` on those loops, followed by `Cleanup.std ()`.
- If the file contains a matrix multiplication-like triple loop, a reasonable
  plan may include tiling outer loops, reordering the loop nest around the
  accumulation, optional SIMD on the innermost loop, and cleanup. Always adapt
  names to the visible loops and arrays.
- If the file contains adjacent loops over the same range, a possible plan is
  fusion, but mark it medium confidence unless dependencies are obviously safe.
- If the file contains reductions into a scalar, be cautious: unrolling, local
  names, fission, or parallelization may require reduction semantics.
- If the request is simply "generate the full script" with no specific goal,
  still emit a conservative runnable script. Prefer one or two safe local
  transformations plus cleanup over an ambitious multi-step optimization.

## Common Mistake Prevention

- Do not invent module names, constructor names, or optional arguments not shown
  in this knowledge or current examples.
- Do not use a target that may match multiple nodes unless the script explicitly
  uses `nbMulti` or an occurrence selector.
- Do not use `tBefore` or `tAfter` for transformations that operate directly on
  a node, such as `Loop.unroll [cFor "i"]`.
- Do use `tBefore` or `tAfter` for insertion, movement, and pragma placement.
- Do not claim validation passed. Provide commands and expected evidence only.
- If exact syntax is uncertain, still show the best-effort script and list the
  uncertain API or argument in the assumptions/risk sections.
