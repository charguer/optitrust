# OptiTrust Transformation Knowledge

This file is a compact orientation map for prompt generation. It is not a full
API reference. The prompt should prefer exact signatures from `lib/transfo/` and
usage examples from `tests/**/**_doc.ml` before emitting code.

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
- use examples from `tests/` to choose exact function names;
- include required non-target arguments such as tile sizes, names, clauses, or
  destination targets;
- ask for missing parameters when no safe default exists;
- do not invent transformations from compiler terminology alone;
- state when an output is a candidate script rather than a proven optimization.

## Useful Example Families

- `tests/function/inline_simple/*_doc.ml`
- `tests/loop/unroll/*_doc.ml`
- `tests/loop/tile/*_doc.ml`
- `tests/loop/fusion/*_doc.ml`
- `tests/loop/fission/*_doc.ml`
- `tests/sequence/insert/*_doc.ml`
- `tests/variable/inline/*_doc.ml`
- `tests/omp/*`
- `case_studies/matmul/`
- `case_studies/harris/`
