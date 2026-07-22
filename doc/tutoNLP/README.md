# OptiNLP Target Tutorial And Test Suite

This directory is both a tutorial for new OptiNLP users and a target-generation
test suite for AI models.

As a tutorial, it teaches OptiTrust target writing through repetition. A user
opens one `.cpp` file, studies the source code, reads the natural-language
request that should be given to OptiNLP, sends that request to the AI, and then
compares the returned target with the expected answer. Repeating this workflow
across many small examples helps new users learn how targets are written, when
to qualify targets by function or body context, when occurrence selectors are
needed, and when the correct answer is to ask for clarification instead of
guessing.

As a test suite, it evaluates whether the selected AI model can generate
correct OptiTrust targets. The same cases can be used to compare Gemini,
OpenAI, mock behavior, or any future provider by checking whether the model
returns the expected target, an accepted equivalent target, or the expected
clarification for ambiguous requests.

Each test has its own source file and matching expected-result file so it can
be practiced, evaluated, or validated independently.

The `.cpp` file is the input source to give to the target-generation prompt.
The matching `.ml` file contains the expected target and a `Show.target`
validation script. Ambiguous or missing-context tests use a matching `.md` file
instead of `.ml`, because the expected result is a clarification request rather
than an executable target.

## Layout

```text
doc/tutoNLP/
  loops/
  functions_calls/
  variables/
  memory_access/
  positions_spans_marks/
  ambiguity_fallback/
```

Each test pair uses the same basename:

```text
loops/l1_unique_loop.cpp
loops/l1_unique_loop.ml

ambiguity_fallback/l5_line_without_source.cpp
ambiguity_fallback/l5_line_without_source.md
```

The older starter evaluation set remains in:

- `tools/optiNLP/eval/target_cases.md`

The target vocabulary and style rules are documented in:

- `tools/optiNLP/knowledge/targets.md`
- `tools/optiNLP/knowledge/target_description.md`
- `tools/optiNLP/prompts/01_target_generator.md`

## How To Use This As A Tutorial

For one practice case:

1. Open one `.cpp` file from this directory.
2. Read the code and identify the node the request is probably talking about.
3. Read the `// Request:` comment at the top of the `.cpp` file.
4. Send the `.cpp` source and that request to OptiNLP.
5. Compare the target returned by the AI with the expected target in the
   matching file.
6. Open the matching `.ml` or `.md` file with the same basename only when you
   want to verify the answer or inspect the expected clarification.
7. Repeat the same workflow on the next case.

The goal is not only to get one answer right. The goal is to build intuition by
seeing many examples of target syntax, including simple semantic targets,
function-qualified targets, repeated-name targets, occurrence selectors,
relative positions, spans, marks, memory accesses, and ambiguity cases.

## How To Use This To Test An AI Model

For a single model-evaluation case:

1. Give the assistant `tools/optiNLP/prompts/01_target_generator.md`.
2. Give it the relevant target knowledge files.
3. Paste one `.cpp` source file from this directory.
4. Use the `// Request:` comment from the `.cpp` file as the user request.
5. Record the model response.
6. Compare the response with the expected target or expected clarification.

A response passes when it:

- uses existing OptiTrust target constructors only;
- returns the expected target or an equally specific accepted variant;
- asks for clarification when the case is intentionally ambiguous;
- avoids `sInstr`, `sInstrRegexp`, `sExpr`, and `sExprRegexp` when semantic
  selectors can express the target;
- includes a short validation suggestion using `Show.target` when source is
  available.

For executable target cases, the matching `.ml` file can also be used as a
target-validation script in the usual OptiTrust `Run.script_cpp` style.

This makes the directory useful for two different audiences:

- new users, who can practice target writing step by step until the syntax
  becomes familiar;
- OptiNLP developers, who can test whether the currently selected AI model is
  able to generate correct targets reliably.

## Difficulty Levels

Level 1: unique semantic target.

- One obvious loop, function, call, variable, array access, or position.
- Example targets: `[cFor "i"]`, `[cFunDef "main"]`,
  `[cArrayWrite "A"]`.

Level 2: contextual target.

- The same kind of target appears more than once, but function, loop, or body
  context resolves it.
- Example targets: `[cFunBody "main_loop"; cFor "i"]`,
  `[cTopFunDef "main"; cCall "foo"]`.

Level 3: multi-target generation.

- The user asks for all, both, every, or exactly N targets.
- Example targets: `[nbMulti; cFor "x"]`,
  `[multi cVarDef ["ix"; "iy"]]`, `[nbExact 2; cFor "i"]`.

Level 4: structural disambiguation.

- The target must be selected by body contents, call arguments, read/write
  role, or a relative position.
- Example targets: `[cFor "y" ~body:[cArrayWrite "out"]]`,
  `[cCall "swap" ~args:[[cVar "a"]; [cVar "b"]]]`.

Level 5: hard case-study target.

- The target involves nested loops, marks, spans, occurrence selectors,
  generated-name patterns, or a case where ambiguity must be reported instead
  of guessed.
- Example targets:
  `[cFunDef "reduce"; cFor "bi"; cFor "ti"; cArrayWrite "d_partial_sums"]`,
  `[tSpanSeq [cForBody "bi"]]`, `[cMark "kernel_sequence"]`.

## Case-Study Sources

The suite is based on target patterns used across `case_studies/`, including:

- tutorial examples for beginner loop and position targets;
- Harris for body-constrained loops, `any cArrayWrite`, and multi-variable
  targets;
- matmul for nested loop and accumulation targets;
- box blur for function-scoped repeated loops and `occFirst`;
- GPU examples for marks, before/after positions, kernel sequences, and nested
  loop targets;
- OpenCV rowsum for mark-qualified repeated loop and call targets;
- Floyd-Warshall and dot product for occurrence indexes, spans, and read/write
  targets.

## Test Index

Loops:

- `loops/l1_unique_loop`
- `loops/l2_same_loop_name_context`
- `loops/l3_two_different_loops`
- `loops/l3_exact_two_same_name`
- `loops/l4_body_array_write`
- `loops/l4_shared_body_feature`
- `loops/l3_unrelated_loop_set`
- `loops/l3_all_same_name`
- `loops/l2_first_repeated_loop`
- `loops/l5_nested_accumulation_loop`
- `loops/l5_gpu_reduction_nested_write`
- `loops/l5_ambiguous_same_name`

Functions and calls:

- `functions_calls/l1_whole_function`
- `functions_calls/l2_function_body`
- `functions_calls/l2_call_inside_function`
- `functions_calls/l3_repeated_calls`
- `functions_calls/l4_call_arguments`
- `functions_calls/l3_function_decl_and_def`

Variables:

- `variables/l1_single_var_def`
- `variables/l3_multi_var_defs`
- `variables/l2_var_initializer`
- `variables/l4_read_vs_write`
- `variables/l5_generated_name_pattern`

Memory and access:

- `memory_access/l1_array_read`
- `memory_access/l1_array_write`
- `memory_access/l4_field_write`
- `memory_access/l4_cell_write_index`
- `memory_access/l4_generic_write_lhs`

Positions, spans, and marks:

- `positions_spans_marks/l2_before_variable`
- `positions_spans_marks/l2_after_loop`
- `positions_spans_marks/l4_first_in_loop_body`
- `positions_spans_marks/l5_span_around_array_write`
- `positions_spans_marks/l5_sequence_span_loop_body`
- `positions_spans_marks/l5_named_mark`
- `positions_spans_marks/l5_mark_qualified_calls`

Ambiguity and fallback:

- `ambiguity_fallback/l5_line_without_source`
- `ambiguity_fallback/l5_vague_loop`
- `ambiguity_fallback/l4_prefer_semantic_expr`
- `ambiguity_fallback/l4_prefer_argument_constraint`
- `ambiguity_fallback/l5_legitimate_expr_fallback`
