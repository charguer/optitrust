# OptiTrust Script Patterns

The AI can only use examples and API details included in the current request.
Do not rely on unstated files or examples.

Most generated scripts should follow the test and case-study style summarized
here:

```ocaml
open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Transformation.name [target];
)
```

Some examples use `open Prelude` instead of or in addition to `open Target`
when helper constructors such as `lit`, `int`, `expr`, `stmt`, or `ty` are
needed. The prompt should include the opens required by the generated code.

## Minimal Script Skeletons

Target-only validation script:

```ocaml
open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Show.target [cFor "i"];
)
```

Single transformation script:

```ocaml
open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop.unroll [cFor "i"];
)
```

Script requiring parsed expressions or statements:

```ocaml
open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->
  !! Sequence_basic.insert ~reparse:true (stmt "a++;") [tBefore; cVarDef "x"];
)
```

Multi-step script:

```ocaml
open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Function.inline [cCall "helper"];
  !! Loop.unroll [cFor "i"];
  !! Cleanup.std ();
)
```

## Common Shapes

Inline a call:

```ocaml
open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Function.inline [cCall "f"];
)
```

Unroll a loop:

```ocaml
open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop.unroll [cFor "i"];
)
```

Tile a loop:

```ocaml
open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.tile (lit "32") ~index:"bi" ~bound:TileDivides [cFor "i"];
)
```

Insert before an instruction:

```ocaml
open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->
  !! Sequence_basic.insert ~reparse:true (stmt "a++;") [tBefore; cVarDef "c"];
)
```

Delete an instruction:

```ocaml
open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Sequence_basic.delete [cVarDef "tmp"];
)
```

Inline a variable definition:

```ocaml
open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Variable.inline [cVarDef "x"];
)
```

Insert a variable:

```ocaml
open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->
  !! Variable.insert ~reparse:true ~typ:(ty "int") ~name:"b" ~value:(lit "2") [tAfter; cVarDef "a"];
)
```

Parallelize a loop with OpenMP:

```ocaml
open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Omp.parallel_for [tBefore; cFor "i"];
)
```

## Generated Script Rules

- Use `!!` for transformations in `Run.script_cpp`.
- Use `!!!` only when an included example or explicit user request requires the
  stronger execution marker; otherwise use `!!`.
- Keep the first generated script minimal.
- Include only transformations described in the prompt, knowledge, or current
  request examples.
- Use the module names and argument order shown in this knowledge.
- Do not generate `Run.script_opti`; OptiLambda parsing is not implemented.
- If the user gives `.opti` text, use it for inspection and target reasoning,
  but generate a C/C++ script workflow.

## Script Checklist

- Start with `open Optitrust`.
- Add `open Target` when using target constructors directly.
- Add `open Prelude` when using helpers such as `lit`, `stmt`, `expr`, or
  parser-style term builders.
- Wrap transformations in `let _ = Run.script_cpp (fun _ -> ... )`.
- Prefix each transformation with `!!`.
- End transformation calls with semicolons inside the script block.
- Include `Cleanup.std ()` only when cleanup is part of the requested or
  recommended transformation plan.
- Use `stmt "..."` for C/C++ statements and declarations.
- Use `expr "..."` or `lit "..."` for expression arguments when shown by the
  transformation shape.
- Use `ty "..."` for type arguments.
- Use target brackets exactly once around a target: `Transformation.x [cFor
  "i"]`, not `Transformation.x [[cFor "i"]]`.
- If a script contains helper definitions, place them before `let _ =
  Run.script_cpp`.

## Validation Guidance

Output validation should be concrete but not pretend it has been run. Good
validation text:

```bash
dune exec -- ./path/to/generated_script.ml
```

When no path is known, say:

```bash
dune exec -- ./<path-to-generated-script>.ml
```

Expected evidence should mention target-resolution success, absence of OptiTrust
exceptions, and visible transformed code properties such as an inlined call or a
removed loop.
