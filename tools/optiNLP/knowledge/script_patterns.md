# OptiTrust Script Patterns

Most generated scripts should follow the existing test and case-study style:

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

## Generated Script Rules

- Use `!!` for transformations in `Run.script_cpp`.
- Keep the first generated script minimal.
- Include only transformations that exist in `lib/transfo/`.
- Prefer examples from `tests/**/**_doc.ml` when choosing module names and
  argument order.
- Do not generate `Run.script_opti`; OptiLambda parsing is not implemented.
- If the user gives `.opti` text, use it for inspection and target reasoning,
  but generate a C/C++ script workflow unless the repository later adds a real
  OptiLambda parser path.
