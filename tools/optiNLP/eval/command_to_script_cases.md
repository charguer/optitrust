# Command To Script Evaluation Cases

Use these cases to manually test `prompts/02_command_to_script.md`.

## Case 1: Unroll Loop

Request:

```text
unroll the loop i
```

Expected script:

```ocaml
open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop.unroll [cFor "i"];
)
```

## Case 2: Inline Function Call

Request:

```text
inline calls to f
```

Expected script:

```ocaml
open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Function.inline [cCall "f"];
)
```

## Case 3: Inline Call In Function

Request:

```text
inline the call to g inside main
```

Expected script:

```ocaml
open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Function.inline [cTopFunDef "main"; cCall "g"];
)
```

## Case 4: Insert Statement Before Variable

Request:

```text
insert a++; before variable c
```

Expected script:

```ocaml
open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->
  !! Sequence_basic.insert ~reparse:true (stmt "a++;") [tBefore; cVarDef "c"];
)
```

## Case 5: Missing Tile Size

Request:

```text
tile loop i
```

Expected behavior:

- Ask for the tile size and desired tile index name, unless provided elsewhere.
- Do not invent a tile size.

## Case 6: Unsupported `.opti` Execution

Request:

```text
run this transformation directly on the .opti file
```

Expected behavior:

- Explain that OptiLambda is currently printer-oriented and no `Run.script_opti`
  workflow should be generated.
- Ask for the C/C++ source or an existing supported script context.
