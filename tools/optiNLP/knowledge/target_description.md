# OptiTrust Target Description

OptiTrust transformations operate on program locations. A target describes one
or more of those locations in the current AST. In generated scripts, a target is
written as an OCaml `constr list` using constructors from `Target`.

Examples:

```ocaml
[cFor "i"]
[cTopFunDef "main"; cCall "foo"]
[tBefore; cVarDef "tmp"]
[occIndex 1; cFor "i"]
```

## Purpose

Targets let transformations find the right AST node without depending on fragile
editor details such as screen position or a source line number. A good target is
specific enough to select the intended node, but semantic enough to survive
small code edits.

Use a target to name:

- a whole function, function body, loop, call, variable definition, read, write,
  array access, return, mark, label, or sequence;
- an insertion position before or after a node;
- a span or sequence boundary;
- one occurrence among several similar nodes;
- several intended nodes when a transformation is meant to apply to all of them.

## Syntax

A target is an OCaml list:

```ocaml
[constraint1; constraint2; constraint3]
```

Constraints are resolved from left to right. Earlier constraints narrow the
search context for later constraints.

Examples:

```ocaml
[cTopFunDef "main"; cCall "foo"]
```

This means: find the top-level function definition `main`, then find a call to
`foo` inside it.

```ocaml
[cFor "i"; cArrayWrite "out"]
```

This means: find the loop named `i`, then find a write to array `out` inside the
loop.

## Common Constructors And Meanings

Use these constructors as the default vocabulary for generated targets.

Functions:

```ocaml
[cFunDef "f"]          (* any function definition named f *)
[cTopFunDef "f"]       (* top-level function definition named f *)
[cFunBody "f"]         (* body block of function f *)
[cTopFunBody "f"]      (* body block of top-level function f *)
[cFunDefAndDecl "f"]   (* function declaration and definition named f *)
```

Use a function definition target when the transformation affects the whole
function. Use a function body target when the transformation affects statements
inside the function.

Loops:

```ocaml
[cFor "i"]             (* for loop whose loop index is i *)
[cFor_c "i"]           (* C-style for loop whose loop index is i *)
[cForBody "i"]         (* body of the for loop whose loop index is i *)
[cFors ["i"; "j"]]     (* several named for loops *)
[cWhile ()]            (* while loop *)
[cDoWhile ()]          (* do-while loop *)
```

`cFor "i"` targets the loop instruction itself. `cForBody "i"` targets the
sequence of statements inside the loop. Use the body form for transformations
that operate on the loop contents rather than on the loop node.

Calls:

```ocaml
[cCall "foo"]                  (* call site of function foo *)
[cCalls ["foo"; "bar"]]        (* call sites of foo and bar *)
[cCall "foo" ~args:[[cVar "x"]]]
```

Use argument constraints when the same function is called multiple times with
different arguments.

Variables, reads, and writes:

```ocaml
[cVarDef "x"]          (* variable declaration or definition of x *)
[cVarDefs ["x"; "y"]]  (* definitions of x and y *)
[cVarsDef "x"]         (* variable definition group containing x *)
[cVarInit "x"]         (* initializer of variable x *)
[cVar "x"]             (* any occurrence of variable x *)
[cReadVar "x"]         (* read occurrence of x *)
[cWriteVar "x"]        (* write occurrence of x *)
[cWrite ()]            (* any write instruction or write expression *)
[cRead ()]             (* any read expression *)
[cReadOrWrite ()]      (* any read or write *)
```

Prefer `cReadVar` or `cWriteVar` when the user says read or write. Use `cVar`
only when either kind of occurrence is acceptable.

Arrays, cells, and fields:

```ocaml
[cArrayRead "a"]                 (* read from array a *)
[cArrayWrite "a"]                (* write to array a *)
[cCellRead ~base:[cVar "a"] ()]  (* read from a cell based on a *)
[cCellWrite ~base:[cVar "a"] ()] (* write to a cell based on a *)
[cFieldRead ~field:"x" ()]       (* read of field x *)
[cFieldWrite ~field:"x" ()]      (* write of field x *)
```

Use array or field selectors when the user identifies a location by memory
access, for example "the loop that writes to out".

Control flow, sequences, and markers:

```ocaml
[cIf ()]              (* if statement *)
[cThen]               (* then branch *)
[cSeq ()]             (* sequence/block *)
[cReturn ()]          (* return statement *)
[cBreak]              (* break statement *)
[cContinue]           (* continue statement *)
[cLabel "done"]       (* label named done *)
[cGoto ~label:"done" ()]
[cMark "name"]        (* OptiTrust mark named name *)
[cMarkAny]            (* any OptiTrust mark *)
[cOmp ()]             (* OpenMP directive *)
```

Use marks when the script or source already contains a stable OptiTrust mark.

## Composition Patterns

Add enclosing context before the node selector:

```ocaml
[cTopFunDef "main"; cCall "foo"]
[cFunBody "f"; cFor "i"]
[cFor "i"; cArrayWrite "out"]
```

Add body constraints when the target should be identified by what appears inside
it:

```ocaml
[cFor "y" ~body:[cArrayWrite "out"]]
[cFor "i" ~body:[cCall "work"]]
```

Add argument constraints when a call is best identified by its arguments:

```ocaml
[cCall "swap" ~args:[[cVar "a"]; [cVar "b"]]]
```

Use list constructors for several named nodes:

```ocaml
[cFors ["i"; "j"]]
[cVarDefs ["x"; "y"]]
[cCalls ["init"; "cleanup"]]
```

## Robust Target Style

Prefer semantic selectors because they describe program structure:

```ocaml
[cFor "i"]
[cTopFunDef "f"; cCall "work"]
[cFor "y" ~body:[cArrayWrite "out"]]
[tBefore; cVarDef "c"]
```

Use context when a simple selector may match too much:

```ocaml
[cTopFunDef "main"; cCall "foo"]
[cFunBody "f"; cFor "i"]
```

Use occurrence selectors when the request names an ordinal occurrence:

```ocaml
[occIndex 1; cFor "i"]  (* second loop named i *)
[occLast; cCall "cleanup"]
[occFirst; cVarDef "tmp"]
[cTopFunDef "f"; occIndex 2; cCall "foo"]
```

Use multiple-match constraints only when the user asks for all matches:

```ocaml
[nbMulti; cCall "update"]
[nbExact 2; cFor "i"]
[nbAny; cMark "optional"]
```

Occurrence constraints:

```ocaml
occIndex 0     (* first matching node, zero-based *)
occIndex 1     (* second matching node *)
occIndex (-1)  (* last matching node *)
occFirst       (* first matching node *)
occLast        (* last matching node *)
nbMulti        (* one or more matches, for intentional all-match targets *)
nbAny          (* zero or more matches *)
nbExact 2      (* exactly two matches *)
```

## Position Targets

Use `tBefore`, `tAfter`, `tFirst`, `tLast`, `tBetweenAll`, or span targets only
when the requested operation needs a position, such as insertion, movement, or a
boundary.

Examples:

```ocaml
[tBefore; cVarDef "x"]
[tAfter; cFor "i"]
[cFunBody "main"; tFirst]
[cForBody "i"; tBetweenAll]
```

Do not return only `[cVarDef "x"]` when the user asks for the position before
the declaration of `x`.

## Fragile Fallbacks

String and expression selectors are fallback tools:

```ocaml
sInstr "x++;"
sExpr "i + 1"
sInstrRegexp "A\\[.*\\]"
sExprRegexp "MINDEX.*"
```

They can break when whitespace, formatting, parentheses, temporary variables, or
minor expression rewrites change. Prefer `cFor`, `cCall`, `cVarDef`,
`cArrayRead`, `cArrayWrite`, `cReadVar`, `cWriteVar`, body constraints, argument
constraints, and occurrence selectors whenever those can identify the target.

Use `sExpr` only when no available semantic selector can express the requested
location, such as targeting a specific anonymous condition or expression that
has no stable name, call, variable, array, field, mark, or structural context.

## Validation

Target-only answers should include a small validation script using `Show.target`
when possible:

```ocaml
open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Show.target [cFor "i"];
)
```

If a target may match several nodes, the answer must say so and either use an
occurrence selector for one node or `nbMulti` for an intentional multi-node
target.
