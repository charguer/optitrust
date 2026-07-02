# OptiTrust Target Knowledge

An OptiTrust target is an OCaml list of constraints used to locate one or more
nodes in the current AST. In scripts, targets are usually written with smart
constructors from `Target`, for example:

```ocaml
[cFor "i"]
[nbMulti; cCall "foo"]
[cTopFunDef "main"; cCall "bar"]
[tBefore; cVarDef "x"]
```

The target generator prompt should produce this current script syntax, not older
or paper-only notation.

## Context Boundary

The AI can only use the source code, script fragments, traces, errors, prompt
text, and knowledge text included in the current request. Do not rely on
unstated files or examples.

## Marked Selection Convention

Some extension requests send the whole active file while wrapping the user's
selected focus in markers:

```text
<start>selected source text<end>
```

Treat the marked span as the current focus, and use the rest of the file as
context for robust disambiguation. The markers are artificial annotations, not
program syntax. Never copy `<start>` or `<end>` into a generated target or
script.

## Core Model

- A target is a `constr list`.
- Constraints are resolved left to right.
- A target may identify exactly one node, several nodes, no nodes, or an
  interstitial position such as before or after an instruction.
- Transformations often expect either one target or explicitly multiple targets.
  Use occurrence constraints when multiplicity matters.
- Robust targets should describe semantic AST structure rather than exact source
  text. Prefer named functions, loops, calls, variables, array accesses, fields,
  marks, enclosing context, body constraints, argument constraints, and
  occurrence selectors before string or expression matching.

## Occurrence Constraints

Use these when the same structural pattern can match several nodes:

```ocaml
nbMulti        (* one or more matches *)
nbAny          (* zero or more matches *)
nbExact 2      (* exactly two matches *)
occIndex 0     (* first match, zero-based *)
occIndex 1     (* second match *)
occIndex (-1)  (* last match *)
occFirst
occLast
```

Examples:

```ocaml
[nbMulti; cFor "i"]
[occIndex 1; cFor "i"]
[occFirst; cCall "foo"]
[occLast; cVarDef "tmp"]
```

If the user says "all", "each", or "every", prefer `nbMulti` when at least one
match is expected. If the user says "second", "third", or "last", use an
occurrence selector. If the prompt cannot determine which occurrence is meant,
return alternatives and ask for clarification.

Occurrence constraints are usually placed before the selector they disambiguate:

```ocaml
[occIndex 1; cFor "i"]          (* second loop named i *)
[cTopFunDef "f"; occLast; cCall "foo"]
```

Use `nbMulti` only when the transformation is intended to apply to several
matches. Do not use `nbMulti` to silence ambiguity when the user asked for one
specific node.

## Relative And Sequence Positions

Use relative constraints for insertion, movement, spans, and transformations
that operate at a position rather than directly on a node:

```ocaml
tBefore
tAfter
tFirst
tLast
tBetweenAll
tSpan [START_TARGET] [STOP_TARGET]
tSpanSeq [SEQ_TARGET]
tSpanAround [INSTR_TARGET]
```

Examples:

```ocaml
[tBefore; cVarDef "x"]
[tAfter; cCall "init"]
[cFunBody "main"; tFirst]
[cForBody "i"; tBetweenAll]
[tSpanSeq [cForBody "i"]]
[tSpanAround [cCall "foo"]]
```

Do not use `tBefore` or `tAfter` unless the operation needs a position, such as
inserting, moving, fissioning, or selecting a boundary.

## Structural Selectors

Common selectors:

```ocaml
cFor "i"
cFor_c "i"
cForBody "i"
cFors ["i"; "j"]
cWhile ()
cDoWhile ()
cIf ()
cThen
cFunDef "foo"
cFunDefs ["f"; "g"]
cTopFunDef "foo"
cTopFunDefs ["f"; "g"]
cFunBody "foo"
cTopFunBody "foo"
cFunDefAndDecl "foo"
cTopFunDefAndDecl "foo"
cCall "foo"
cCalls ["foo"; "bar"]
cVarDef "x"
cVarDefs ["x"; "y"]
cVarsDef "x"
cVarInit "x"
cVar "x"
cVarReg "x.*"
cReadVar "x"
cWriteVar "x"
cWrite ()
cRead ()
cReadOrWrite ()
cArrayRead "a"
cArrayWrite "a"
cCellRead ~base:[cVar "a"] ()
cCellWrite ~base:[cVar "a"] ()
cFieldRead ~field:"x" ()
cFieldWrite ~field:"x" ()
cSeq ()
cReturn ()
cBreak
cContinue
cLabel "name"
cGoto ~label:"name" ()
cMark "mark"
cMarkAny
cOmp ()
```

Nested constraints narrow the match by context:

```ocaml
[cTopFunDef "main"; cCall "foo"]
[cFunBody "main"; cFor "i"]
[cFor "i"; cArrayWrite "A"]
[cFor "i" ~body:[cArrayWrite "out"]]
[cCall "foo" ~args:[[cVar "x"]]]
```

Use empty names only when the user clearly wants a broad match, for example
`[cFunDef ""]` for any function definition or `[cFor ""]` for any loop.

## Target Selection Patterns

- Function body: use `[cFunBody "f"]` or `[cTopFunBody "f"]`.
- Whole function definition: use `[cFunDef "f"]` or `[cTopFunDef "f"]`.
- Function declaration plus definition: use `[cFunDefAndDecl "f"]` or
  `[cTopFunDefAndDecl "f"]` when the operation must affect both.
- Named loop: use `[cFor "i"]` when the visible code has only one loop named
  `i`; otherwise add context such as `[cTopFunDef "f"; cFor "i"]` or an
  occurrence selector.
- Loop body: use `[cForBody "i"]` when the transformation targets the contents
  of the loop rather than the loop instruction itself.
- Call inside a function: use `[cTopFunDef "main"; cCall "foo"]`.
- Statement position before/after a declaration or call: use relative position
  first, then the node selector, such as `[tBefore; cVarDef "x"]`.
- Array write in a loop: use `[cFor "i"; cArrayWrite "A"]`.
- Last or second occurrence: use `occLast` or `occIndex 1` before the selector.
- Loop matching by body: if several loops share the same index, narrow with
  body constraints such as `[cFor "y" ~body:[cArrayWrite "out"]]`.
- Call matching by argument: use `cCall "foo" ~args:[[cVar "x"]]` when the same
  function is called with different arguments.
- Read versus write: use `cReadVar "x"` for reads, `cWriteVar "x"` for writes,
  and `cVar "x"` only when either use is acceptable.
- Exact instruction fallback: use `sInstr "..."` only when semantic selectors
  are not enough or the user explicitly references source text.
- Expression fallback: use `sExpr "..."` only when the requested expression has
  no stable semantic selector, such as a specific anonymous condition that
  cannot be identified by function, loop, call, variable, array, field, mark,
  argument, body, or occurrence context.

## Ambiguity And Safety Rules

- If the visible code contains two or more identical matches and the user did
  not specify which one, ask for clarification and show the likely alternatives.
- If the user says "inside f", include the function context.
- If the user says "the loop that writes to A", prefer a loop selector with a
  body constraint, for example `[cFor "i" ~body:[cArrayWrite "A"]]`.
- If the user asks for a position, use `tBefore`, `tAfter`, `tFirst`, `tLast`,
  `tBetweenAll`, or a span target. Do not return only the node target.
- If a target may match multiple nodes, say so explicitly and use `nbMulti` only
  when applying to all matches is intended.
- Do not use line numbers in final targets when stable structural selectors are
  visible in the source.

## String Selectors

String-based selectors are useful when a more semantic selector is unavailable:

```ocaml
sInstr "x++;"
sExpr "i + 1"
sInstrRegexp "A\\[.*\\]"
sExprRegexp "MINDEX.*"
[cIf ~cond:[sExpr "x < n"] (); dThen]
```

Prefer semantic constructors such as `cFor`, `cCall`, `cVarDef`, `cArrayRead`,
`cArrayWrite`, `cReadVar`, `cWriteVar`, body constraints, argument constraints,
and occurrence selectors before falling back to string or expression matching.
String and expression selectors are more fragile because they can break after
formatting changes, equivalent expression rewrites, added temporaries, or small
source edits.

## Full `Target` Constructor Coverage

This section summarizes the important smart constructors exposed by
`lib/framework/target/target.ml`. Generate user-facing targets with these
constructors when they fit. Do not copy implementation helpers or invent
constructors outside this vocabulary.

### Logic, Depth, And Grouping

- `cTrue` matches anything; `cFalse` matches nothing.
- `cStrictNew` matches at depth zero. Use it inside low-level composite
  targets when the same node must satisfy the next constraint exactly.
- `cStrict` matches at depth one. It is useful for direct children.
- `cInDepth` searches at any depth.
- `cInContracts` also searches inside contracts.
- `cTarget [ ... ]` wraps a list of constraints as one constraint.
- `cOr [[...]; [...]]` is a union of alternative targets.
- `cAnd [[...]; [...]]` is an intersection.
- `cDiff [[...]] [[...]]` matches the first target minus the second.
- `any cFor ["i"; "j"]` means any of several named alternatives.
- `multi cFor ["i"; "j"]` means multiple named alternatives and includes
  `nbMulti`.
- `cPath p`, `target_of_path p`, and `target_of_paths ps` are path-based
  helpers. Prefer semantic constructors over paths in generated scripts unless
  the user is working directly with resolved paths.

### Direction Constraints

Direction constraints navigate inside a matched AST node. They are lower-level
than semantic constructors, but useful when the requested location is a part of
a construct:

```ocaml
dRoot
dBefore 0
dAfter 0
dSeqNth 2
dCond
dThen
dElse
dBody
dLetBody
dVarBody
dVarInit
dInit
dForStart
dForStop
dForStep
dForCInit
dForCStep
dName
dType
dArg 0
dLHS
dRHS
```

Use examples:

```ocaml
[cIf (); dCond]       (* condition of an if *)
[cIf (); dThen]       (* then branch *)
[cIf (); dElse]       (* else branch *)
[cCall "f"; dArg 0]   (* first argument of call f *)
[cWrite (); dLHS]     (* left-hand side of a write *)
[cWrite (); dRHS]     (* right-hand side of a write *)
[cFor "i"; dForStop]  (* bound of loop i *)
```

Switch and enum directions exist for specialized cases:

```ocaml
dDirCase 0 (dCaseName 0)
dDirCase 0 dCaseBody
dEnumConst 0 dEnumConstName
dEnumConst 0 dEnumConstVal
```

Prefer higher-level selectors when they express the same idea.

### Type Constraints

Many constructors accept `~typ:"..."` or `~typ_pred:...` to restrict by type.
Use `~typ` only when the type is visible and important to disambiguation.

```ocaml
[cVarDef ~typ:"int" "n"]
[cVar ~typ:"double" "x"]
[cWriteVar ~typ:"int" "i"]
```

Related helpers:

- `cHasType "int"` matches nodes with a printed type.
- `cHasTypeAst ty` and `cHasTypePred pred` are OCaml-level helpers.
- `with_type ~typ target` adds a type constraint to an existing target.
- `cArg "x"` and `cArg ~typ:"int" "x"` match function arguments by name and
  optional type.

### Variable And Definition Targets

Variable definitions can be matched by name, optional regexp/substr matching,
initializer/body, and type:

```ocaml
[cVarDef "x"]
[cVarDef ~body:[cInt 0] "x"]
[cVarDef ~typ:"int" "x"]
[cVarDefReg "tmp.*"]
[cVarDefs ["x"; "y"]]
[cVarsDef "x"]
[cVarInit "x"]
[cDef "x"]
```

Use `cVarsDef` for grouped declarations when the transformation targets a
multi-variable definition group. Use `cVarInit "x"` for the initializer, not the
whole definition.

Variable occurrences:

```ocaml
[cVar "x"]
[cVar ~substr:true "tmp"]
[cVarReg "tmp.*"]
[cReadVar "x"]
[cWriteVar "x"]
```

Use `cReadVar` for reads and `cWriteVar` for writes. Use `cVar` only when read
versus write does not matter.

### Function Targets

Function targets can match names, arguments, return type, body contents, top
level only, and declaration-vs-definition scope:

```ocaml
[cFunDef "f"]
[cFunDefs ["f"; "g"]]
[cFunBody "f"]
[cFunDefAndDecl "f"]
[cTopFunDef "f"]
[cTopFunDefs ["f"; "g"]]
[cTopFunBody "f"]
[cTopFunDefAndDecl "f"]
[cTopFunDefReg "kernel_.*"]
[cTopFunDefAndDeclReg "kernel_.*"]
[cTop "f"]
```

Optional refinements:

```ocaml
[cFunDef ~args:[[cVarDef "n"]] "f"]
[cTopFunDef ~ret_typ:"int" "main"]
[cTopFunDef ~body:[cFor "i"] "f"]
```

Use `cTopFunDef` when the user names a top-level C/C++ function. Use `cFunBody`
or `cTopFunBody` when the transformation targets the statement sequence inside
the function.

### Loop And Branch Targets

Simple OptiTrust loops:

```ocaml
[cFor "i"]
[cFor ~start:[cInt 0] "i"]
[cFor ~stop:[cVar "n"] "i"]
[cFor ~step:[cInt 1] "i"]
[cFor ~body:[cArrayWrite "out"] "i"]
[cFors ["i"; "j"]]
[cForBody "i"]
[cForNestedAtDepth 2]
```

C-style loops:

```ocaml
[cFor_c "i"]
[cFor_c ~cond:[sExpr "i < n"] "i"]
```

Prefer `cFor "i"` for normal OptiTrust simple loops and `cFor_c "i"` when the
source still has a C-style `for (init; cond; step)` shape.

Other control flow:

```ocaml
[cWhile ()]
[cWhile ~cond:[cVar "keep"] ()]
[cDoWhile ()]
[cIf ()]
[cIf ~cond:[cVar "ok"] ()]
[cThen]
[cSwitch ()]
[cSwitch ~cond:[cVar "tag"] ()]
[cReturn ()]
[cReturn ~res:[cVar "x"] ()]
[cBreak]
[cContinue]
[cAbort ()]
```

Use `cIf` with semantic `~cond`, `~then_`, or `~else_` targets when possible.
Use `dThen` or `dElse` after `cIf` when the user asks for a branch position or
branch body.

Switch cases use case descriptors inside `cSwitch`, not as standalone
constraints:

```ocaml
[cSwitch ~cases:[(cCase ~value:[cInt 0] (), [cBreak])] ()]
[cSwitch ~cases:[(cDefault, [cReturn ()])] ()]
```

### Calls, Arguments, And Primitive Operations

Function calls:

```ocaml
[cCall "foo"]
[cCalls ["foo"; "bar"]]
[cCall "foo" ~args:[[cVar "x"]; [cVar "y"]]]
[cCall ~regexp:true "foo_.*"]
[cCall ~fun_:[cVar "fp"] ""]
```

Do not provide both `name` and `~fun_` except with an empty name. Use
`~accept_encoded:true` only for primitive or encoded calls when examples show it.

Argument-list helpers used by call/function constructors:

- `target_list_simpl [[...]; [...]]` means exact argument targets.
- `target_list_one_st target` means at least one item satisfies the target.
- `target_list_all_st target` means all items satisfy the target.
- `target_list_pred_default` means no argument restriction.

Primitive and operator targets:

```ocaml
[cPrim p]
[cPrimCall p]
[cPrimPredCall pred]
[cPrimCallArith ()]
[cBinop Binop_mul]
[cPlusEq ()]
[cDiv ()]
[cMul ()]
[cRef ()]
[cNew ()]
[cDelete ()]
```

These are lower-level. Prefer domain-specific selectors such as `cWrite`,
`cRead`, `cArrayWrite`, `cCall`, or `cVar` unless the user explicitly asks for
an operator or primitive.

### Reads, Writes, Arrays, Cells, And Fields

General reads/writes:

```ocaml
[cWrite ()]
[cWrite ~lhs:[cVar "x"] ()]
[cWrite ~lhs:[cVar "x"] ~rhs:[cInt 0] ()]
[cRead ()]
[cRead ~addr:[cVar "x"] ()]
[cReadOrWrite ()]
```

Array and cell access:

```ocaml
[cAccesses ()]
[cCellAccess ~base:[cVar "a"] ()]
[cCellAccess ~base:[cVar "a"] ~index:[cVar "i"] ()]
[cCellRead ~base:[cVar "a"] ()]
[cCellWrite ~base:[cVar "a"] ()]
[cCellReadOrWrite ~base:[cVar "a"] ()]
[cArrayRead "a"]
[cArrayRead ~index:[cVar "i"] "a"]
[cArrayWrite "a"]
[cArrayWriteAccess "a"]
[cArrayInit]
[cCell ()]
```

`cArrayRead "a"` excludes writes to `a`; `cArrayWrite "a"` matches writes to
cells of `a`. Use `cCellAccess` when the base/index structure matters. Use
`cCell` mainly for array-initialization cells.

Field and struct access:

```ocaml
[cFieldAccess ~field:"next" ()]
[cFieldRead ~field:"next" ()]
[cFieldWrite ~field:"next" ()]
[cFieldReadOrWrite ~field:"next" ()]
```

Access constructors support `~base`, `~field`, `~substr`, and `~regexp`.

### Literals, Types, Enums, Namespaces, And Includes

Use literal selectors only when the literal itself is the requested target or a
needed disambiguator:

```ocaml
[cLit]
[cInt 0]
[cDouble 1.0]
[cBool true]
[cString "hello"]
```

Other declarations:

```ocaml
[cInclude "stdio.h"]
[cTypDef "T"]
[cEnum ~name:"Color" ()]
[cEnum ~constants:[("RED", [cInt 0])] ()]
[cNamespace "ns"]
```

Labels and special OptiTrust helper calls:

```ocaml
[cLabel "done"]
[cGoto ~label:"done" ()]
[cAny]
[cChoose]
[cMindex ()]
[cOmp ()]
```

### Marks And Spans

Marks are stable when the script intentionally placed them:

```ocaml
[cMark "m"]
[cMarks ["m1"; "m2"]]
[cMarkAny]
[cMarkSpan "m"]
[cMarkSpanStart "m"]
[cMarkSpanStop "m"]
```

Use `cMarkSpan "m"` for a span marked by OptiTrust span marks. Use
`cMarkSpanStart` or `cMarkSpanStop` only when the boundary mark itself is the
target.

### Resolver And Transformation Utilities

These functions explain how targets are used by transformations. They are not
usually emitted by the target generator unless the user asks for target-debug or
transformation implementation code.

- `check target` resolves a target for debugging.
- `enable_multi_targets target` adds `nbMulti` if no occurrence constraint is
  already present.
- `filter_constr_occurrence target` removes occurrence constraints.
- `fix_target_multi target` automatically permits multiple matches for logical
  `cOr`/`cAnd` targets when no occurrence constraint is present.
- `resolve_target`, `resolve_target_exactly_one`, `resolve_target_between`,
  `resolve_target_span`, and exact-one variants resolve targets to paths.
- `get_trm_at target` and `get_trm_at_exn target` retrieve the AST node at a
  unique target.
- `iter`, `iteri`, and `foreach` apply code to each resolved path.
- `apply_at_target_paths`, `applyi_at_target_paths`,
  `apply_at_target_paths_before`, and `apply_at_target_paths_in_seq` are used
  by transformations that edit target nodes or positions.
- `reparse_after` wraps transformations that need the modified C/C++ to be
  reparsed after editing.
- String representation helpers compute printed code for `sInstr`, `sExpr`,
  and regexp selectors. This is why string selectors are slower and more
  fragile than semantic constructors.

## Prompt Policy

The target generator should:

- quote exact identifiers as OCaml strings;
- use current OptiTrust target constructors only;
- prefer semantic targets over line-number-only, text-only, or expression-only
  targets;
- turn line references into structural targets when source code is available;
- avoid `sExpr` unless no stable semantic selector is available;
- ask for clarification when two plausible targets remain;
- mention why a target may match multiple nodes;
- avoid inventing selectors not present in `Target`.
