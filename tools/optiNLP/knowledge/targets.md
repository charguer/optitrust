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

## Core Model

- A target is a `constr list`.
- Constraints are resolved left to right.
- A target may identify exactly one node, several nodes, no nodes, or an
  interstitial position such as before or after an instruction.
- Transformations often expect either one target or explicitly multiple targets.
  Use occurrence constraints when multiplicity matters.

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
```

Examples:

```ocaml
[tBefore; cVarDef "x"]
[tAfter; cCall "init"]
[cFunBody "main"; tFirst]
[cForBody "i"; tBetweenAll]
```

Do not use `tBefore` or `tAfter` unless the operation needs a position, such as
inserting, moving, fissioning, or selecting a boundary.

## Structural Selectors

Common selectors:

```ocaml
cFor "i"
cFor_c "i"
cWhile ()
cIf ()
cFunDef "foo"
cTopFunDef "foo"
cFunBody "foo"
cTopFunBody "foo"
cCall "foo"
cVarDef "x"
cVarsDef "x"
cVar "x"
cReadVar "x"
cWriteVar "x"
cArrayRead "a"
cArrayWrite "a"
cFieldRead ~field:"x" ()
cFieldWrite ~field:"x" ()
cSeq ()
cReturn ()
cLabel "name"
cMark "mark"
```

Nested constraints narrow the match by context:

```ocaml
[cTopFunDef "main"; cCall "foo"]
[cFunBody "main"; cFor "i"]
[cFor "i"; cArrayWrite "A"]
[cIf ~cond:[sExpr "x < n"] (); dThen]
```

Use empty names intentionally only when the repository examples do so and the
target is clearly broad, for example `[cFunDef ""]` or `[cFor ""]`.

## String Selectors

String-based selectors are useful when a more semantic selector is unavailable:

```ocaml
sInstr "x++;"
sExpr "i + 1"
sInstrRegexp "A\\[.*\\]"
sExprRegexp "MINDEX.*"
```

Prefer semantic constructors such as `cFor`, `cCall`, `cVarDef`, `cArrayRead`,
and `cArrayWrite` before falling back to string matching.

## Prompt Policy

The target generator should:

- quote exact identifiers as OCaml strings;
- use current OptiTrust target constructors only;
- prefer semantic targets over line-number-only targets;
- turn line references into structural targets when source code is available;
- ask for clarification when two plausible targets remain;
- mention why a target may match multiple nodes;
- avoid inventing selectors not present in `Target`.

Sources to refresh when syntax changes:

- `lib/framework/target/target.ml`
- `lib/framework/target/constr.ml`
- `doc/target.md`
- `tests/**/**_doc.ml`
