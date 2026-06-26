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
[cIf ~cond:[sExpr "x < n"] (); dThen]
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
