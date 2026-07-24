# Target Generator Smoke Test Notes

This file records the first manual smoke test for
`prompts/01_target_generator.md`.

## Purpose

Step 4 of the OptiNLP plan is to test Prompt 1 on concrete target-generation
examples and refine the prompt before building command-to-script generation.

## Coverage Added

The evaluation set now covers:

- named function targets;
- named loop targets;
- loop targets inside a specific function;
- repeated calls with `nbMulti`;
- ordinal selection with `occIndex`;
- insertion positions with `tBefore`;
- after-loop positions with `tAfter`;
- array writes with `cArrayWrite`;
- loop body constraints such as `cFor "y" ~body:[cArrayWrite "out"]`;
- robust call argument constraints such as
  `cCall "swap" ~args:[[cVar "a"]; [cVar "b"]]`;
- rejection examples for fragile `sExpr` and `sInstr` targets when semantic
  selectors are available;
- multiple named alternatives using `multi`;
- ambiguous repeated loops that should trigger clarification;
- printed OptiLambda used only as readable structure.

## Refinements Made

Prompt 1 now explicitly says:

- convert line references to semantic targets when source code is available;
- use stable names instead of line numbers in final target syntax;
- add enclosing context when the same target name appears in several scopes;
- use occurrence selectors for ordinal requests;
- avoid `sExpr`, `sExprRegexp`, `sInstr`, and `sInstrRegexp` unless semantic
  selectors cannot express the requested location;
- prefer body and argument constraints over exact source text;
- ask for source code when only a line number is given.

## Manual Pass Criteria

A target-generation response passes when it:

- uses existing `Target` constructors only;
- returns the expected target or an equally specific accepted variant;
- asks for clarification for ambiguous cases;
- rejects fragile text or expression targets when a stable semantic target is
  visible in the source;
- does not generate a transformation script;
- does not claim `.opti` text is runnable input.

## Next Prompt Gaps To Watch

- Whether `tBefore` and `tAfter` should be placed before or after the structural
  selector may depend on the transformation. Prompt 2 should learn this from the
  transformation examples rather than forcing one global convention.
- Source line targeting will need a convention for line-numbered snippets. A
  future tool integration can provide AST, trace, or string-representation data
  to reduce ambiguity.
