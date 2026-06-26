# OptiLambda Context For OptiNLP

OptiLambda is the textual language used to display OptiTrust internal AST terms
without going through the C/C++ printer. It is useful for traces, diffs, target
reasoning, and future internal-language workflows.

The AI can only use OptiLambda examples, syntax notes, traces, and source text
included in the current request. Do not rely on unstated files or examples.

Current status:

- OptiLambda is implemented as a printer over `Ast.trm`.
- A parser is planned, but not implemented.
- The framework can print three synchronized representations: `surface`,
  `internal`, and `typed`.
- All three representations describe the same AST; switching representation
  changes the printed view, not the transformation step.

Prompt implications:

- The AI may inspect printed `.opti` code to understand functions, loops,
  assignments, calls, marks, and contracts.
- The AI must not claim that `.opti` can currently be used as runnable input.
- The AI must not generate `Run.script_opti`.
- For now, generated runnable scripts should use `Run.script_cpp`.

Visible OptiLambda cues:

- `fun name(args): type { ... }` describes a function.
- `for<seq> i in 0..n { ... }` describes a sequential loop over `i`.
- Assignments, reads, writes, marks, and contract-like annotations can be used
  for target reasoning.
- Printed `.opti` text is inspection evidence only; runnable transformation
  scripts still target the C/C++ workflow.
