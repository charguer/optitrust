# OptiLambda Context For OptiNLP

OptiLambda is the textual language used to display OptiTrust internal AST terms
without going through the C/C++ printer. It is useful for traces, diffs, target
reasoning, and future internal-language workflows.

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
- The AI must not generate `Run.script_opti` unless the repository later adds
  that API.
- For now, generated runnable scripts should use `Run.script_cpp`.

Important source:

- `lib/optilambda/optilambda_syntax.md`
- `lib/optilambda/optilambda_style.ml`
- `lib/optilambda/optilambda_printer.ml`
- `tests_infra/optilambda/`
