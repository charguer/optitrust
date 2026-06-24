# Code To Candidate Script Evaluation Cases

Use these cases to manually test `prompts/03_code_to_candidate_script.md`.

## Case 1: Simple Loop

Input:

```c
void f(int n) {
  for (int i = 0; i < n; i++) {
    work(i);
  }
}
```

Acceptable suggestions:

- `Loop.unroll [cFor "i"]` as a candidate only if the goal is reducing loop
  overhead or exposing straight-line code.
- `Omp.parallel_for [cFor "i"]` only if independence is plausible and validation
  is required.

Required behavior:

- Rank confidence.
- State that independence is not proven from the snippet alone.

## Case 2: Function Call In Hot Loop

Input:

```c
void f(int n) {
  for (int i = 0; i < n; i++) {
    y[i] = helper(x[i]);
  }
}
```

Acceptable suggestion:

```ocaml
open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Function.inline [cFor "i"; cCall "helper"];
)
```

Required behavior:

- Explain that inlining may expose further simplifications.
- Require validation with diff/trace or tests.

## Case 3: Adjacent Loops

Input:

```c
void f(int n) {
  for (int i = 0; i < n; i++) A[i] = i;
  for (int i = 0; i < n; i++) B[i] = A[i] + 1;
}
```

Acceptable suggestion:

- Candidate loop fusion targeting the repeated `i` loops.

Required behavior:

- Mark as medium confidence.
- Mention dependency and resource checks.
- Do not claim semantic safety without validation.

## Case 4: Printed OptiLambda

Input:

```optilambda
fun main(n: int): int {
  for<seq> i in 0..n {
    x = x + i;
  }
  x
}
```

Required behavior:

- Use the printed loop to reason about targets such as `[cFor "i"]`.
- Do not generate `Run.script_opti`.
- If a script is proposed, state that it must be applied through an existing
  C/C++ script workflow until parser support exists.
