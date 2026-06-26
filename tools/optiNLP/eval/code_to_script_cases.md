# Code To Full Script Evaluation Cases

Use these cases to manually test `prompts/03_code_to_full_script.md`.

## Case 1: Simple Loop

Input:

```c
void f(int n) {
  for (int i = 0; i < n; i++) {
    work(i);
  }
}
```

Acceptable full-script behavior:

- Emit a complete OCaml script with `open Optitrust`, target-related opens, and
  `Run.script_cpp`.
- Include at least one conservative candidate transformation in the table.
- The `Full Transformation Script` section must contain a complete `.ml` script, not just
  `[cFor "i"]`.

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
- Emit the complete script in the `Full Transformation Script` section.

## Case 3: Adjacent Loops

Input:

```c
void f(int n) {
  for (int i = 0; i < n; i++) A[i] = i;
  for (int i = 0; i < n; i++) B[i] = A[i] + 1;
}
```

Acceptable full-script behavior:

- Candidate loop fusion targeting the repeated `i` loops.

Required behavior:

- Mark as medium confidence.
- Mention dependency and resource checks.
- Do not claim semantic safety without validation.
- Emit the complete script in the `Full Transformation Script` section.

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

## Case 5: Matrix Multiplication Full File

Input:

```c
void mm(int n, double* A, double* B, double* C) {
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      double sum = 0.;
      for (int k = 0; k < n; k++) {
        sum += A[i*n+k] * B[k*n+j];
      }
      C[i*n+j] = sum;
    }
  }
}
```

Request:

```text
generate a complete transformation script for the whole file
```

Required behavior:

- Treat this as Prompt 3 / full-script generation, not Prompt 2.
- Emit a complete OCaml script in the style of `matmul.ml`.
- Include `open Optitrust` and the needed target/prelude opens.
- Use `Run.script_cpp`.
- Prefer a coherent matrix-multiplication strategy such as tiling `i`, `j`, and
  `k`, loop reordering, optional SIMD/parallelism when targets are clear, and
  `Cleanup.std ()`.
- Rank transformations and state which assumptions need validation.
