# Target Generator Evaluation Cases

Use these cases to manually test `prompts/01_target_generator.md`.

Each case gives a user request, a source snippet when needed, and the expected
target output. A prompt run passes when it returns the expected target or an
equally specific accepted variant, and when it asks for clarification in the
cases marked ambiguous.

## Case 1: Function Definition

Request:

```text
target the function main
```

Expected target:

```ocaml
[cFunDef "main"]
```

Acceptable narrower top-level variant:

```ocaml
[cTopFunDef "main"]
```

## Case 2: Loop By Index

Source:

```c
void kernel(int n) {
  for (int i = 0; i < n; i++) {
    work(i);
  }
}
```

Request:

```text
target the loop i
```

Expected target:

```ocaml
[cFor "i"]
```

## Case 3: Loop Inside Function

Source:

```c
void init(int n) {
  for (int i = 0; i < n; i++) clear(i);
}

void main_loop(int n) {
  for (int i = 0; i < n; i++) update(i);
}
```

Request:

```text
target the loop i inside function main_loop
```

Expected target:

```ocaml
[cFunBody "main_loop"; cFor "i"]
```

## Case 4: Multiple Calls

Source:

```c
void step() {
  update(0);
  update(1);
}
```

Request:

```text
target every call to update
```

Expected target:

```ocaml
[nbMulti; cCall "update"]
```

## Case 5: Occurrence Selection

Source:

```c
void two_loops(int n) {
  for (int i = 0; i < n; i++) a(i);
  for (int i = 0; i < n; i++) b(i);
}
```

Request:

```text
target the second loop named i
```

Expected target:

```ocaml
[occIndex 1; cFor "i"]
```

## Case 6: Insertion Position

Source:

```c
void f() {
  int a = 0;
  int c = 1;
}
```

Request:

```text
target the position before variable c is declared
```

Expected target:

```ocaml
[tBefore; cVarDef "c"]
```

## Case 7: Array Write

Source:

```c
void fill(int n, int* A) {
  for (int i = 0; i < n; i++) {
    A[i] = i;
  }
}
```

Request:

```text
target writes to A
```

Expected target:

```ocaml
[nbMulti; cArrayWrite "A"]
```

## Case 8: Ambiguous Loop

Request:

```text
target the loop on line 10
```

Expected behavior:

- If source code with line numbers is available, map line 10 to a semantic
  target such as `[cFor "i"]` or `[occIndex 1; cFor "i"]`.
- If source code is not available, ask for the code or the loop index/name.

## Case 9: Call Inside Function

Source:

```c
void helper() {
  foo();
}

void main() {
  foo();
}
```

Request:

```text
target the call to foo inside main
```

Expected target:

```ocaml
[cTopFunDef "main"; cCall "foo"]
```

Accepted variant:

```ocaml
[cFunBody "main"; cCall "foo"]
```

## Case 10: Position After Loop

Source:

```c
void f(int n) {
  for (int i = 0; i < n; i++) {
    work(i);
  }
  finish();
}
```

Request:

```text
target the position after the loop i
```

Expected target:

```ocaml
[cFor "i"; tAfter]
```

Accepted variant:

```ocaml
[tAfter; cFor "i"]
```

## Case 11: Loop With Array Write In Body

Source:

```c
void harris(int n, int* out, int* tmp) {
  for (int y = 0; y < n; y++) {
    tmp[y] = y;
  }
  for (int y = 0; y < n; y++) {
    out[y] = tmp[y];
  }
}
```

Request:

```text
target the y loop that writes to out
```

Expected target:

```ocaml
[cFor "y" ~body:[cArrayWrite "out"]]
```

## Case 12: Multiple Named Variables

Source:

```c
void f() {
  int gray = 0;
  int ix = 0;
  int iy = 0;
}
```

Request:

```text
target the variable definitions gray, ix, and iy
```

Expected target:

```ocaml
[multi cVarDef ["gray"; "ix"; "iy"]]
```

## Case 13: Ambiguous Named Loop Without Context

Source:

```c
void a(int n) {
  for (int i = 0; i < n; i++) work_a(i);
}

void b(int n) {
  for (int i = 0; i < n; i++) work_b(i);
}
```

Request:

```text
target the loop i
```

Expected behavior:

- Do not claim a unique target.
- Ask which enclosing function is intended.
- Good alternatives to show:

```ocaml
[cFunBody "a"; cFor "i"]
[cFunBody "b"; cFor "i"]
```

## Case 14: OptiLambda Printed Loop

Source:

```optilambda
fun main(n: int): int {
  for<seq> i in 0..n {
    x = x + i;
  }
  x
}
```

Request:

```text
target the OptiLambda loop over i
```

Expected target:

```ocaml
[cFor "i"]
```

Expected note:

- The `.opti` text is used only for target reasoning.
- Do not generate `Run.script_opti`.
