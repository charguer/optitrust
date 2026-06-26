# Alist Monolith And AFL Tests

## 1. Explanation Of The Testing

This folder contains property-based tests for `Alist`.

The goal is to check that the real `Alist` implementation behaves like a simple
reference implementation. The reference implementation is `Naive_alist`, which
uses normal OCaml lists. It is slower, but easier to trust, so it is useful as a
specification of the expected behavior.

The main test file is:

```text
alist_monolith_test.ml
```

It compares:

```text
Naive_alist
```

against:

```text
Alist
```

Monolith generates many random sequences of operations, runs each operation on
both implementations, and checks that the observable results are the same. The
test also calls `Alist.check` on returned `Alist` values, so internal invariant
bugs can be detected even when the final visible result still looks correct.

There are two testing modes:

- Monolith random testing: the normal quick property-based test mode. It is the
  first thing to run.
- AFL fuzzing: the deeper fuzzing mode. It uses AFL to guide test generation
  through coverage feedback. It requires extra setup and can run for a long time.

The Monolith test is not part of the normal OptiTrust build. This is intentional:
CI and regular developers should be able to run:

```sh
dune build
```

without installing Monolith or AFL.

For this reason, `alist_monolith_test.exe` is enabled only with the custom Dune
profile:

```text
monolith
```

Commands that build or run this executable must therefore use:

```sh
--profile monolith
```

## 2. Monolith

### Setup

Install Monolith in your current opam switch:

```sh
opam install monolith
```

Check that the executable builds:

```sh
dune build --profile monolith tests_infra/alist/alist_monolith_test.exe
```

If this command succeeds, Monolith random testing is ready.

### Run

Run the random test with:

```sh
make -C tests_infra/alist random
```

For a short smoke test:

```sh
make -C tests_infra/alist random MAX_SCENARIOS=1 TIMEOUT=1
```

The command-line variables mean:

```text
MAX_SCENARIOS
```

Maximum number of independent Monolith scenarios to generate. A scenario is one
generated test program made of many random operations. Use a small value, such as
`1`, when you only want to check that the test builds and runs.

```text
TIMEOUT
```

Maximum running time, in seconds, for the random test. Use a small value, such as
`1`, for a quick smoke test, and a larger value when you want more random
coverage.

So this command:

```sh
make -C tests_infra/alist random MAX_SCENARIOS=1 TIMEOUT=1
```

means: run the Monolith random test from `tests_infra/alist`, generate at most
one scenario, and stop after about one second. It is useful for a quick local
check before committing.

Equivalent direct Dune command:

```sh
dune exec --profile monolith tests_infra/alist/alist_monolith_test.exe -- --max-scenarios 1 --timeout 1
```

Do not run:

```sh
dune exec alist_monolith_test
```

from the repository root. Dune will not find the executable that way because it
lives under `tests_infra/alist` and is enabled only under the `monolith` profile.

If you see:

```text
Library "monolith" not found
```

then Monolith is not installed in the switch used by the command. Run:

```sh
opam install monolith
```

## 3. AFL

### Setup

AFL fuzzing needs two things:

```text
1. The system AFL tool, usually `afl-fuzz`
2. A special opam switch compiled with AFL instrumentation
```

On Ubuntu/Debian, install AFL with one of:

```sh
sudo apt install afl
```

or:

```sh
sudo apt install afl++
```

Check whether AFL is available:

```sh
afl-fuzz -h
```

Then create the AFL opam switch and install the OCaml dependencies:

```sh
make -C tests_infra/alist setup
```

This creates or reuses the switch:

```text
4.14.2+afl
```

and installs the test dependencies in that switch, including:

```text
monolith
base64
ppx_deriving
ppxlib
sexplib0
```

The separate switch is needed because AFL fuzzing requires OCaml to be compiled
with AFL instrumentation support through `ocaml-option-afl`.

### Run

Start AFL fuzzing with:

```sh
make -C tests_infra/alist test
```

Useful AFL follow-up commands:

```sh
make -C tests_infra/alist summary
make -C tests_infra/alist show
make -C tests_infra/alist min
```

The AFL output directory is generated test data and should not be committed.

If you see `Library "monolith" not found` while running AFL commands, run the
AFL setup command again:

```sh
make -C tests_infra/alist setup
```

If you see:

```text
afl-fuzz: command not found
```

then the system AFL tool is missing. Install `afl` or `afl++` using your system
package manager.

If normal CI fails because of Monolith, check that `alist_monolith_test.exe` is
still gated behind:

```lisp
(enabled_if (= %{profile} monolith))
```

in `tests_infra/alist/dune`.
