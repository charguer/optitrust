# Alist Monolith Tests

This folder contains property-based tests for `Alist`.

The test compares the real `Alist` implementation against `Naive_alist`, a
simple list-backed reference implementation. Monolith generates operation
sequences and checks that both implementations have the same observable
behavior. The candidate wrapper also runs `Alist.check` on returned structures
so invariant failures are caught during testing.

## Why A Separate Dune Profile?

The Monolith package is a testing/fuzzing dependency, not a normal OptiTrust
build dependency. CI runs:

```sh
dune build
```

and should not fail just because Monolith is not installed.

For this reason, `alist_monolith_test.exe` is enabled only when Dune is run with
the `monolith` profile:

```sh
dune build --profile monolith tests_infra/alist/alist_monolith_test.exe
```

Normal project builds skip this executable, while developers who want to run the
Alist tests can opt in explicitly.

## Random Monolith Testing

Run the random test with:

```sh
make -C tests_infra/alist random
```

For a short smoke test:

```sh
make -C tests_infra/alist random MAX_SCENARIOS=1 TIMEOUT=1
```

Equivalent direct Dune command:

```sh
dune exec --profile monolith tests_infra/alist/alist_monolith_test.exe -- --max-scenarios 1 --timeout 1
```

## AFL Fuzzing

Set up the AFL switch and dependencies:

```sh
make -C tests_infra/alist setup
```

Run AFL fuzzing:

```sh
make -C tests_infra/alist test
```

Useful follow-up commands:

```sh
make -C tests_infra/alist summary
make -C tests_infra/alist show
make -C tests_infra/alist min
```

The AFL output directory is generated test data and should not be committed.
