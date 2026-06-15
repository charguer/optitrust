# Benchmarking Guide

This folder contains the permanent benchmark helpers added for comparing the
old list-backed `Mlist` with the new `Alist`-backed `Mlist`.

Generated benchmark data is written under:

```text
benchmark/runs/<run-id>/
```

The scripts refuse to append to an existing run id by default. Use a new run id
or clean generated results first.

## Cleanup

Remove generated benchmark outputs:

```bash
./benchmark/cleanup
```

This removes generated paths such as:

```text
benchmark/runs/
benchmark/report.md
```

It preserves benchmark helper scripts and this README.

## Mlist Microbenchmarks

Run direct `Mlist_old` vs new `Mlist` operation benchmarks and generate plots:

```bash
./benchmark/run_mlist_bench mlist-run
```

Outputs:

```text
benchmark/runs/mlist-run/micro/mlist_microbench.csv
benchmark/runs/mlist-run/micro/mlist_microbench.log
benchmark/runs/mlist-run/summary/mlist_microbench_summary.csv
benchmark/runs/mlist-run/plots/
benchmark/runs/mlist-run/plots/mlist_cases/
```

The microbenchmark compares both implementations inside the same executable:

```text
old-mlist        -> lib/ast/mlist_old.ml
new-alist-mlist -> lib/ast/mlist.ml
```

Current benchmark tiers:

```text
normal sizes:      16, 32, 64, 1000, 10000, 100000
normal repetitions: 10

large sizes:       1000000
large repetitions: 3
large operations: selected lower-risk operations only
```

The summary uses:

```text
speedup_percent_new_vs_old = ((old_mean_ms - new_mean_ms) / old_mean_ms) * 100
```

Positive speedup means the new implementation is faster.

Regenerate plots from an existing CSV:

```bash
dune exec tests_infra/mlist/mlist_plot.exe -- \
  benchmark/runs/mlist-run/micro/mlist_microbench.csv
```

## Tester Timing

Run real OptiTrust tester workloads with timing enabled:

```bash
./benchmark/run_tester_bench tester-run tests/sequence
```

If no target is provided, this runs the tester default, equivalent to:

```bash
./tester run
```

which reads `all.tests`.

Defaults:

```text
WARMUPS=1
REPETITIONS=10
IMPLEMENTATION=new-alist-mlist
```

Override them with environment variables:

```bash
REPETITIONS=5 WARMUPS=2 IMPLEMENTATION=old-mlist \
  ./benchmark/run_tester_bench old-sequence tests/sequence
```

The runner:

```text
1. pre-builds tester and plotter
2. performs warmup run(s), not logged
3. performs measured runs, logged as iteration=1..N
4. generates tester plots
```

Outputs:

```text
benchmark/runs/tester-run/tester/per_test_exec_timing.csv
benchmark/runs/tester-run/tester/per_test_timing.csv
benchmark/runs/tester-run/tester/tester_summary.csv
benchmark/runs/tester-run/tester/tester_summary.json
benchmark/runs/tester-run/tester/tester.log
benchmark/runs/tester-run/summary/tester_timing_summary.csv
benchmark/runs/tester-run/plots/tester_tests.svg
```

`per_test_exec_timing.csv` is the raw timing recorded inside
`Batching.run_test`.

`per_test_timing.csv` joins that timing with final tester status:

```text
success
wrong
failed
missing_exp
ignored
```

## Old vs New Tester Comparison

To compare real tester workloads with new `Mlist` and old `Mlist`, use:

```bash
REPETITIONS=10 WARMUPS=1 \
  ./benchmark/run_tester_mlist_compare make-tests
```

With a focused group:

```bash
REPETITIONS=10 WARMUPS=1 \
  ./benchmark/run_tester_mlist_compare sequence tests/sequence
```

This script does **not** modify the active checkout. It creates two isolated
copies under `/tmp`:

```text
/tmp/optitrust-mlist-compare-<run-id>-<pid>/new
/tmp/optitrust-mlist-compare-<run-id>-<pid>/old
```

The new copy uses the current `lib/ast/mlist.ml`.

The old copy installs:

```text
lib/ast/mlist_old.ml -> lib/ast/mlist.ml
```

inside the temporary copy only. It also applies a small temporary compatibility
patch because the old implementation exposes `empty` as a value while the
current code expects `empty ()`.

Outputs:

```text
benchmark/runs/<run-id>-new/
benchmark/runs/<run-id>-old/
benchmark/runs/<run-id>-compare/
```

Main comparison files:

```text
benchmark/runs/<run-id>-compare/summary/tester_timing_summary.csv
benchmark/runs/<run-id>-compare/plots/tester_tests.svg
```

Keep temporary copies for debugging:

```bash
KEEP_TMP=1 ./benchmark/run_tester_mlist_compare debug-run tests/sequence
```

## Plot Existing Tester Runs

Compare two existing tester timing CSVs:

```bash
./benchmark/plot_tester_compare sequence-compare \
  benchmark/runs/sequence-old/tester/per_test_timing.csv \
  benchmark/runs/sequence-new/tester/per_test_timing.csv
```

This writes:

```text
benchmark/runs/sequence-compare/summary/tester_timing_summary.csv
benchmark/runs/sequence-compare/plots/tester_tests.svg
```

## Important Notes

- Do not reuse run ids unless you intentionally want to append:

```bash
ALLOW_EXISTING_RUN=1 ./benchmark/run_mlist_bench existing-run
```

- Prefer fresh run ids for serious measurements.

- For full `make tests` equivalence, run tester comparison without a target:

```bash
./benchmark/run_tester_mlist_compare make-tests
```

- `make tests` is defined in the root `Makefile` as:

```bash
./tester run
```

- Direct `Mlist` microbenchmarks can show large operation-level wins even when
  full tester workloads show little total improvement. The tester includes
  parsing, transformations, trace work, printing, output comparison, GC, and
  other non-`Mlist` costs.
