# Transpose benchmark (naive vs. bank-conflict-free)

A small, reproducible harness to compare a naive GPU matrix-transpose kernel
against a tiled, bank-conflict-free one across representative input sizes.

Target: **CUDA 13.5**, **Blackwell** (default device: RTX PRO 6000 Blackwell, `sm_120`).

## Build & run

    make                    # RTX PRO 6000 Blackwell -> sm_120 (default)
    make run                # full sweep -> results/
    make analyze            # -> results/summary.csv, verdict.txt, boxplot.*

Quick smoke test: `make check`.

Blackwell arch reference (CUDA 13.5):

    sm_100  datacenter           (B100 / B200 / GB200)
    sm_120  consumer/workstation (RTX 50 / GB202 / RTX PRO 6000 Blackwell)
    sm_121  GB10                 (DGX Spark / Jetson Thor)

Set `CUDA_HOME` if your install is not at `/usr/local/cuda-13.2`
(e.g. `make CUDA_HOME=/usr/local/cuda`).

## Protocol

For each size (H x W) and each random draw:

1. Generate a random input A (seeded, reproducible).
2. Run the CPU reference and each GPU kernel on the same A; verify equality
   (max abs diff <= --tol). A mismatch fails the run (non-zero exit).
3. Run `--warmup` untimed iterations.
4. Run `--iters` timed iterations (CUDA events). By default the L2 cache is
   flushed before each timed sample so every run is cold-cache (no warm-cache
   bias). Disable with --no-l2flush.

## Output

* results/samples.csv       raw per-sample time_us / gbps
* results/correctness.csv   per-config verification result
* results/meta.csv          device + run metadata
* results/summary.csv, verdict.txt, boxplot.*  (from analyze.py)

The verdict compares medians per shape using a bootstrap CI on the median
ratio and (if scipy is present) the Mann-Whitney U test.

## Methodology notes

* **Why many samples / quantiles:** single-run timing is dominated by
  scheduler and clock noise. We report the median and quartiles (not the
  mean) and use bootstrap CIs so the conclusion is robust to outliers.
* **L2 flush:** without it, small matrices stay resident in L2 across
  samples, inflating apparent bandwidth. The flush makes all sizes
  memory-system-bound and comparable. Blackwell parts have larger L2, so
  the flush buffer is sized from the device's reported L2 automatically.
* **Clocks:** for tighter runs, lock the GPU clock
  (`nvidia-smi -pm 1; nvidia-smi -lgc <mhz>`) so boost variance is removed.
* **Correctness first:** timing is only meaningful on a verified-correct
  kernel; verification is per (size, draw, impl).

## Suggested shapes (the default set)

Square — sweep the latency -> memory-bandwidth regime:

    512, 1024, 2048, 4096, 8192, 16384   (NxN)

Non-square — aspect ratios that stress boundary/padding & coalescing:

    256x2048, 2048x256, 1024x4096, 4096x1024, 256x8192, 8192x256

Small sizes are occupancy/latency dominated (tile kernel usually wins);
large sizes saturate memory bandwidth (both converge). The spread across
the set is what the "similar vs. different" test is sensitive to.
