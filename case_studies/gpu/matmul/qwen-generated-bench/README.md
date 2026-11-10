# Matrix multiplication benchmark (OptiTrust vs. reference kernels)

A small, reproducible harness to compare multiple GPU matrix-multiplication
kernels across representative input sizes, using paired timing and
infinity-norm correctness verification.

Target: **CUDA 13.5**, **Blackwell** (RTX PRO 6000, `sm_120`).

## Build & run

    make                    # build (sm_120 default)
    make run                # full sweep -> results/
    make analyze            # -> results/speedup.csv, verdict.txt, figures

Quick smoke test: `make check`.

Set `CUDA_HOME` if your install is not at `/usr/local/cuda-13.2`
(e.g. `make CUDA_HOME=/usr/local/cuda`).

## Kernels under study

To add a new kernel:
1. Write your `__global__` function in `kernels.cuh`.
2. Add one entry to `allKernels()` in `benchmark.cu`.
3. Select it with `--impl mykernel` or include in `--impl all`.

## Protocol

For each shape (m×n×p) and each random draw:

1. Generate random inputs A(m×p) and B(p×n), uniform [−1, 1], seeded.
2. Compute CPU reference C = A·B in **double precision** (correctness only,
   not timed).
3. For each registered GPU kernel:
   - Run once, read back C, verify against CPU reference using
     **infinity-norm relative error**: `max|C_gpu − C_ref| / max|C_ref|`.
     Mismatch (error > `--tol`, default 1e−2) → non-zero exit.
   - Run `--warmup` untimed iterations.
   - Run `--iters` timed iterations (CUDA events, L2 flush between samples).
   - Record per-sample time (µs) and TFLOPs to CSV.

## Suggested sizes (default set)

Square — sweep from latency-bound to compute-bound:

    128×128×128
    256×256×256
    512×512×512
    1024×1024×1024
    2048×2048×2048
    4096×4096×4096
    8192×8192×8192

Rectangular / tall-skinny — stress coalescing, occupancy, tile-boundary
effects:

    1024×2048×512
    512×2048×1024
    2048×1024×512
    4096×512×4096
    512×4096×4096

All m, n, p must be multiples of 32 (required by the tiled kernels).
Small sizes are latency/occupancy-dominated; large sizes saturate the
compute units or memory bandwidth (depending on the kernel's arithmetic
intensity).

## Output

* `results/samples.csv`       raw per-sample time_us / tflops
* `results/correctness.csv`   per-config infinity-norm relative error
* `results/meta.csv`          device + run metadata
* `results/summary.csv`       per-shape quantile stats + median TFLOPs
* `results/speedup.csv`       per-shape paired speedup vs. baseline
* `results/verdict.txt`       geomean speedup + per-shape table
* `results/speedup.{pdf,png}` speedup vs. log10(m·n·p) with CIs
* `results/tflops.{pdf,png}`  TFLOPs vs. log10(m·n·p) per kernel

## Methodology notes

* **Paired timing:** all kernels run the same number of iterations in the
  same order per (draw, iteration), so `analyze.py` can pair samples by
  (draw, sample) for matched-ratio speedup. This removes input-to-input and
  trial-to-trial variance from the comparison.
* **L2 flush:** without it, small matrices (≤ ~128 MB on Blackwell) stay
  resident in L2 across samples, inflating apparent bandwidth. The flush
  makes all sizes DRAM-bound and comparable.
* **Infinity-norm correctness:** standard BLAS test metric. Per-element
  relative error is misleading for GEMM because many C entries are near
  zero (cancellation). The infinity norm `max|diff|/max|ref|` is the
  accepted "correct" threshold: 1% for float32 GEMM with p ≤ 1024.
* **Clocks:** for tighter runs, lock the GPU clock
  (`nvidia-smi -pm 1; nvidia-smi -lgc <mhz>`) so boost variance is removed.
* **TFLOPs:** computed as `2·m·n·p / time`. This is the standard metric
  for GEMM throughput.
