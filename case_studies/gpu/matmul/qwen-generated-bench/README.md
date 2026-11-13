# Matrix Multiplication Benchmark

A reproducible harness to compare multiple GPU matrix-multiplication kernels across representative input sizes, using paired timing and infinity-norm correctness verification.

Target: **13.x**, **Blackwell** (RTX PRO 6000, `sm_120`).

## Build & Run

    make                    # build (sm_120 default)
    make run                # full sweep -> results/
    make analyze            # -> results/speedup.csv, verdict.txt, figures

Quick smoke test: `make check`.

Set `CUDA_HOME` if your install is not at `/usr/local/cuda`.

## Protocol

For each shape (m×n×p) and each random draw:

1. Generate random inputs A(m×p) and B(p×n), uniform [−1, 1], seeded.
2. Compute CPU reference C = A·B in **double precision** (correctness only, not timed).
3. For each registered GPU kernel:
   - Run once, read back C, verify against CPU reference using **infinity-norm relative error**: `max|C_gpu − C_ref| / max|C_ref|`.
   - Run `--warmup` untimed iterations.
   - Run `--iters` timed iterations (CUDA events, L2 flush between samples).
   - Record per-sample time (µs) and TFLOPs to CSV.

## Default Sizes

Square — sweep from latency-bound to compute-bound:

    512×512×512
    1024×1024×1024
    2048×2048×2048
    4096×4096×4096

Rectangular / tall-skinny — stress coalescing, occupancy, tile-boundary effects:

    1024×2048×512
    512×2048×1024
    2048×1024×512
    4096×512×4096
    512×4096×4096

All m, n, p must be multiples of 32 (required by the tiled kernels).

## Output

* `results/samples.csv`       raw per-sample time_us / tflops
* `results/correctness.csv`   per-config infinity-norm relative error
* `results/meta.csv`          device + run metadata
* `results/summary.csv`       per-shape quantile stats + median TFLOPs
* `results/speedup.csv`       per-shape paired speedup vs. baseline
* `results/verdict.txt`       geomean speedup + per-shape table
* `results/speedup.{pdf,png}` speedup vs. shape with CIs
* `results/tflops.{pdf,png}`  TFLOPs vs. shape per kernel
* `results/runtime.{pdf,png}` runtime vs. shape per kernel
* `results/focused_*.pdf`     focused comparison plots

## Methodology Notes

* **Paired Timing:** All kernels run the same number of iterations in the same order per (draw, iteration). This allows `analyze.py` to pair samples by (draw, sample) for matched-ratio speedup, removing input-to-input variance.
* **L2 Flush:** The L2 cache is flushed before each timed sample to ensure all runs are cold-cache and DRAM-bound, making small and large sizes comparable.
* **Correctness:** Uses infinity-norm relative error. A tolerance of 1e-2 (1%) is used for float32 GEMM, which is a standard threshold for this precision.
* **Clocks:** For tighter runs, lock the GPU clock (`nvidia-smi -pm 1; nvidia-smi -lgc <mhz>`) so boost variance is removed.
