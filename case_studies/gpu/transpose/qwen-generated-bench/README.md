# Transpose Benchmark

A reproducible harness to compare multiple GPU matrix-transpose kernels (naive, tiled, bank-conflict-free) across representative input sizes.

Target: **CUDA 13.x**, **Blackwell** (RTX PRO 6000, `sm_120`).

## Build & Run

    make                    # build (sm_120 default)
    make run                # full sweep -> results/
    make analyze            # -> results/speedup.csv, verdict.txt, figures

Quick smoke test: `make check`.

Set `CUDA_HOME` if your install is not at `/usr/local/cuda`.

## Protocol

For each size (H x W) and each random draw:

1. Generate a random input A (H×W floats, uniform [−1, 1], seeded).
2. Compute the CPU reference transpose.
3. For each kernel:
   - Run once, read back the result, compare to CPU reference using **max absolute difference**.
   - Run `--warmup` untimed iterations.
   - Run `--iters` timed iterations (CUDA events, L2 flush between samples).
   - Record per-sample time (µs) and effective bandwidth (GB/s) to CSV.

## Default Sizes

Square — sweep the latency -> memory-bandwidth regime:

    512×512
    1024×1024
    2048×2048
    4096×4096

Non-square — aspect ratios that stress boundary/padding & coalescing:

    256×2048
    2048×256
    1024×4096
    4096×1024

Small sizes are occupancy/latency dominated; large sizes saturate memory bandwidth.

## Output

* `results/samples.csv`       raw per-sample time_us / gbps
* `results/correctness.csv`   per-config max absolute difference
* `results/meta.csv`          device + run metadata
* `results/summary.csv`       per-shape quantile stats + median GB/s
* `results/speedup.csv`       per-shape paired speedup vs. baseline
* `results/verdict.txt`       geomean speedup + per-shape table
* `results/speedup.{pdf,png}` speedup vs. shape with CIs
* `results/bandwidth.{pdf,png}` GB/s vs. shape per kernel
* `results/focused_*.pdf`     focused comparison plots

## Methodology Notes

* **Paired Timing:** All kernels run the same number of iterations in the same order per (draw, iteration). This allows `analyze.py` to pair samples by (draw, sample) for matched-ratio speedup, removing input-to-input variance.
* **L2 Flush:** The L2 cache is flushed before each timed sample to ensure all runs are cold-cache and DRAM-bound, making small and large sizes comparable.
* **Correctness:** Uses max absolute difference. Since transpose is an exact data movement operation, a very tight tolerance of 1e-6 is used to ensure bit-exact or near-bit-exact results.
* **Clocks:** For tighter runs, lock the GPU clock (`nvidia-smi -pm 1; nvidia-smi -lgc <mhz>`) so boost variance is removed.
