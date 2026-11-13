# Reduction Benchmark

A reproducible harness to compare multiple GPU reduction kernels against a CPU reference across representative input sizes.

Target: **CUDA 13.x**, **Blackwell** (RTX PRO 6000, `sm_120`).

## Build & Run

    make                    # build (sm_120 default)
    make run                # full sweep -> results/
    make analyze            # -> results/speedup.csv, verdict.txt, figures

Quick smoke test: `make check`.

Set `CUDA_HOME` if your install is not at `/usr/local/cuda`.

## Protocol

For each N (total elements) and each random draw:

1. Generate a random input A (N floats, uniform [−1, 1], seeded).
2. Compute the CPU reference sum (Kahan summation).
3. For each kernel:
   - Run once, read back partial sums, compute final sum, compare to CPU reference using **relative error**.
   - Run `--warmup` untimed iterations.
   - Run `--iters` timed iterations (CUDA events, L2 flush between samples).
   - Record per-sample time (µs) and effective bandwidth (GB/s) to CSV.

## Default Sizes

Powers of 2, covering the range from latency-bound to memory-bandwidth-bound:

    2^18  =  256K   (  1 MB)
    2^20  =    1M   (  4 MB)
    2^22  =    4M   ( 16 MB)
    2^24  =   16M   ( 64 MB)
    2^26  =   64M   (256 MB)
    2^28  =  256M   (  1 GB)

Small N: latency-bound, kernel differences visible.
Large N: memory-bandwidth-bound, all kernels converge toward the roofline.

## Output

* `results/samples.csv`       raw per-sample time_us / gbps
* `results/correctness.csv`   per-config relative error
* `results/meta.csv`          device + run metadata
* `results/summary.csv`       per-size quantile stats + median GB/s
* `results/speedup.csv`       per-size paired speedup vs. baseline
* `results/verdict.txt`       geomean speedup + per-size table
* `results/speedup.{pdf,png}` speedup vs. size with CIs
* `results/bandwidth.{pdf,png}` GB/s vs. size per kernel
* `results/focused_*.pdf`     focused comparison plots

## Methodology Notes

* **Paired Timing:** All kernels run the same number of iterations in the same order per (draw, iteration). This allows `analyze.py` to pair samples by (draw, sample) for matched-ratio speedup, removing input-to-input variance.
* **L2 Flush:** The L2 cache is flushed before each timed sample to ensure all runs are cold-cache and DRAM-bound, making small and large sizes comparable.
* **Correctness:** Uses relative error against a Kahan summation reference. A tolerance of 1e-3 (0.1%) is used to account for floating-point accumulation differences while catching genuine kernel bugs.
* **Clocks:** For tighter runs, lock the GPU clock (`nvidia-smi -pm 1; nvidia-smi -lgc <mhz>`) so boost variance is removed.
