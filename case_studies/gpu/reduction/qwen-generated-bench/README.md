# Reduction benchmark (OptiGPU / reference / best)

A reproducible harness to compare multiple GPU reduction kernels against a
CPU reference across representative input sizes.

Target: **CUDA 13.2**, **Blackwell** (RTX PRO 6000, `sm_120`).

## Build & run

    make                    # build (sm_120 default)
    make run                # full sweep -> results/
    make analyze            # -> results/speedup.csv, verdict.txt, figures

Quick smoke test: `make check`.

## Protocol

For each N (total elements) and each random draw:

1. Generate a random input A (N floats, seeded).
2. Compute the CPU reference sum (Kahan summation).
3. For each kernel:
   - Run once, read back partial sums, compute final sum, compare to CPU
     (relative error <= --tol). Mismatch → non-zero exit.
   - Run --warmup untimed iterations.
   - Run --iters timed iterations (CUDA events, L2 flush between samples).

## Suggested sizes (default set)

    2^18  =  256K   (  1 MB)
    2^20  =    1M   (  4 MB)
    2^22  =    4M   ( 16 MB)
    2^24  =   16M   ( 64 MB)   ← your current test size
    2^26  =   64M   (256 MB)
    2^28  =  256M   (  1 GB)
    2^30  =    1G   (  4 GB)

Small N: latency-bound, kernel differences visible.
Large N: memory-bandwidth-bound, all kernels converge toward the roofline.

## Methodology notes

- **L2 flush:** without it, arrays that fit in L2 (≤ ~128 MB on Blackwell)
  will be served from cache, inflating bandwidth. The flush makes all sizes
  DRAM-bound and comparable.
- **Paired timing:** each kernel runs the same number of iterations in the
  same order, so `analyze.py` can pair samples by (draw, iteration) for
  matched-ratio speedup.
- **CPU reference:** Kahan summation minimizes FP accumulation error so
  the tolerance (--tol, default 1e-3 relative) is only exceeded by genuine
  kernel bugs, not rounding noise.

## Output

* results/samples.csv       raw per-sample time_us / gbps
* results/correctness.csv   per-config relative error
* results/meta.csv          device + run metadata
* results/speedup.csv       per-size paired speedup vs. baseline
* results/verdict.txt       human-readable summary + geomean speedup
* results/speedup.png       speedup vs. log10(N) with CIs
* results/bandwidth.png     GB/s vs. log10(N) per kernel
