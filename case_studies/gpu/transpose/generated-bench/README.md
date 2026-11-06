# transpose_bench

Academic-style benchmark harness for GPU matrix-transpose kernels.

## Files

- `transpose_benchmark.cu` — the kernels (`naive`, `nobank` = the classic
  32x32 shared-memory tile with +1 padding, `optitrust` = the `__kernel0`
  generated kernel) plus the benchmark engine.
- `main.cpp` — CLI driver: sizes, kernels, sample counts, seed, output dir.
- `optitrust_gpu_cuda.cuh` — CUDA helper macros/types (`MSIZE*`, `MINDEX*`,
  `exact_div`, checked `cudaMalloc`/`cudaMemcpy` wrappers, `CUDA_CHECK`).
- `transpose_benchmark.h` — host-side interface shared by `main.cpp` and the `.cu`.
- `stats.h` — quantiles (linear interpolation, numpy "type 7") and summary stats.
- `analyze.py` — reads the CSVs, prints per-size medians/IQRs, runs pairwise
  Mann-Whitney U tests between kernels per size, writes `stats.txt` and, if
  matplotlib is installed, `boxplot_HxW.png` per size.
- `Makefile` — build / run / analyze / test targets.

## Build and run

```
make            # nvcc -O3 -Xcompiler -O3,-march=native, arch auto-detected via nvidia-smi
make run        # ./transpose_bench --out results
make quick      # small smoke run (2 inputs, 30 samples)
make analyze    # run, then python3 analyze.py results
make test       # host-only unit tests for stats.h
```

Custom runs:

```
./transpose_bench --sizes 1024x1024,4096x4096,2080x4128 --kernels nobank,optitrust \
    --inputs 5 --runs 200 --warmup 20 --seed 7 --out results_run2
```

## Methodology

For each size (H, W) and each of `--inputs` random inputs:

1. generate a random HxW float matrix on the host (seeded per size/input),
2. compute the CPU reference transpose (tiled, cache-friendly),
3. run each kernel once on the GPU and compare its output **bit-exactly**
   against the reference (transposition is a permutation, so any arithmetic
   difference shows up); results go to `correctness.csv`,
4. after `--warmup` untimed launches, enqueue `--runs` timed launches with a
   pair of CUDA events around each launch, synchronize once, and record one
   sample per launch (`samples.csv`: kernel,H,W,input_id,run,time_ms,bandwidth_GBs).
   Enqueueing all timed launches before the single synchronize keeps the GPU
   busy back-to-back, so host launch latency is not charged to the kernels.

`summary.csv` aggregates all samples of a (kernel, size) pair: n, mean, std,
min, Q1, median, Q3, max for both ms and GB/s. `meta.txt` records device and
run configuration.

## Suggested sizes

The tiled kernels require H and W to be multiples of 32. A representative set
(default in `main.cpp`): 32x32 (single tile, launch-latency dominated),
512x512 (L2-resident), 1024x1024, 2048x2048 (around typical L2 size),
4096x4096 and 8192x8192 (DRAM-bound), 1024x4096 / 4096x1024 and 2048x8192
(rectangular, to expose aspect-ratio effects on coalescing and grid shape),
2080x4128 (non-power-of-two tile counts, 65x129 tiles, to check tile-boundary
generality).

## Output CSV schemas

- `samples.csv`: `kernel,H,W,input_id,run,time_ms,bandwidth_GBs`
- `correctness.csv`: `kernel,H,W,input_id,mismatches,status`
- `summary.csv`: `kernel,H,W,n_samples,mean_ms,std_ms,min_ms,q1_ms,median_ms,q3_ms,max_ms,mean_GBs,median_GBs,q1_GBs,q3_GBs`

The benchmark exits non-zero if any correctness check fails.
