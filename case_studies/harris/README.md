# What is this case study?

Harris Corner Detection case study corresponding to [Halide's schedule](https://github.com/halide/Halide/blob/0782d80b4907f94b4bc2b0df806306952ad39111/apps/harris/) for Intel CPU.

# How do I run the benchmarks?

Benchmark all:
```sh
make bench
```

Benchmark Halide reference:
```sh
make bench_halide
```

Benchmark `[name].cpp` implementation:
```sh
make bench_[name]
```