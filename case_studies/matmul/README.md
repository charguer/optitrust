# What is this case study?

Matrix Multiplication case study corresponding to [TVM's schedule](https://tvm.apache.org/docs/how_to/optimize_operators/opt_gemm.html) for Intel CPU.

# How do I run the benchmarks?

Install python dependencies:
```sh
python -m pip install -r pip_requirements.txt 
```

Benchmark all:
```sh
make bench
```

Benchmark numpy reference:
```sh
make bench_ref
```

Benchmark `[name].c` implementation:
```sh
make bench_[name]
```