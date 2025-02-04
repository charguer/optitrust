# What is this case study?

Matrix Multiplication case study corresponding to [TVM's schedule](https://web.archive.org/web/20240920165959/https://tvm.apache.org/docs/how_to/optimize_operators/opt_gemm.html) for Intel CPU.

- Original TVM code in v0.19.0: https://github.com/apache/tvm/blob/v0.19.0/gallery/how_to/optimize_operators/opt_gemm.py
- The TVM code was also copy-pasted in [bench.py](bench.py)

# How do I install dependencies?

Install the Intel OneAPI Base Toolkit to get access to the `icx` compiler and Intel MKL library (following instructions copy pasted from official documentation):
```sh
# download the key to system keyring
wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB \
| gpg --dearmor | sudo tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null

# add signed entry to apt sources and configure the APT client to use Intel repository:
echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] https://apt.repos.intel.com/oneapi all main" | sudo tee /etc/apt/sources.list.d/oneAPI.list

# update packages list and repository index:
sudo apt update

# install the Intel® oneAPI Base Toolkit package
sudo apt install intel-basekit
```

Then, everytime you want the Intel tools in your shell environment, run:
```sh
source /opt/intel/oneapi/setvars.sh
```

Install python dependencies (numpy, Intel MKL and Apache TVM):
```sh
python3 -m pip install -r pip_requirements.txt 
```

## Versions successfully used

```sh
> oneapi-cli version
v0.2.0-36-g69364e768c

> icx --version
Intel(R) oneAPI DPC++/C++ Compiler 2023.2.0 (2023.2.0.20230721)
Target: x86_64-unknown-linux-gnu
Thread model: posix

> python3 --version
Python 3.10.12
```

# How do I run the benchmarks?

Benchmark only numpy and TVM baselines:
```sh
make bench_ref
```

Benchmark baselines and OptiTrust generated code:
```sh
make bench
```

Benchmark `[name].c` implementation:
```sh
make bench_[name]
```

Benchmarks baselines and all visible matmul C files (generated or handwritten):
```sh
make bench_all
```

# Notes

## Script Improvements

- allocate one 'sum' accumulator per thread?
    - hoist = create array + reuse space
- '*' '/' to bitshift
- memset/memcpy insertions

- allow unrolling without requiring shift?
- convenient loop hoist with static analysis inference
- define 'Loop.multi_tile' to replace foreach?
    Loop.multi_tile (trm_int size) ~index:"b${index}" ~bound:TileDivides
        [("i", 32); ("j", 32); ("k", 4)] [cLabel "C"; cFor index_to_split]

## Script Details

- !! Loop.reorder_at ~order:["bi"; "bj"; "bk"; "i"; "k"; "j"] [cPlusEqVar "sum"];
=
!! Loop.reorder ~order:["bi"; "bj"; "i"; "j"] [cFor "bi"];
!! Loop.hoist ~nb_loops:2 [cVarDef "sum"];
!! Loop.fission_all_instrs ~nb_loops:2 [cFor "i"];
!! Loop.reorder ~order:["bk"; "i"; "k"; "j"] [cFor ~body:[cPlusEqVar "sum"] "i"];
- !!! Loop.hoist_expr "pB" [0; 1; 1; 0; 1; 1] [cArrayRead "B"];
=
!!! Variable.bind "pB" [cArrayRead "B"];
!! Loop.hoist_alloc [0; 1; 1; 0; 1; 1] [cVarDef "pB"];
!! Loop.hoist_instr [0; 1; 1; 0; 1; 1] [cArrayWrite "pB"];
