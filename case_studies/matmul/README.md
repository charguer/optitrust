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

Benchmark interesting subset:
```sh
make bench_demo
```

Benchmark numpy and TVM reference:
```sh
make bench_ref
```

Benchmark `[name].c` implementation:
```sh
make bench_[name]
```

# Notes

## Static Analysis

- add divides hypothesis to mm function 'List.forall (divides 32) [n; m; p]'
- check hypothesis that loop iterations are disjoint for reorder/fission
    - pfor { for { } } --> for { pfor { } }
- parallel for:
    - (1) add ghost instr; (2) loop invariant; (3) tag loop as parallel in logic
    - !! Loop.writes_to_tiles ~parallel:true ... [cFor "bj"];

## Script Improvements

- simplify `exact_div` with `Arith.(simpl compute)`
- allocate one 'sum' accumulator per thread?
    - hoist = create array + reuse space
- '*' '/' to bitshift
- memset/memcpy insertions

- allow unrolling without requiring shift?
- allow SIMD before unroll
- avoid requiring an explicit reparse to get types for shift_to_zero
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