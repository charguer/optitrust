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

# Script Improvements

- TODO: for transfos: `~simpl:[surrounding Arith.(simpl gather)]`
- TODO:
  `Function.inline [cDiff [[cTopFunDef ""]] [[cFunDef "harris"]]]`
- `!! Instr.accumulate ~nb:9 [nbMulti; cVarDef "acc"];`
- remove `0.0f * x`
- improve 'acc' variable renaming step
- FIXME: duplicates even with suffix:
  ```
  !! ["conv3x3"; "sobelX"; "sobelY"; (* "binomial"; *) "mul"; "coarsity"] |>  List.iter (fun fun_to_inline ->
    Function.inline ~delete:true ~vars:(Variable.Rename.add_suffix ("_" ^ fun_to_inline)) [nbMulti; cFun fun_to_inline];
  );
  ```
- could specialize 'conv2D' into 'conv3x3':
  ```
  (* Function.specialize ? *)
  !! Specialize.function_arg "conv3x3" [true; true; true; true; false; false; true] [nbMulti; cFun "conv2D"];
  ```