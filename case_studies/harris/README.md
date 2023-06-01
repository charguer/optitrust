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

- `Function.inline [nbMulti; cDiff [[cTopFunDef ""]] [[cFunDef "harris"]]]`
- `Variable.inline ~simpl [nbMulti; cFunBody "harris"; cConstDef ""]`
- `Instr.accumulate ~nb:9 [nbMulti; cVarDef "acc"];`
- remove `0.0f * x`
- improve 'acc' variable renaming step
  - `Variable.rename ~add_suffix:array` / `Variable.rename ~op:AddSuffix(array)`
  - allow renaming to depend on context (e.g. `acc_${out}`)
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

# Script Notes

- not necessary for perf: `Variable.bind_syntactic ~dest:[tBefore; cVarDef "acc_ix"] ~fresh_name:"g${occ}" [cArrayRead "gray"];`
- without overlapped tiling: `!! Image.loop_align_stop_extend_start "y" ~start:(int 0) ~stop:(trm_var "h");`