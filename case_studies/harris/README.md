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

# Naive Code Improvements

```c
// TODO: this should be added by OptiTrust tiling
inline int min(int a, int b) { return a < b ? a : b; }

// NOTE: need to decide if 'in' has 3 or 4 channels (alpha)
void grayscale(float* out,
               int h, int w,
               const float* in)
```

# Script Improvements

- TODO: make Function.inline ~simpl work
- `loop_align_stop_extend_start`
  - remove hack: reparse to trigger missed simplifications
- `Function.inline [nbMulti; cDiff [[cTopFunDef ""]] [[cFunDef "harris"]]]`
- `Variable.inline ~simpl [nbMulti; cFunBody "harris"; cConstDef ""]`
- `Instr.accumulate ~nb:9 [nbMulti; cVarDef "acc"];`
- remove `0.0f * x`
- improve 'acc' variable renaming step
  - `Variable.rename ~add_suffix:array` / `Variable.rename ~op:AddSuffix(array)`
  - allow renaming to depend on context (e.g. `acc_${out}`)
- `List.iter local_matrix`
  - `("gray", [Some (expr "by", int 36);  None])`
  - `("gray", [(dim1, (expr "by", int 36))])`
- `List.iter circular_buffer`
  - `(a - 4 + b)%4 = (a+b)%4`, requires static analysis
- ```
  List.iter rewrite [
    "int a; int b; int c; ==> (a + b <= c + b) == (a <= b)";
  ];
  ```
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
- without overlapped tiling: `Image.loop_align_stop_extend_start ~start:(int 0) ~stop:(trm_var "h");`
- with overlapped tiling: `Image.loop_align_stop_extend_start ~start:(trm_var "by") ~stop:(expr "min(h, by + 36)")`