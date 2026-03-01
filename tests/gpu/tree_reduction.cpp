#include "optitrust_gpu.h"

__DECL(reduce_sum, "int * (int -> float) -> float");
__AXIOM(reduce_sum_empty, "forall (f: int -> float) -> 0.f =. reduce_sum(0, f)");
__AXIOM(reduce_sum_add_right, "forall (n: int) (f: int -> float) (_: n >= 0) -> reduce_sum(n, f) +. f(n) =. reduce_sum(n + 1, f)");

#include "tree_reduction.h"


float reduce2(int N) {
  __ensures("_Res =. reduce_sum(N, fun (j: int) -> reduce_sum((1 << 8), fun i -> 1.0f))");

  __DEF_TYPED(thing, "int -> float", "fun (j: int) -> reduce_sum((1 << 8), fun (i: int) -> 1.0f)");

  float out = 0.f;
  __ghost(rewrite_float_linear, "inside := (fun v -> &out ~~> v), by := reduce_sum_empty(thing)");
  for (int b = 0; b < N; b++) {
    __spreserves("&out ~~> reduce_sum(b, thing)");

    float * const arr = MALLOC1(float, 1<<8);

    for (int i = 0; i < (1<<8); i++) {
      __xwrites("&arr[MINDEX1(1<<8,i)] ~~> 1.0f");
      arr[MINDEX1(1<<8,i)] = 1.0f;
    }

    const float v = tree_reduce(arr, 8);

    out += v;
    __ghost(rewrite_float_linear, "inside := fun v -> &out ~~> reduce_sum(b, thing) +. v");

    free(arr);

    __ghost(in_range_bounds, "b", "b_geq_0 <- lower_bound");
    __ghost(rewrite_float_linear, "inside := (fun v -> &out ~~> v), by := reduce_sum_add_right(b, thing, b_geq_0)");
  }

  __ghost(eq_refl_float, "reduce_sum(N, thing)");
  return out;
}
