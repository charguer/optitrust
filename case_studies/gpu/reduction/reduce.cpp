#include <optitrust_gpu.h>

__DECL(reduce_sum, "int * (int -> float) -> float");
__AXIOM(reduce_sum_empty, "forall (f: int -> float) -> 0.f =. reduce_sum(0, f)");
__AXIOM(reduce_sum_add_right, "forall (n: int) (f: int -> float) (_: n >= 0) -> reduce_sum(n, f) +. f(n) =. reduce_sum(n + 1, f)");

float reduce(float *arr, int N) {
  __requires("A: int -> float");
  __reads("arr ~> Matrix1(N, A)");
  __ensures("_Res =. reduce_sum(N, A)");
  __preserves("HostCtx");

  float sum = 0.f;
  __ghost(rewrite_float_linear, "inside := (fun v -> &sum ~~> v), by := reduce_sum_empty(A)");

  for (int i = 0; i < N; i++) {
    __spreserves("&sum ~~> reduce_sum(i,A)");

    __GHOST_BEGIN(focusArr, ro_matrix1_focus, "arr, i");
    sum += arr[MINDEX1(N,i)];
    __GHOST_END(focusArr);

    __ghost(in_range_bounds, "i", "i_geq_0 <- lower_bound");
    __ghost(rewrite_float_linear, "inside := (fun v -> &sum ~~> v), by := reduce_sum_add_right(i, A, i_geq_0)");
  }

  __ghost(eq_refl_float, "reduce_sum(N, A)");
  return sum;
}
