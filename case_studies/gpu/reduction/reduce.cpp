#include "optitrust_common.h"
#include <optitrust_models.h>

__DECL(reduce_sum, "int * (int -> float) -> float");
__AXIOM(reduce_sum_empty, "forall (f: int -> float) -> 0.f =. reduce_sum(0, f)");
__AXIOM(reduce_sum_add_right, "forall (n: int) (f: int -> float) (_: n >= 0) -> reduce_sum(n, f) +. f(n) =. reduce_sum(n + 1, f)");

float reduce(float *arr, int N) {
  __requires("A: int -> float");
  __reads("arr ~> Matrix1(N, A)");
  __ensures("_Res =. reduce_sum(N, A)");

  float sum = 0.f;
  __ghost(rewrite_float_linear, "inside := (fun v -> &sum ~~> v), by := reduce_sum_empty(A)");

  for (int i = 0; i < N; i++) {
    __spreserves("&sum ~~> reduce_sum(i,A)");
    __xreads("&arr[MINDEX1(N,i)] ~~> A(i)");

    sum += arr[MINDEX1(N,i)];
    __ghost(in_range_bounds, "i", "i_geq_0 <- lower_bound");
    __ghost(rewrite_float_linear, "inside := (fun v -> &sum ~~> v), by := reduce_sum_add_right(i, A, i_geq_0)");
  }

  __ghost(assume, "P := reduce_sum(N, A) =. reduce_sum(N, A)");
  return sum;
}
