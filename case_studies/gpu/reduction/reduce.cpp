#include "optitrust_common.h"
#include <optitrust_models.h>

__DECL(reduce_sum, "int * (int -> float) -> float");
__AXIOM(reduce_sum_empty, "forall (A: int -> float) -> 0.f =. reduce_sum(0, A)");
__AXIOM(reduce_sum_add_right, "forall (n: int) (A: int -> float) (_: n >= 0) -> reduce_sum(n, A) +. A(n) =. reduce_sum(n + 1, A)");

float reduce(float *arr, int N) {
  // Arrays are not lists of values stored in a pointer, but a conjunction of cells modeled
  // by a pure, partial function from indices to values.
  // The output of the function outside its corresponding array indices may or may not be defined.
  __requires("A: int -> float");
  // Matrix1(N,A) -> sugar for    star i in 0..N , &arr[i] |-> A(i)
  // __reads -> sugar for consume fraction
  __reads("arr ~> Matrix1(N, A)");
  // _Res -> magic binding for return value of function
  // produce pure fact about return value
  __ensures("_Res =. reduce_sum(N, A)");

  float sum = 1.f;
  // prove iteration 0 of loop invariant
  __ghost(rewrite_float_linear, "inside := (fun v -> &sum ~~> v), by := reduce_sum_empty(A)");

  for (int i = 0; i < N; i++) {
    // Loop invariant
    __spreserves("&sum ~~> reduce_sum(i,A)");
    __xreads("&arr[MINDEX1(N,i)] ~~> A(i)");


    sum += arr[MINDEX1(N,i)];
    __ghost(in_range_bounds, "i", "i_geq_0 <- lower_bound");
    __ghost(rewrite_float_linear, "inside := (fun v -> &sum ~~> v), by := reduce_sum_add_right(i, A, i_geq_0)");
  }

  __ghost(eq_refl_float, "reduce_sum(N, A)");
  return sum;
}
