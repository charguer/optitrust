#include <optitrust_models.h>

__DECL(reduce_sum, "int * (int -> float) -> float");
__AXIOM(reduce_sum_empty, "forall (f: int -> float) -> 0.f =. reduce_sum(0, f)");
__AXIOM(reduce_sum_add_right, "forall (n: int) (f: int -> float) (_: n >= 0) -> reduce_sum(n, f) +. f(n) =. reduce_sum(n + 1, f)");
__DEF(matmul, "fun (A B: int * int -> float) (p: int) -> fun (i j: int) -> reduce_sum(p, fun k -> A(i, k) *. B(k, j))");


/* Multiplies the vect A (dim n) by the vector B (dim n),
 * and returns the result of the scalar product.
*/
float vect_vect_mul(float* a, float* b, int n) {
  __requires("A: int -> float, B: int -> float");
  __reads("a ~> Matrix1(n, A), b ~> Matrix1(n, B)");
  __ensures("_Res =. reduce_sum(n, fun j -> A(j) *. B(j))");
  float s = 0.f;
  __ghost(rewrite_float_linear, "inside := fun v -> &s ~~> v, by := reduce_sum_empty(fun j -> A(j) *. B(j))");
  for (int i = 0; i < n; i++) {
    __spreserves("&s ~~> reduce_sum(i, fun j -> A(j) *. B(j))");
    __GHOST_BEGIN(focusA, ro_matrix1_focus, "a, i");
    __GHOST_BEGIN(focusB, ro_matrix1_focus, "b, i");
    s += a[MINDEX1(n,i)] * b[MINDEX1(n,i)];
    __GHOST_END(focusA);
    __GHOST_END(focusB);
    __ghost(in_range_bounds, "i", "i_gt_0 <- lower_bound");
    __ghost(rewrite_float_linear, "inside := fun v -> &s ~~> v, by := reduce_sum_add_right(i, fun j -> A(j) *. B(j), i_gt_0)");
  }
  __ghost(eq_refl_float, "reduce_sum(n, fun j -> A(j) *. B(j))");
  return s;
}
