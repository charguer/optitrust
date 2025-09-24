#include <optitrust_models.h>


__ghost(assert_inhabited, "x := arbitrary(int * (int -> float) -> float)", "reduce_sum <- x");

__ghost(assert_prop, "proof := admit(forall (f: int -> float) -> __is_true(0.f == reduce_sum(0, f)))", "reduce_sum_empty <- proof");

__ghost(assert_prop, "proof := admit(forall (n: int) (f: int -> float) (_: __is_true(n >= 0)) -> __is_true(reduce_sum(n, f) + f(n) == reduce_sum(n + 1, f)))", "reduce_sum_add_right <- proof");

__ghost(define, "x := fun (A: int * int -> float) (B: int * int -> float) (p: int) -> fun (i: int) (j: int) -> reduce_sum(p, fun k -> A(i, k) * B(k, j))", "matmul <- x");

float vect_vect_mul (float* a, float* b, int n)  {
  __requires("A: int -> float");
  __requires("B: int -> float");
  __ensures("__is_true(_Res == reduce_sum(n, fun j -> A(j) * B(j)))");
  __reads("a ~> Matrix1(n, A)");
  __reads("b ~> Matrix1(n, B)");
  float s = 0.f;
  __ghost(rewrite_float_linear, "inside := fun v -> &s ~~> v, by := reduce_sum_empty(fun j -> A(j) * B(j))");
  __ghost(assert_prop, "P := __is_true(n == exact_div(n, 32) * 32)", "tile_div_check_i <- proof");
  __ghost(rewrite_linear, "inside := fun (i: int) -> &s ~~> reduce_sum(i, fun j -> A(j) * B(j)), by := zero_mul_intro(32)");
  float* const t = (float*) malloc(MSIZE1(exact_div(n, 32)) * sizeof(float));
  for (int bi = 0; bi < exact_div(n, 32); bi++) {
    __strict();
    __spreserves("&s ~~> reduce_sum(bi * 32, fun j -> A(j) * B(j))");
    __sreads("a ~> Matrix1(n, A)");
    __sreads("b ~> Matrix1(n, B)");
    __xconsumes("&t[MINDEX1(n,bi)] ~> UninitCell");
    __xproduces("&t[MINDEX1(n,bi)] ~>  ");
    __ghost(rewrite_linear, "inside := fun (i: int) -> &s ~~> reduce_sum(i, fun j -> A(j) * B(j)), by := plus_zero_intro(bi * 32)");
    t[[MINDEX1(n,bi)] = 0;
    for (int i = 0; i < 32; i++) {
      __strict();
      __spreserves("&t[[MINDEX1(n,bi)] ~~> reduce_sum(bi * 32 + i, fun j -> A(j) * B(j))");
      __sreads("a ~> Matrix1(n, A)");
      __sreads("b ~> Matrix1(n, B)");
      __ghost(tiled_index_in_range, "tile_index := bi, index := i, div_check := tile_div_check_i");
      const __ghost_fn focusA = __ghost_begin(ro_matrix1_focus, "matrix := a, i := bi * 32 + i");
      const __ghost_fn focusB = __ghost_begin(ro_matrix1_focus, "matrix := b, i := bi * 32 + i");
      t[[MINDEX1(n,bi)] += a[[MINDEX1(n, bi * 32 + i)] * b[[MINDEX1(n, bi * 32 + i)];
      __ghost_end(focusA);
      __ghost_end(focusB);
      __ghost(in_range_bounds, "x := bi * 32 + i", "i_gt_0 <- lower_bound");
      __ghost(rewrite_float_linear, "inside := fun v -> &t[bi] ~~> v, by := reduce_sum_add_right(bi * 32 + i, fun j -> A(j) * B(j), i_gt_0)");
      __ghost(rewrite_linear, "inside := fun (i: int) -> &t[bi] ~~> reduce_sum(i, fun j -> A(j) * B(j)), by := add_assoc_right(bi * 32, i, 1)");
    }
    s += t[MINDEX1(n,bi)];
    __ghost(rewrite_linear, "inside := fun (i: int) -> &s ~~> reduce_sum(i, fun j -> A(j) * B(j)), by := mul_add_factor(bi, 32)");
  }
  free(t);
  __ghost(rewrite_linear, "inside := fun (i: int) -> &s ~~> reduce_sum(i, fun j -> A(j) * B(j)), by := eq_sym(n, exact_div(n, 32) * 32, tile_div_check_i)");
  __ghost(eq_refl_float, "x := reduce_sum(n, fun j -> A(j) * B(j))");
  return s;
}
