#include <optitrust_models.h>

__DECL(reduce_sum, "int * (int -> float) -> float");
__AXIOM(reduce_sum_empty, "forall (f: int -> float) -> 0.f =. reduce_sum(0, f)");
__AXIOM(reduce_sum_add_right, "forall (n: int) (f: int -> float) (_: n >= 0) -> reduce_sum(n, f) +. f(n) =. reduce_sum(n + 1, f)");
__DEF(matmul, "fun (A B: int * int -> float) (p: int) -> fun (i j: int) -> reduce_sum(p, fun k -> A(i, k) *. B(k, j))");

/* Multiplies the matrix A (dim m x p) by the matrix B (dim p x n),
 * and writes the result in the matrix C (dim m x n):
 *   C = A * B
 */
void mm(float* c, float* a, float* b, int m, int n, int p) {
  __requires("A: int * int -> float, B: int * int -> float");
  __reads("a ~> Matrix2(m, p, A), b ~> Matrix2(p, n, B)");
  __writes("c ~> Matrix2(m, n, matmul(A, B, p))");

  for (int i = 0; i < m; i++) {
    __xwrites("for j in 0..n -> &c[MINDEX2(m, n, i, j)] ~~> matmul(A, B, p)(i, j)");

    for (int j = 0; j < n; j++) {
      __xwrites("&c[MINDEX2(m, n, i, j)] ~~> matmul(A, B, p)(i, j)");

      float sum = 0.f;
      __ghost(rewrite_float_linear, "inside := fun v -> &sum ~~> v, by := reduce_sum_empty(fun k -> A(i, k) *. B(k, j))");
      for (int k = 0; k < p; k++) {
        __spreserves("&sum ~~> reduce_sum(k, fun k0 -> A(i, k0) *. B(k0, j))");

        __GHOST_BEGIN(focusA, ro_matrix2_focus, "a, i, k");
        __GHOST_BEGIN(focusB, ro_matrix2_focus, "b, k, j");
        sum += a[MINDEX2(m, p, i, k)] * b[MINDEX2(p, n, k, j)];
        __GHOST_END(focusA);
        __GHOST_END(focusB);

        __ghost(in_range_bounds, "k", "k_gt_0 <- lower_bound"); // TODO: proper name k_ge_0
        __ghost(rewrite_float_linear, "inside := fun v -> &sum ~~> v, by := reduce_sum_add_right(k, fun k -> A(i, k) *. B(k, j), k_gt_0)");
      }

      c[MINDEX2(m, n, i, j)] = sum;
    }
  }
}

void mm1024(float* c, float* a, float* b) {
  __requires("A: int * int -> float, B: int * int -> float");
  __reads("a ~> Matrix2(1024, 1024, A), b ~> Matrix2(1024, 1024, B)");
  __writes("c ~> Matrix2(1024, 1024, matmul(A, B, 1024))");

  mm(c, a, b, 1024, 1024, 1024);
}
