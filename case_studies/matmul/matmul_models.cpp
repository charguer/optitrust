#include <optitrust_models.h>

__DECL(reduce_sum, "Range * (int -> double) -> double");
__AXIOM(reduce_sum_empty, "forall (begin: int) (f: int -> double) -> 0.0 =. reduce_sum(begin..begin, f)");
__AXIOM(reduce_sum_add_right, "forall (begin end: int) (f: int -> double) -> reduce_sum(begin..end, f) +. f(end) =. reduce_sum(begin..(end + 1), f)");
__DEF(matmul, "fun (A B: int * int -> double) (p: int) -> fun (i j: int) -> reduce_sum(0..p, fun k -> A(i, k) *. B(k, j))");

/* Multiplies the matrix A (dim m x p) by the matrix B (dim p x n),
 * and writes the result in the matrix C (dim m x n):
 *   C = A * B
 */
void mm(double* c, double* a, double* b, int m, int n, int p) {
  __requires("A: int * int -> double, B: int * int -> double");
  __reads("a ~> Matrix2(m, p, A), b ~> Matrix2(p, n, B)");
  __writes("c ~> Matrix2(m, n, matmul(A, B, p))");

  for (int i = 0; i < m; i++) {
    __xwrites("for j in 0..n -> &c[MINDEX2(m, n, i, j)] ~~> matmul(A, B, p)(i, j)");

    for (int j = 0; j < n; j++) {
      __xwrites("&c[MINDEX2(m, n, i, j)] ~~> matmul(A, B, p)(i, j)");

      double sum = 0.0;
      __ghost(rewrite_float_linear, "inside := fun v -> &sum ~~> v, by := reduce_sum_empty(0, fun k -> A(i, k) *. B(k, j))");
      for (int k = 0; k < p; k++) {
        __spreserves("&sum ~~> reduce_sum(0..k, fun k0 -> A(i, k0) *. B(k0, j))");

        __GHOST_BEGIN(focusA, ro_matrix2_focus, "a, i, k");
        __GHOST_BEGIN(focusB, ro_matrix2_focus, "b, k, j");
        sum += a[MINDEX2(m, p, i, k)] * b[MINDEX2(p, n, k, j)];
        __GHOST_END(focusA);
        __GHOST_END(focusB);

        __ghost(rewrite_float_linear, "inside := fun v -> &sum ~~> v, by := reduce_sum_add_right(0, k, fun k -> A(i, k) *. B(k, j))");
      }

      c[MINDEX2(m, n, i, j)] = sum;
    }
  }
}

void mm1024(double* c, double* a, double* b) {
  __requires("A: int * int -> double, B: int * int -> double");
  __reads("a ~> Matrix2(1024, 1024, A), b ~> Matrix2(1024, 1024, B)");
  __writes("c ~> Matrix2(1024, 1024, matmul(A, B, 1024))");

  mm(c, a, b, 1024, 1024, 1024);
}
