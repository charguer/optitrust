#include <optitrust_models.h>

// TODO: move to share with matmul_models, into optitrust_models
__DECL(reduce_sum, "int * (int -> int) -> int");
__AXIOM(reduce_sum_empty, "forall (f: int -> int) -> 0 = reduce_sum(0, f)");
__AXIOM(reduce_sum_add_right, "forall (n: int) (f: int -> int) (_: n >= 0) -> reduce_sum(n, f) + f(n) = reduce_sum(n + 1, f)");

/*
  cn: number of (color) channels
  w: width of box filter (convolution window)
  n: size of the row resulting from filtering
*/
void rowSum(const int w, const int* s, int* d, const int n, const int cn) {
  __requires("S: int * int -> int");
  __requires("w >= 0, n >= 1, cn >= 0");
  __reads("s ~> Matrix2(n+w-1, cn, S)");
  __writes("d ~> Matrix2(n, cn, fun (i c: int) -> reduce_sum(w, fun k -> S(i+k,c)))");

  for (int i = 0; i < n; i++) { // for each pixel
    __sreads("s ~> Matrix2(n+w-1, cn, S)");
    __xwrites("for c in 0..cn -> &d[MINDEX2(n, cn, i, c)] ~~> reduce_sum(w, fun k -> S(i+k,c))");

    for (int c = 0; c < cn; c++) { // foreach channel
      __sreads("s ~> Matrix2(n+w-1, cn, S)");
      __xwrites("&d[MINDEX2(n, cn, i, c)] ~~> reduce_sum(w, fun k -> S(i+k,c))");

      // TODO: difference with assert_prop admit ?
      __ghost(assume, "is_subrange(i..i + w, 0..n + w - 1)"); // TODO: solve

      int sum = 0;
      __ghost(rewrite_linear, "inside := fun v -> &sum ~~> v, by := reduce_sum_empty(fun k -> S(i+k,c))");
      __ghost(rewrite_linear, "inside := fun v -> &sum ~~> reduce_sum(v, fun k0 -> S(i + k0, c)), from := 0, to := i - i");

      for (int k = i; k < i+w; k++) {  // for (int k = 0; k < w; k++) {
        __spreserves("&sum ~~> reduce_sum(k-i, fun k0 -> S(i+k0,c))");

        __ghost(in_range_extend, "k, i..i+w, 0..n+w-1"); // i+k < n+w-1
        __GHOST_BEGIN(focus, ro_matrix2_focus, "s, k, c"); // i+k
        sum += s[MINDEX2(n+w-1, cn, k, c)];
        __GHOST_END(focus);

        // __ghost(in_range_bounds, "k", "k_ge_0 <- lower_bound"); // i+k_ge_0
        // TODO: remove admit
        __ghost(assert_prop, "proof := admit(k - i >= 0)", "kmi_ge_0 <- proof");

        __ghost(rewrite_linear, "inside := fun v -> &sum ~~> reduce_sum(k - i, fun k0 -> S(i + k0, c)) + S(v, c), from := k, to := i + (k - i)");
        __ghost(rewrite_linear, "inside := fun v -> &sum ~~> v, by := reduce_sum_add_right(k - i, fun k -> S((i+k),c), kmi_ge_0)");
        __ghost(rewrite_linear, "inside := fun v -> &sum ~~> reduce_sum(v, fun k0 -> S(i + k0, c)), from := k - i + 1, to := (k + 1) - i");
      }

      __ghost(rewrite_linear, "inside := fun v -> &sum ~~> reduce_sum(v, fun k0 -> S(i + k0, c)), from := i + w - i, to := w");
      d[MINDEX2(n, cn, i, c)] = sum;
    }
  }
}
