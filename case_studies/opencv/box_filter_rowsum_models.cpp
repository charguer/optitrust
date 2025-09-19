#include <optitrust_models.h>

__GHOST(plus1) {
  __requires("n: int, np1: int, np1 = n + 1");
  __ensures("eq: np1 = n + 1");
}

/*
  cn: number of (color) channels
  w: width of box filter (convolution window)
  n: size of the row resulting from filtering
*/
// NOTE: using 'int' as 'Z' to avoid overflow questions, instead of 'u8'/'u16'
void rowSum(const int w, const int* s, int* d, const int n, const int cn) {
  __requires("S: int * int -> int");
  __requires("w >= 0, n >= 1, cn >= 0");
  __reads("s ~> Matrix2(n+w-1, cn, S)");
  __writes("d ~> Matrix2(n, cn, fun (i c: int) -> reduce_int_sum(i, i+w, fun k -> S(k,c)))");

  for (int i = 0; i < n; i++) { // for each pixel
    __sreads("s ~> Matrix2(n+w-1, cn, S)");
    __xwrites("for c in 0..cn -> &d[MINDEX2(n, cn, i, c)] ~~> reduce_int_sum(i, i+w, fun k -> S(k,c))");

    for (int c = 0; c < cn; c++) { // foreach channel
      __sreads("s ~> Matrix2(n+w-1, cn, S)");
      __xwrites("&d[MINDEX2(n, cn, i, c)] ~~> reduce_int_sum(i, i+w, fun k -> S(k,c))");

      int sum = 0;
      __ghost(rewrite_linear, "inside := fun v -> &sum ~~> v, by := reduce_int_sum_empty(i, fun k -> S(k,c))");

      for (int k = i; k < i+w; k++) {
        __spreserves("&sum ~~> reduce_int_sum(i, k, fun k0 -> S(k0,c))");

        __ghost(assume, "in_range(k, 0..n+w-1)");
        __GHOST_BEGIN(focus, ro_matrix2_focus, "s, k, c");
        sum += s[MINDEX2(n+w-1, cn, k, c)];
        __GHOST_END(focus);

        __ghost(in_range_bounds, "k, i", "k_ge_i <- lower_bound");
        __ghost(plus1, "k, k+1", "kp1 <- eq");
        __ghost(rewrite_linear, "inside := fun v -> &sum ~~> v, by := reduce_int_sum_add_right(i, k, fun k -> S(k,c), k_ge_i, k + 1, kp1)");
      }

      d[MINDEX2(n, cn, i, c)] = sum;
    }
  }
}
