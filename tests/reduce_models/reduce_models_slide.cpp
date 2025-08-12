#include <optitrust_models.h>

__ghost_ret plus1() {
  __requires("n: int");
  __requires("np1: int");
  __requires("np1 = n + 1");
  __ensures("eq: np1 = n + 1");
}

void rowSum(int w, int* s, int* d, int n, int cn, int c, int* sum) {
  __requires("S: int * int -> int");
  __requires("w >= 0");
  __requires("n >= 1");
  __requires("cn >= 0");
  /* __writes(
      "d ~> Matrix2(n, cn, fun (i: int) (c: int) -> reduce_int_sum(i, i + w, "
      "fun k -> S(k, c)))"); */
  __reads("s ~> Matrix2(n + w - 1, cn, S)");

  __requires("in_range(c, 0..cn)");
  /* __modifies("for i in 0..1 -> &d[MINDEX2(n, cn, i, c)] ~~> reduce_int_sum(i, i + "
         "w, fun k -> S(k, c))"); */
  __modifies("sum ~~> reduce_int_sum(0 + 1 - 1, 0 + 1 + w - 1, fun k0 -> S(k0, c))");
  __consumes("for i in (0 + 1)..n -> &d[MINDEX2(n, cn, i, c)] ~> UninitCell");
  __produces("for i in (0 + 1)..n -> &d[MINDEX2(n, cn, i, c)] ~~> reduce_int_sum(i, i "
             "+ w, fun k -> S(k, c))");

  for (int i = 0 + 1; i < n; i++) {
    __strict();
    __sreads("s ~> Matrix2(n + w - 1, cn, S)");
    __xwrites(
        "&d[MINDEX2(n, cn, i, c)] ~~> reduce_int_sum(i, i + w, fun k -> "
        "S(k, c))");
    __ghost(assume, "P := in_range(i, 0..n)");
    int sum = 0;
    __ghost(rewrite_linear,
            "inside := fun v -> &sum ~~> v, by := "
            "reduce_int_sum_empty(i, fun k -> S(k, c))");
    for (int k = i; k < i + w; k++) {
      __strict();
      __spreserves("&sum ~~> reduce_int_sum(i, k, fun k0 -> S(k0, c))");
      __sreads("s ~> Matrix2(n + w - 1, cn, S)");
      __ghost(assume, "P := in_range(k, 0..(n + w - 1))");
      const __ghost_fn focus =
          __ghost_begin(ro_matrix2_focus, "matrix := s, i := k, j := c");
      sum += s[MINDEX2(n + w - 1, cn, k, c)];
      __ghost_end(focus);
      __ghost(in_range_bounds, "x := k, a := i", "k_ge_i <- lower_bound");
      __ghost(plus1, "n := k, np1 := k + 1", "kp1 <- eq");
      __ghost(rewrite_linear,
              "inside := fun v -> &sum ~~> v, by := "
              "reduce_int_sum_add_right(i, k, fun k -> S(k, c), k_ge_i, "
              "k + 1, kp1)");
    }
    d[MINDEX2(n, cn, i, c)] = sum;
  }
}
