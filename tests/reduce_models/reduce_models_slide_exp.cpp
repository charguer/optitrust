#include <optitrust_models.h>

__ghost_ret plus1() {
  __requires("n: int");
  __requires("np1: int");
  __requires("__is_true(np1 == n + 1)");
  __ensures("eq: __is_true(np1 == n + 1)");
}

void rowSum(int w, int* s, int* d, int n, int cn, int c, int* sum) {
  __requires("S: int * int -> int");
  __requires("__is_true(w >= 0)");
  __requires("__is_true(n >= 1)");
  __requires("__is_true(cn >= 0)");
  __requires("in_range(c, 0..cn)");
  __preserves(
      "sum ~~> reduce_int_sum(0 + 1 - 1, 0 + 1 + w - 1, fun k0 -> S(k0, c))");
  __writes(
      "for i in (0 + 1)..n -> &d[MINDEX2(n, cn, i, c)] ~~> reduce_int_sum(i, i "
      "+ w, fun k -> S(k, c))");
  __reads("s ~> Matrix2(n + w - 1, cn, S)");
  int sum = *sum;
  for (int i = 0 + 1; i < n; i += 1) {
    __spreserves(
        "&sum ~~> reduce_int_sum(i - 1, i + w - 1, fun k0 -> S(k0, c))");
    __xwrites(
        "&d[MINDEX2(n, cn, i, c)] ~~> reduce_int_sum(i, i + w, fun k0 -> S(k0, "
        "c))");
    __ghost(to_prove, "P := in_range(i - 1, 0..(n + w - 1))");
    __ghost(to_prove, "P := in_range(c, 0..cn)");
    const __ghost_fn __ghost_pair_1 =
        __ghost_begin(ro_matrix2_focus, "matrix := s, i := i - 1, j := c");
    __ghost(to_prove, "P := in_range(i + 1 + w - 1, 0..(n + w - 1))");
    __ghost(to_prove, "P := in_range(c, 0..cn)");
    const __ghost_fn __ghost_pair_2 = __ghost_begin(
        ro_matrix2_focus, "matrix := s, i := i + 1 + w - 1, j := c");
    sum += s[MINDEX2(n + w - 1, cn, i + 1 + w - 1, c)] -
           s[MINDEX2(n + w - 1, cn, i - 1, c)];
    __ghost_end(__ghost_pair_2);
    __ghost_end(__ghost_pair_1);
    __ghost(to_prove, "P := __is_true(i + w - 1 >= i - 1)", "H <- H");
    __ghost(to_prove, "P := __is_true(i + 1 - 1 == i - 1 + 1)", "H1 <- H");
    __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w - 1 + 1)",
            "H2 <- H");
    __ghost(rewrite_linear,
            "inside := [&] (int v) -> HProp  &sum ~~> v, by := "
            "reduce_int_sum_slide(i - 1, i + w - 1, i + 1 - 1, i + 1 + w - 1, "
            "fun k0 -> S(k0, c), H, H1, H2)");
    d[MINDEX2(n, cn, i, c)] = sum;
    __ghost(to_prove, "P := __is_true(i + 1 - 1 == i)", "H3 <- H");
    __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w)", "H4 <- H");
    __ghost(rewrite_linear,
            "inside := [&] (int v) -> HProp  &d[MINDEX2(n, cn, i, c)] ~~> "
            "reduce_int_sum(v, i + 1 + w - 1, fun k0 -> S(k0, c)), by := H3");
    __ghost(rewrite_linear,
            "inside := [&] (int v) -> HProp  &d[MINDEX2(n, cn, i, c)] ~~> "
            "reduce_int_sum(i, v, fun k0 -> S(k0, c)), by := H4");
  }
}
