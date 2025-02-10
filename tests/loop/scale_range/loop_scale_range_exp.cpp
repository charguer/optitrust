#include <optitrust.h>

void f(int N) {
  __pure();
  int x = 0;
  __ghost(to_prove, "P := __is_true(2 != 0)");
  for (int i_s = 0; i_s < 2 * 10; i_s += 2) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(exact_div(i_s, 2), 0..10)");
    x += i_s;
  }
  const int ratio = 5;
  __ghost(to_prove, "P := __is_true(ratio != 0)");
  for (int j = 0; j < ratio * N; j += ratio) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(exact_div(j, ratio), 0..N)");
    x += j;
  }
}

void ghost_in_range(int N) {
  __pure();
  int x = 0;
  __ghost(to_prove, "P := __is_true(4 != 0)");
  for (int i = 0; i < 4 * (N - 2); i += 4) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(exact_div(i, 4), 0..N - 2)");
    __ghost([&]() { __requires("in_range(exact_div(i, 4), 0..N - 2)"); }, "");
    x += exact_div(i, 4);
  }
}

void arrays(int N, int* w, int* r, int* f) {
  __modifies("f ~> Matrix1(N)");
  __writes("w ~> Matrix1(N)");
  __reads("r ~> Matrix1(N)");
  __ghost(to_prove, "P := __is_true(2 != 0)");
  __ghost(group_scale_uninit,
          "stop := N, step := 1, items := fun i -> &w[MINDEX1(N, i)] ~> Cell, "
          "factor := 2, new_step := 2, new_stop := 2 * N");
  __ghost(group_scale,
          "stop := N, step := 1, items := fun i -> &f[MINDEX1(N, i)] ~> Cell, "
          "factor := 2, new_step := 2, new_stop := 2 * N");
  __ghost(
      [&]() {
        __modifies(
            "for __TMP_3 in range(0, 2 * N, 2) -> &f[MINDEX1(N, "
            "exact_div(__TMP_3, 2))] ~> Cell");
        __admitted();
        __with("justif := arith_simpl");
      },
      "");
  __ghost(
      [&]() {
        __modifies(
            "_Uninit(for __TMP_3 in range(0, 2 * N, 2) -> &w[MINDEX1(N, "
            "exact_div(__TMP_3, 2))] ~> Cell)");
        __admitted();
        __with("justif := arith_simpl");
      },
      "");
  for (int i = 0; i < 2 * N; i += 2) {
    __strict();
    __xmodifies("&f[MINDEX1(N, exact_div(i, 2))] ~> Cell");
    __xwrites("&w[MINDEX1(N, exact_div(i, 2))] ~> Cell");
    __ghost(assume, "P := in_range(exact_div(i, 2), 0..N)");
    w[MINDEX1(N, exact_div(i, 2))] = exact_div(i, 2);
    f[MINDEX1(N, exact_div(i, 2))] =
        exact_div(i, 2) + f[MINDEX1(N, exact_div(i, 2))];
  }
  __ghost(
      [&]() {
        __modifies(
            "for __TMP_3 in range(0, 2 * N, 2) -> &f[MINDEX1(N, "
            "exact_div(__TMP_3, 2))] ~> Cell");
        __admitted();
        __with("justif := arith_simpl");
      },
      "");
  __ghost(
      [&]() {
        __modifies(
            "for __TMP_3 in range(0, 2 * N, 2) -> &w[MINDEX1(N, "
            "exact_div(__TMP_3, 2))] ~> Cell");
        __admitted();
        __with("justif := arith_simpl");
      },
      "");
  __ghost(group_unscale,
          "stop := N, step := 1, items := fun i -> &w[MINDEX1(N, i)] ~> Cell, "
          "factor := 2, new_step := 2, new_stop := 2 * N");
  __ghost(group_unscale,
          "stop := N, step := 1, items := fun i -> &f[MINDEX1(N, i)] ~> Cell, "
          "factor := 2, new_step := 2, new_stop := 2 * N");
}
