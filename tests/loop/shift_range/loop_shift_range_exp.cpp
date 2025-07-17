#include <optitrust.h>

void f() {
  __pure();
  int x = 0;
  const int st = 0;
  const int N = 10;
  for (int i_s = 4; i_s < 12; i_s++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(i_s - 2, 2..10)");
    x += i_s;
  }
  for (int i2 = 2; i2 < 12; i2++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(i2 - 2, 0..10)");
    x += i2 - 2;
  }
  int w = 10 + 2;
  for (int j2 = 0; j2 < N; j2++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(j2 + st, st..(st + N))");
    x += j2 + st;
  }
  const int shift = 5;
  for (int k2 = shift; k2 < N + shift; k2++) {
    __strict();
    __smodifies("&x ~> Cell");
    const int k = k2 - shift;
    __ghost(assume, "P := in_range(k, 0..N)");
    x += k;
  }
}

void ghost_in_range(int* x, int N) {
  for (int m = 8; m < N + 4; m++) {
    __ghost(assume, "P := in_range(m - 4, 4..N)");
    __ghost(assume, "P := in_range(m - 6, 2..(N - 2))");
    __ghost([&]() { __requires("in_range(m - 6, 2..(N - 2))"); });
    x += m - 6;
  }
}

void arrays(int N, int* w, int* r, int* f) {
  __modifies("f ~> Matrix1(N)");
  __writes("w ~> Matrix1(N)");
  __reads("r ~> Matrix1(N)");
  __ghost(group_shift,
          "start := 0, stop := N, step := 1, items := fun (i: int) -> "
          "&w[MINDEX1(N, i)] ~> UninitCell, shift := 2, new_start := 0 + 2, "
          "new_stop := N + 2");
  __ghost(group_shift,
          "start := 0, stop := N, step := 1, items := fun (i: int) -> "
          "&f[MINDEX1(N, i)] ~> Cell, shift := 2, new_start := 0 + 2, new_stop "
          ":= N + 2");
  __ghost([&]() {
    __consumes(
        "for __TMP_4 in (0 + 2)..(N + 2) -> &f[MINDEX1(N, __TMP_4 - 2)] ~> "
        "Cell");
    __produces(
        "for __TMP_4 in 2..(N + 2) -> &f[MINDEX1(N, __TMP_4 - 2)] ~> Cell");
    __admitted();
    __with("justif := arith_simpl");
  });
  __ghost([&]() {
    __consumes(
        "for __TMP_4 in (0 + 2)..(N + 2) -> &w[MINDEX1(N, __TMP_4 - 2)] ~> "
        "UninitCell");
    __produces(
        "for __TMP_4 in 2..(N + 2) -> &w[MINDEX1(N, __TMP_4 - 2)] ~> "
        "UninitCell");
    __admitted();
    __with("justif := arith_simpl");
  });
  for (int i = 2; i < N + 2; i++) {
    __strict();
    __xmodifies("&f[MINDEX1(N, i - 2)] ~> Cell");
    __xwrites("&w[MINDEX1(N, i - 2)] ~> Cell");
    __ghost(assume, "P := in_range(i - 2, 0..N)");
    w[MINDEX1(N, i - 2)] = i - 2;
    f[MINDEX1(N, i - 2)] = i - 2 + f[MINDEX1(N, i - 2)];
  }
  __ghost([&]() {
    __consumes(
        "for __TMP_4 in 2..(N + 2) -> &f[MINDEX1(N, __TMP_4 - 2)] ~> Cell");
    __produces(
        "for __TMP_4 in (0 + 2)..(N + 2) -> &f[MINDEX1(N, __TMP_4 - 2)] ~> "
        "Cell");
    __admitted();
    __with("justif := arith_simpl");
  });
  __ghost([&]() {
    __consumes(
        "for __TMP_4 in 2..(N + 2) -> &w[MINDEX1(N, __TMP_4 - 2)] ~> Cell");
    __produces(
        "for __TMP_4 in (0 + 2)..(N + 2) -> &w[MINDEX1(N, __TMP_4 - 2)] ~> "
        "Cell");
    __admitted();
    __with("justif := arith_simpl");
  });
  __ghost(group_unshift,
          "start := 0, stop := N, step := 1, items := fun (i: int) -> "
          "&w[MINDEX1(N, i)] ~> Cell, shift := 2, new_start := 0 + 2, new_stop "
          ":= N + 2");
  __ghost(group_unshift,
          "start := 0, stop := N, step := 1, items := fun (i: int) -> "
          "&f[MINDEX1(N, i)] ~> Cell, shift := 2, new_start := 0 + 2, new_stop "
          ":= N + 2");
}
