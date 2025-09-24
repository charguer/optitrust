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

void non_transparent_ghosts(int* t, int n) {
  __writes("t ~> Matrix1(n)");
  __ghost(group_intro_zero, "items := fun i -> &t[MINDEX1(n, i)] ~> Cell");
  __ghost([&]() {
    __consumes("for i in 0..0 -> &t[MINDEX1(n, i)] ~> Cell");
    __consumes("t ~> UninitMatrix1(n)");
    __produces("for i in 0..(0 + 2 - 2) -> &t[MINDEX1(n, i)] ~> Cell");
    __produces("for i in (0 + 2 - 2)..n -> &t[MINDEX1(n, i)] ~> UninitCell");
    __admitted();
  });
  __ghost([&]() {
    __consumes("for i in 0..(0 + 2 - 2) -> &t[MINDEX1(n, i)] ~> Cell");
    __consumes("for i in (0 + 2 - 2)..n -> &t[MINDEX1(n, i)] ~> UninitCell");
    __produces("for i in 0..(2 - 2) -> &t[MINDEX1(n, i)] ~> Cell");
    __produces("for i in (2 - 2)..n -> &t[MINDEX1(n, i)] ~> UninitCell");
    __admitted();
  });
  for (int i = 2; i < n + 2; i++) {
    __strict();
    __smodifies("for i in 0..(i - 2) -> &t[MINDEX1(n, i)] ~> Cell");
    __smodifies("for i in (i - 2)..n -> &t[MINDEX1(n, i)] ~> UninitCell");
    __ghost(assume, "P := in_range(i - 2, 0..n)");
    __ghost(assume, "P := is_subrange((i - 2)..(i - 1), (i - 2)..n)");
    __ghost(
        group_split,
        "split := i - 1, items := fun i -> &t[MINDEX1(n, i)] ~> UninitCell");
    for (int j = i - 2; j < i - 1; j++) {
      __strict();
      __xwrites("&t[MINDEX1(n, j)] ~> Cell");
      t[MINDEX1(n, j)] = j;
    }
    __ghost(group_join,
            "split := i - 2, items := fun i -> &t[MINDEX1(n, i)] ~> Cell");
    __ghost([&]() {
      __consumes("for i in 0..(i - 1) -> &t[MINDEX1(n, i)] ~> Cell");
      __consumes("for i in (i - 1)..n -> &t[MINDEX1(n, i)] ~> UninitCell");
      __produces("for i in 0..(i + 1 - 2) -> &t[MINDEX1(n, i)] ~> Cell");
      __produces("for i in (i + 1 - 2)..n -> &t[MINDEX1(n, i)] ~> UninitCell");
      __admitted();
    });
  }
  __ghost([&]() {
    __modifies("for i in 0..(n + 2 - 2) -> &t[MINDEX1(n, i)] ~> Cell");
    __modifies("for i in (n + 2 - 2)..n -> &t[MINDEX1(n, i)] ~> UninitCell");
    __admitted();
  });
  __ghost([&]() {
    __consumes("for i in 0..(n + 2 - 2) -> &t[MINDEX1(n, i)] ~> Cell");
    __consumes("for i in (n + 2 - 2)..n -> &t[MINDEX1(n, i)] ~> UninitCell");
    __produces("t ~> Matrix1(n)");
    __produces("for i in n..n -> &t[MINDEX1(n, i)] ~> UninitCell");
    __admitted();
  });
  __ghost(group_shift,
          "start := n, stop := n, shift := - n, new_start := 0, new_stop := 0, "
          "items := fun i -> &t[MINDEX1(n, i)] ~> UninitCell");
  __ghost(group_elim_zero,
          "items := fun i -> &t[MINDEX1(n, i - - n)] ~> UninitCell");
}
