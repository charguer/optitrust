#include <optitrust.h>

void seq_array() {
  __pure();
  int x = 0;
  const int st = 0;
  const int N = 10;
  for (int i2 = 0 + 2; i2 < 10 + 2; i2++) {
    __strict();
    __smodifies("&x ~> Cell");
    const int i = i2 - 2;
    __ghost(assume, "P := in_range(i, 0..10)");
    x += i;
  }
  for (int j2 = 0; j2 < st + N - st; j2++) {
    __strict();
    __smodifies("&x ~> Cell");
    const int j = j2 - -st;
    __ghost(assume, "P := in_range(j, st..(st + N))");
    x += j;
  }
  int shift = 5;
  for (int k = 0; k < N; k++) {
    __strict();
    __smodifies("&x ~> Cell");
    x += k;
  }
  for (int m3 = 4; m3 < N - 2 + (4 - 2); m3++) {
    __strict();
    __smodifies("&x ~> Cell");
    const int m = m3 - (4 - 2);
    __ghost(assume, "P := in_range(m, 2..(N - 2))");
    x += m;
  }
}

void excl_array(int* t, int n) {
  __modifies("t ~> Matrix1(n)");
  __ghost(group_shift,
          "start := 0, stop := n, step := 1, items := fun (i: int) -> "
          "&t[MINDEX1(n, i)] ~> Cell, shift := 2, new_start := 0 + 2, new_stop "
          ":= n + 2");
  for (int j = 0 + 2; j < n + 2; j++) {
    __strict();
    __xmodifies("&t[MINDEX1(n, j - 2)] ~> Cell");
    const int i = j - 2;
    __ghost(assume, "P := in_range(i, 0..n)");
    t[MINDEX1(n, i)] += i;
  }
  __ghost(group_unshift,
          "start := 0, stop := n, step := 1, items := fun (i: int) -> "
          "&t[MINDEX1(n, i)] ~> Cell, shift := 2, new_start := 0 + 2, new_stop "
          ":= n + 2");
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
  for (int i2 = 0 + 2; i2 < n + 2; i2++) {
    __strict();
    __smodifies("for i in 0..(i2 - 2) -> &t[MINDEX1(n, i)] ~> Cell");
    __smodifies("for i in (i2 - 2)..n -> &t[MINDEX1(n, i)] ~> UninitCell");
    const int i = i2 - 2;
    __ghost(assume, "P := in_range(i, 0..n)");
    __ghost(assume, "P := is_subrange(i..(i + 1), i..n)");
    __ghost(
        group_split,
        "split := i + 1, items := fun i -> &t[MINDEX1(n, i)] ~> UninitCell");
    for (int j = i; j < i + 1; j++) {
      __strict();
      __xwrites("&t[MINDEX1(n, j)] ~> Cell");
      t[MINDEX1(n, j)] = j;
    }
    __ghost(group_join,
            "split := i, items := fun i -> &t[MINDEX1(n, i)] ~> Cell");
    __ghost([&]() {
      __consumes("for i in 0..(i2 - 2 + 1) -> &t[MINDEX1(n, i)] ~> Cell");
      __consumes("for i in (i2 - 2 + 1)..n -> &t[MINDEX1(n, i)] ~> UninitCell");
      __produces("for i in 0..(i2 + 1 - 2) -> &t[MINDEX1(n, i)] ~> Cell");
      __produces("for i in (i2 + 1 - 2)..n -> &t[MINDEX1(n, i)] ~> UninitCell");
      __admitted();
    });
  }
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
