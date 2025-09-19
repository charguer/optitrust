#include <optitrust.h>

void f() {
  __pure();
  int x = 0;
  const int st = 0;
  const int N = 10;
  __ghost(to_prove, "P := is_subrange(0..(0 + 5), 0..10)");
  for (int i = 0; i < 0 + 5; i++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(i, 0..10)");
    x += i;
  }
  for (int i = 0 + 5; i < 10; i++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(i, 0..10)");
    x += i;
  }
  __ghost(to_prove, "P := is_subrange(st..(st + 5), st..N)");
  for (int j = st; j < st + 5; j++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(j, st..N)");
    x += j;
  }
  for (int j = st + 5; j < N; j++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(j, st..N)");
    x += j;
  }
  const int cut = 5;
  __ghost(to_prove, "P := is_subrange(0..cut, 0..N)");
  for (int k = 0; k < cut; k++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(k, 0..N)");
    x += k;
  }
  for (int k = cut; k < N; k++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(k, 0..N)");
    x += k;
  }
  __ghost(to_prove, "P := is_subrange(st..cut, st..N)");
  for (int l = st; l < cut; l++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(l, st..N)");
    x += l;
  }
  for (int l = cut; l < N; l++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(l, st..N)");
    x += l;
  }
}

void array_copy(int* a, int* b, int n) {
  __requires("__is_true(n >= 0)");
  __writes("a ~> Matrix1(n)");
  __reads("b ~> Matrix1(n)");
  __ghost(to_prove, "P := is_subrange(0..0, 0..n)");
  __ghost(group_split,
          "start := 0, stop := n, step := 1, split := 0, items := fun (i: int) "
          "-> &a[MINDEX1(n, i)] ~> UninitCell");
  for (int i = 0; i < 0; i++) {
    __strict();
    __sreads("b ~> Matrix1(n)");
    __xwrites("&a[MINDEX1(n, i)] ~> Cell");
    __ghost(assume, "P := in_range(i, 0..n)");
    const __ghost_fn focus =
        __ghost_begin(ro_matrix1_focus, "matrix := b, i := i");
    a[MINDEX1(n, i)] = b[MINDEX1(n, i)];
    __ghost_end(focus);
  }
  for (int i = 0; i < n; i++) {
    __strict();
    __sreads("b ~> Matrix1(n)");
    __xwrites("&a[MINDEX1(n, i)] ~> Cell");
    __ghost(assume, "P := in_range(i, 0..n)");
    const __ghost_fn focus =
        __ghost_begin(ro_matrix1_focus, "matrix := b, i := i");
    a[MINDEX1(n, i)] = b[MINDEX1(n, i)];
    __ghost_end(focus);
  }
  __ghost(group_join,
          "start := 0, stop := n, step := 1, split := 0, items := fun (i: int) "
          "-> &a[MINDEX1(n, i)] ~> Cell");
}

void non_transparent_ghosts(int* t, int n) {
  __requires("__is_true(n >= 0)");
  __writes("t ~> Matrix1(n)");
  const int cut = 0;
  __ghost(group_intro_zero, "items := fun i -> &t[MINDEX1(n, i)] ~> Cell");
  __ghost(to_prove, "P := is_subrange(0..cut, 0..n)");
  for (int i = 0; i < cut; i++) {
    __strict();
    __smodifies("for i in 0..i -> &t[MINDEX1(n, i)] ~> Cell");
    __smodifies("for i in i..n -> &t[MINDEX1(n, i)] ~> UninitCell");
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
  }
  for (int i = cut; i < n; i++) {
    __strict();
    __smodifies("for i in 0..i -> &t[MINDEX1(n, i)] ~> Cell");
    __smodifies("for i in i..n -> &t[MINDEX1(n, i)] ~> UninitCell");
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
  }
  __ghost(group_shift,
          "start := n, stop := n, shift := - n, new_start := 0, new_stop := 0, "
          "items := fun i -> &t[MINDEX1(n, i)] ~> UninitCell");
  __ghost(group_elim_zero,
          "items := fun i -> &t[MINDEX1(n, i - - n)] ~> UninitCell");
}
