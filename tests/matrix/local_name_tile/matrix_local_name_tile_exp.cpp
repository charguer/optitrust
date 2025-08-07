#include <optitrust.h>

void f() {
  __pure();
  int* const a = (int*)malloc(MSIZE3(10, 10, 4) * sizeof(int));
  const __ghost_fn focusa = __ghost_begin(
      group2_focus_subrange,
      "items := fun i -> fun j -> for k in 0..4 -> &a[MINDEX3(10, 10, 4, i, j, "
      "k)] ~> UninitCell, sub_range := 2..10");
  for (int i = 0; i < 10; i++) {
    __strict();
    __xwrites(
        "for j in 2..10 -> for k in 0..4 -> &a[MINDEX3(10, 10, 4, i, j, k)] ~> "
        "Cell");
    for (int j = 2; j < 10; j++) {
      __strict();
      __xwrites("for k in 0..4 -> &a[MINDEX3(10, 10, 4, i, j, k)] ~> Cell");
      for (int k = 0; k < 4; k++) {
        __strict();
        __xwrites("&a[MINDEX3(10, 10, 4, i, j, k)] ~> Cell");
        a[MINDEX3(10, 10, 4, i, j, k)] = 1;
      }
    }
  }
  int* const y_local = (int*)malloc(MSIZE3(10, 8, 4) * sizeof(int));
  __ghost([&]() {
    __consumes("y_local ~> UninitMatrix3(10, 8, 4)");
    __produces(
        "for i1 in 0..10 -> for i2 in 2..10 -> for i3 in 0..4 -> "
        "&y_local[MINDEX3(10, 8, 4, i1 - 0, i2 - 2, i3 - 0)] ~> UninitCell");
    __admitted();
    __with("justif := shift_groups");
  });
  for (int i1 = 0; i1 < 10; i1++) {
    __strict();
    __xwrites(
        "for i2 in 2..10 -> for i3 in 0..4 -> &y_local[MINDEX3(10, 8, 4, i1 - "
        "0, i2 - 2, i3 - 0)] ~> Cell");
    __xreads(
        "for i2 in 2..10 -> for i3 in 0..4 -> &a[MINDEX3(10, 10, 4, i1, i2, "
        "i3)] ~> Cell");
    for (int i2 = 2; i2 < 10; i2++) {
      __strict();
      __xwrites(
          "for i3 in 0..4 -> &y_local[MINDEX3(10, 8, 4, i1 - 0, i2 - 2, i3 - "
          "0)] ~> Cell");
      __xreads("for i3 in 0..4 -> &a[MINDEX3(10, 10, 4, i1, i2, i3)] ~> Cell");
      for (int i3 = 0; i3 < 4; i3++) {
        __strict();
        __xwrites(
            "&y_local[MINDEX3(10, 8, 4, i1 - 0, i2 - 2, i3 - 0)] ~> Cell");
        __xreads("&a[MINDEX3(10, 10, 4, i1, i2, i3)] ~> Cell");
        y_local[MINDEX3(10, 8, 4, i1 - 0, i2 - 2, i3 - 0)] =
            a[MINDEX3(10, 10, 4, i1, i2, i3)];
      }
    }
  }
  for (int i = 0; i < 10; i++) {
    __strict();
    __xmodifies(
        "for j in 2..10 -> for k in 0..4 -> &y_local[MINDEX3(10, 8, 4, i, j - "
        "2, k)] ~> Cell");
    for (int j = 2; j < 10; j++) {
      __strict();
      __xmodifies(
          "for k in 0..4 -> &y_local[MINDEX3(10, 8, 4, i, j - 2, k)] ~> Cell");
      for (int k = 0; k < 4; k++) {
        __strict();
        __xmodifies("&y_local[MINDEX3(10, 8, 4, i, j - 2, k)] ~> Cell");
        y_local[MINDEX3(10, 8, 4, i, j - 2, k)] += 1;
      }
    }
  }
  for (int i1 = 0; i1 < 10; i1++) {
    __strict();
    __xwrites(
        "for i2 in 2..10 -> for i3 in 0..4 -> &a[MINDEX3(10, 10, 4, i1, i2, "
        "i3)] ~> Cell");
    __xreads(
        "for i2 in 2..10 -> for i3 in 0..4 -> &y_local[MINDEX3(10, 8, 4, i1 - "
        "0, i2 - 2, i3 - 0)] ~> Cell");
    for (int i2 = 2; i2 < 10; i2++) {
      __strict();
      __xwrites("for i3 in 0..4 -> &a[MINDEX3(10, 10, 4, i1, i2, i3)] ~> Cell");
      __xreads(
          "for i3 in 0..4 -> &y_local[MINDEX3(10, 8, 4, i1 - 0, i2 - 2, i3 - "
          "0)] ~> Cell");
      for (int i3 = 0; i3 < 4; i3++) {
        __strict();
        __xwrites("&a[MINDEX3(10, 10, 4, i1, i2, i3)] ~> Cell");
        __xreads("&y_local[MINDEX3(10, 8, 4, i1 - 0, i2 - 2, i3 - 0)] ~> Cell");
        a[MINDEX3(10, 10, 4, i1, i2, i3)] =
            y_local[MINDEX3(10, 8, 4, i1 - 0, i2 - 2, i3 - 0)];
      }
    }
  }
  __ghost([&]() {
    __consumes(
        "for i1 in 0..10 -> for i2 in 2..10 -> for i3 in 0..4 -> "
        "&y_local[MINDEX3(10, 8, 4, i1 - 0, i2 - 2, i3 - 0)] ~> UninitCell");
    __produces("y_local ~> UninitMatrix3(10, 8, 4)");
    __admitted();
    __with("justif := shift_groups");
  });
  free(y_local);
  __ghost_end(focusa);
  free(a);
  int* const b = (int*)malloc(MSIZE3(10, 10, 4) * sizeof(int));
  const __ghost_fn focusb = __ghost_begin(
      group2_focus_subrange,
      "items := fun i -> fun j -> for k in 0..4 -> &b[MINDEX3(10, 10, 4, i, j, "
      "k)] ~> UninitCell, sub_range := 2..10");
  for (int i = 0; i < 10; i++) {
    __strict();
    __xwrites(
        "for j in 2..10 -> for k in 0..4 -> &b[MINDEX3(10, 10, 4, i, j, k)] ~> "
        "Cell");
    for (int j = 2; j < 10; j++) {
      __strict();
      __xwrites("for k in 0..4 -> &b[MINDEX3(10, 10, 4, i, j, k)] ~> Cell");
      for (int k = 0; k < 4; k++) {
        __strict();
        __xwrites("&b[MINDEX3(10, 10, 4, i, j, k)] ~> Cell");
        b[MINDEX3(10, 10, 4, i, j, k)] = 1;
      }
    }
  }
  __ghost_end(focusb);
  free(b);
  int* const c = (int*)malloc(MSIZE3(10, 10, 4) * sizeof(int));
  const __ghost_fn focusc = __ghost_begin(
      group2_focus_subrange,
      "items := fun i -> fun j -> for k in 0..4 -> &c[MINDEX3(10, 10, 4, i, j, "
      "k)] ~> UninitCell, sub_range := 2..10");
  for (int i = 0; i < 10; i++) {
    __strict();
    __xwrites(
        "for j in 2..10 -> for k in 0..4 -> &c[MINDEX3(10, 10, 4, i, j, k)] ~> "
        "Cell");
    for (int j = 2; j < 10; j++) {
      __strict();
      __xwrites("for k in 0..4 -> &c[MINDEX3(10, 10, 4, i, j, k)] ~> Cell");
      for (int k = 0; k < 4; k++) {
        __strict();
        __xwrites("&c[MINDEX3(10, 10, 4, i, j, k)] ~> Cell");
        c[MINDEX3(10, 10, 4, i, j, k)] = 1;
      }
    }
    for (int j2 = 2; j2 < 10; j2++) {
      __strict();
      __xwrites("for k in 0..4 -> &c[MINDEX3(10, 10, 4, i, j2, k)] ~> Cell");
      for (int k2 = 0; k2 < 4; k2++) {
        __strict();
        __xwrites("&c[MINDEX3(10, 10, 4, i, j2, k2)] ~> Cell");
        c[MINDEX3(10, 10, 4, i, j2, k2)] = 2;
      }
    }
  }
  __ghost_end(focusc);
  free(c);
  int z = 0;
}
