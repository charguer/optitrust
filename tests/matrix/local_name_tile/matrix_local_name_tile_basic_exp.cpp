#include <optitrust.h>

void malloc_uninit_pre() {
  __pure();
  int* const a = (int*)malloc(MSIZE3(10, 10, 4) * sizeof(int));
  const __ghost_fn focus = __ghost_begin(
      group2_focus_subrange,
      "items := fun i -> fun j -> for k in 0..4 -> &a[MINDEX3(10, 10, 4, i, j, "
      "k)] ~> UninitCell, sub_range := 2..10");
  int* const x = (int*)malloc(MSIZE3(10 - 0, 10 - 2, 4 - 0) * sizeof(int));
  __ghost([&]() {
    __consumes("x ~> UninitMatrix3(10 - 0, 10 - 2, 4 - 0)");
    __produces(
        "for i1 in 0..10 -> for i2 in 2..10 -> for i3 in 0..4 -> &x[MINDEX3(10 "
        "- 0, 10 - 2, 4 - 0, i1 - 0, i2 - 2, i3 - 0)] ~> UninitCell");
    __admitted();
    __with("justif := shift_groups");
  });
  for (int i = 0; i < 10; i++) {
    __strict();
    __xwrites(
        "for j in 2..10 -> for k in 0..4 -> &x[MINDEX3(10 - 0, 10 - 2, 4 - 0, "
        "i - 0, j - 2, k - 0)] ~> Cell");
    for (int j = 2; j < 10; j++) {
      __strict();
      __xwrites(
          "for k in 0..4 -> &x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i - 0, j - 2, k "
          "- 0)] ~> Cell");
      for (int k = 0; k < 4; k++) {
        __strict();
        __xwrites(
            "&x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i - 0, j - 2, k - 0)] ~> Cell");
        x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i - 0, j - 2, k - 0)] = 1;
      }
    }
  }
  for (int i1 = 0; i1 < 10; i1++) {
    __strict();
    __xwrites(
        "for i2 in 2..10 -> for i3 in 0..4 -> &a[MINDEX3(10, 10, 4, i1, i2, "
        "i3)] ~> Cell");
    __xreads(
        "for i2 in 2..10 -> for i3 in 0..4 -> &x[MINDEX3(10 - 0, 10 - 2, 4 - "
        "0, i1 - 0, i2 - 2, i3 - 0)] ~> Cell");
    for (int i2 = 2; i2 < 10; i2++) {
      __strict();
      __xwrites("for i3 in 0..4 -> &a[MINDEX3(10, 10, 4, i1, i2, i3)] ~> Cell");
      __xreads(
          "for i3 in 0..4 -> &x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i1 - 0, i2 - 2, "
          "i3 - 0)] ~> Cell");
      for (int i3 = 0; i3 < 4; i3++) {
        __strict();
        __xwrites("&a[MINDEX3(10, 10, 4, i1, i2, i3)] ~> Cell");
        __xreads(
            "&x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i1 - 0, i2 - 2, i3 - 0)] ~> "
            "Cell");
        a[MINDEX3(10, 10, 4, i1, i2, i3)] =
            x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i1 - 0, i2 - 2, i3 - 0)];
      }
    }
  }
  __ghost([&]() {
    __consumes(
        "for i1 in 0..10 -> for i2 in 2..10 -> for i3 in 0..4 -> &x[MINDEX3(10 "
        "- 0, 10 - 2, 4 - 0, i1 - 0, i2 - 2, i3 - 0)] ~> UninitCell");
    __produces("x ~> UninitMatrix3(10 - 0, 10 - 2, 4 - 0)");
    __admitted();
    __with("justif := shift_groups");
  });
  free(x);
  __ghost_end(focus);
  free(a);
  int z = 0;
}

void malloc_uninit_post() {
  __pure();
  int* const a = (int*)malloc(MSIZE1(10) * sizeof(int));
  const __ghost_fn focus = __ghost_begin(
      group_focus_subrange,
      "items := fun i -> &a[MINDEX1(10, i)] ~> UninitCell, sub_range := 2..10");
  for (int i1 = 2; i1 < 10; i1++) {
    __strict();
    __xwrites("&a[MINDEX1(10, i1)] ~> Cell");
    a[MINDEX1(10, i1)] = 1;
  }
  int* const x = (int*)malloc(MSIZE1(10 - 2) * sizeof(int));
  __ghost([&]() {
    __consumes("x ~> UninitMatrix1(10 - 2)");
    __produces("for i1 in 2..10 -> &x[MINDEX1(10 - 2, i1 - 2)] ~> UninitCell");
    __admitted();
    __with("justif := shift_groups");
  });
  for (int i1 = 2; i1 < 10; i1++) {
    __strict();
    __xwrites("&x[MINDEX1(10 - 2, i1 - 2)] ~> Cell");
    __xreads("&a[MINDEX1(10, i1)] ~> Cell");
    x[MINDEX1(10 - 2, i1 - 2)] = a[MINDEX1(10, i1)];
  }
  for (int i = 2; i < 10; i++) {
    __strict();
    __xmodifies("&x[MINDEX1(10 - 2, i - 2)] ~> Cell");
    x[MINDEX1(10 - 2, i - 2)] += 1;
  }
  __ghost([&]() {
    __consumes("for i1 in 2..10 -> &x[MINDEX1(10 - 2, i1 - 2)] ~> UninitCell");
    __produces("x ~> UninitMatrix1(10 - 2)");
    __admitted();
    __with("justif := shift_groups");
  });
  free(x);
  __ghost_end(focus);
  free(a);
}

void malloc_uninit_prepost() {
  __pure();
  int* const a = (int*)malloc(MSIZE3(10, 10, 4) * sizeof(int));
  const __ghost_fn focus = __ghost_begin(
      group2_focus_subrange,
      "items := fun i -> fun j -> for k in 0..4 -> &a[MINDEX3(10, 10, 4, i, j, "
      "k)] ~> UninitCell, sub_range := 2..10");
  int* const x = (int*)malloc(MSIZE3(10 - 0, 10 - 2, 4 - 0) * sizeof(int));
  __ghost([&]() {
    __consumes("x ~> UninitMatrix3(10 - 0, 10 - 2, 4 - 0)");
    __produces(
        "for i1 in 0..10 -> for i2 in 2..10 -> for i3 in 0..4 -> &x[MINDEX3(10 "
        "- 0, 10 - 2, 4 - 0, i1 - 0, i2 - 2, i3 - 0)] ~> UninitCell");
    __admitted();
    __with("justif := shift_groups");
  });
  for (int i = 0; i < 10; i++) {
    __strict();
    __xwrites(
        "for j in 2..10 -> for k in 0..4 -> &x[MINDEX3(10 - 0, 10 - 2, 4 - 0, "
        "i - 0, j - 2, k - 0)] ~> Cell");
    for (int j = 2; j < 10; j++) {
      __strict();
      __xwrites(
          "for k in 0..4 -> &x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i - 0, j - 2, k "
          "- 0)] ~> Cell");
      for (int k = 0; k < 4; k++) {
        __strict();
        __xwrites(
            "&x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i - 0, j - 2, k - 0)] ~> Cell");
        x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i - 0, j - 2, k - 0)] = 1;
      }
    }
  }
  __ghost([&]() {
    __consumes(
        "for i1 in 0..10 -> for i2 in 2..10 -> for i3 in 0..4 -> &x[MINDEX3(10 "
        "- 0, 10 - 2, 4 - 0, i1 - 0, i2 - 2, i3 - 0)] ~> UninitCell");
    __produces("x ~> UninitMatrix3(10 - 0, 10 - 2, 4 - 0)");
    __admitted();
    __with("justif := shift_groups");
  });
  free(x);
  __ghost_end(focus);
  free(a);
}
