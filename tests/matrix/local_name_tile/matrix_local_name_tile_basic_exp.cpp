#include <optitrust.h>

typedef int T;

void malloc_uninit_pre() {
  __pure();
  T* const a = (T* const)MALLOC3(10, 10, 4, sizeof(T));
  const __ghost_fn focus = __ghost_begin(
      group2_focus_subrange_uninit,
      "items := fun i -> fun j -> for k in 0..4 -> &a[MINDEX3(10, 10, 4, i, j, "
      "k)] ~> Cell, sub_range := 2..10");
  T* const x = (T* const)MALLOC3(10 - 0, 10 - 2, 4 - 0, sizeof(T));
  __ghost(
      [&]() {
        __consumes("_Uninit(x ~> Matrix3(10 - 0, 10 - 2, 4 - 0))");
        __produces(
            "_Uninit(for i1 in 0..10 -> for i2 in 2..10 -> for i3 in 0..4 -> "
            "&x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i1 - 0, i2 - 2, i3 - 0)] ~> "
            "Cell)");
        __admitted();
        __with("justif := shift_groups");
      },
      "");
  for (int i = 0; i < 10; i++) {
    __strict();
    __writes(
        "for j in 2..10 -> for k in 0..4 -> &x[MINDEX3(10 - 0, 10 - 2, 4 - 0, "
        "i - 0, j - 2, k - 0)] ~> Cell");
    for (int j = 2; j < 10; j++) {
      __strict();
      __writes(
          "for k in 0..4 -> &x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i - 0, j - 2, k "
          "- 0)] ~> Cell");
      for (int k = 0; k < 4; k++) {
        __strict();
        __writes(
            "&x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i - 0, j - 2, k - 0)] ~> Cell");
        x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i - 0, j - 2, k - 0)] = 1;
      }
    }
  }
  for (int i1 = 0; i1 < 10; i1++) {
    __strict();
    __writes(
        "for i2 in 2..10 -> for i3 in 0..4 -> &a[MINDEX3(10, 10, 4, i1, i2, "
        "i3)] ~> Cell");
    __reads(
        "for i2 in 2..10 -> for i3 in 0..4 -> &x[MINDEX3(10 - 0, 10 - 2, 4 - "
        "0, i1 - 0, i2 - 2, i3 - 0)] ~> Cell");
    for (int i2 = 2; i2 < 10; i2++) {
      __strict();
      __writes("for i3 in 0..4 -> &a[MINDEX3(10, 10, 4, i1, i2, i3)] ~> Cell");
      __reads(
          "for i3 in 0..4 -> &x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i1 - 0, i2 - 2, "
          "i3 - 0)] ~> Cell");
      for (int i3 = 0; i3 < 4; i3++) {
        __strict();
        __writes("&a[MINDEX3(10, 10, 4, i1, i2, i3)] ~> Cell");
        __reads(
            "&x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i1 - 0, i2 - 2, i3 - 0)] ~> "
            "Cell");
        a[MINDEX3(10, 10, 4, i1, i2, i3)] =
            x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i1 - 0, i2 - 2, i3 - 0)];
      }
    }
  }
  __ghost(
      [&]() {
        __consumes(
            "for i1 in 0..10 -> for i2 in 2..10 -> for i3 in 0..4 -> "
            "&x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i1 - 0, i2 - 2, i3 - 0)] ~> "
            "Cell");
        __produces("_Uninit(x ~> Matrix3(10 - 0, 10 - 2, 4 - 0))");
        __admitted();
        __with("justif := shift_groups");
      },
      "");
  MFREE3(10 - 0, 10 - 2, 4 - 0, x);
  __ghost_end(focus);
  MFREE3(10, 10, 4, a);
  int z = 0;
}

void malloc_uninit_post() {
  __pure();
  T* const a = (T* const)MALLOC1(10, sizeof(T));
  const __ghost_fn focus = __ghost_begin(
      group_focus_subrange_uninit,
      "items := fun i -> &a[MINDEX1(10, i)] ~> Cell, sub_range := 2..10");
  for (int i1 = 2; i1 < 10; i1++) {
    __strict();
    __writes("&a[MINDEX1(10, i1)] ~> Cell");
    a[MINDEX1(10, i1)] = 1;
  }
  T* const x = (T* const)MALLOC1(10 - 2, sizeof(T));
  __ghost(
      [&]() {
        __consumes("_Uninit(x ~> Matrix1(10 - 2))");
        __produces("for i1 in 2..10 -> &x[MINDEX1(10 - 2, i1 - 2)] ~> Cell");
        __admitted();
        __with("justif := shift_groups");
      },
      "");
  for (int i1 = 2; i1 < 10; i1++) {
    __strict();
    __writes("&x[MINDEX1(10 - 2, i1 - 2)] ~> Cell");
    __reads("&a[MINDEX1(10, i1)] ~> Cell");
    x[MINDEX1(10 - 2, i1 - 2)] = a[MINDEX1(10, i1)];
  }
  for (int i = 2; i < 10; i++) {
    __strict();
    __modifies("&x[MINDEX1(10 - 2, i - 2)] ~> Cell");
    x[MINDEX1(10 - 2, i - 2)] += 1;
  }
  __ghost(
      [&]() {
        __consumes(
            "_Uninit(for i1 in 2..10 -> &x[MINDEX1(10 - 2, i1 - 2)] ~> Cell)");
        __produces("_Uninit(x ~> Matrix1(10 - 2))");
        __admitted();
        __with("justif := shift_groups");
      },
      "");
  MFREE1(10 - 2, x);
  __ghost_end(focus);
  MFREE1(10, a);
}

void malloc_uninit_prepost() {
  __pure();
  T* const a = (T* const)MALLOC3(10, 10, 4, sizeof(T));
  const __ghost_fn focus = __ghost_begin(
      group2_focus_subrange_uninit,
      "items := fun i -> fun j -> for k in 0..4 -> &a[MINDEX3(10, 10, 4, i, j, "
      "k)] ~> Cell, sub_range := 2..10");
  T* const x = (T* const)MALLOC3(10 - 0, 10 - 2, 4 - 0, sizeof(T));
  __ghost(
      [&]() {
        __consumes("_Uninit(x ~> Matrix3(10 - 0, 10 - 2, 4 - 0))");
        __produces(
            "_Uninit(for i1 in 0..10 -> for i2 in 2..10 -> for i3 in 0..4 -> "
            "&x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i1 - 0, i2 - 2, i3 - 0)] ~> "
            "Cell)");
        __admitted();
        __with("justif := shift_groups");
      },
      "");
  for (int i = 0; i < 10; i++) {
    __strict();
    __writes(
        "for j in 2..10 -> for k in 0..4 -> &x[MINDEX3(10 - 0, 10 - 2, 4 - 0, "
        "i - 0, j - 2, k - 0)] ~> Cell");
    for (int j = 2; j < 10; j++) {
      __strict();
      __writes(
          "for k in 0..4 -> &x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i - 0, j - 2, k "
          "- 0)] ~> Cell");
      for (int k = 0; k < 4; k++) {
        __strict();
        __writes(
            "&x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i - 0, j - 2, k - 0)] ~> Cell");
        x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i - 0, j - 2, k - 0)] = 1;
      }
    }
  }
  __ghost(
      [&]() {
        __consumes(
            "_Uninit(for i1 in 0..10 -> for i2 in 2..10 -> for i3 in 0..4 -> "
            "&x[MINDEX3(10 - 0, 10 - 2, 4 - 0, i1 - 0, i2 - 2, i3 - 0)] ~> "
            "Cell)");
        __produces("_Uninit(x ~> Matrix3(10 - 0, 10 - 2, 4 - 0))");
        __admitted();
        __with("justif := shift_groups");
      },
      "");
  MFREE3(10 - 0, 10 - 2, 4 - 0, x);
  __ghost_end(focus);
  MFREE3(10, 10, 4, a);
}
