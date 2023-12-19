#include <optitrust.h>

typedef int T;

void f(T* b) {
  __modifies("b ~> Matrix3(10, 10, 4)");
  T* const a = (T* const)MALLOC3(10, 10, 4, sizeof(T));
const __ghost_fn focus =
      __ghost_begin(group2_focus_subrange_uninit,
                    "items := fun i -> fun j -> Group(range(0, 4, 1), fun k -> "
                    "&a[MINDEX3(10, 10, 4, i, j, k)] ~> Cell), start := 2, "
                    "stop := 10, step := 1");
  T* const x = (T* const)MALLOC3(10, 8, 4, sizeof(T));
  __ghost(rewrite,
          "H1 := _Uninit(x ~> Matrix3(10, 8, 4)), H2 := _Uninit(Group(range(0, "
          "10, 1), fun i1 -> Group(range(2, 10, 1), fun i2 -> Group(range(0, "
          "4, 1), fun i3 -> &x[MINDEX3(10, 8, 4, i1, i2 - 2, i3)] ~> Cell)))), "
          "by := shift_groups");
  for (int i = 0; i < 10; i++) {
__consumes(
        "_Uninit(Group(range(2, 10, 1), fun j -> Group(range(0, 4, 1), fun k "
        "-> &x[MINDEX3(10, 8, 4, i, j - 2, k)] ~> Cell)))");
    __produces(
        "Group(range(2, 10, 1), fun j -> Group(range(0, 4, 1), fun k -> "
        "&x[MINDEX3(10, 8, 4, i, j - 2, k)] ~> Cell))");
    for (int j = 2; j < 10; j++) {
__consumes(
          "_Uninit(Group(range(0, 4, 1), fun k -> &x[MINDEX3(10, 8, 4, i, j - "
          "2, k)] ~> Cell))");
      __produces(
          "Group(range(0, 4, 1), fun k -> &x[MINDEX3(10, 8, 4, i, j - 2, k)] "
          "~> Cell)");
      for (int k = 0; k < 4; k++) {
        __consumes("_Uninit(&x[MINDEX3(10, 8, 4, i, j - 2, k)] ~> Cell)");
        __produces("&x[MINDEX3(10, 8, 4, i, j - 2, k)] ~> Cell");
        x[MINDEX3(10, 8, 4, i, j - 2, k)] = 1;
      }
    }
  }
  for (int i1 = 0; i1 < 10; i1++) {
    __consumes(
        "_Uninit(Group(range(2, 10, 1), fun i2 -> Group(range(0, 4, 1), fun i3 "
        "-> &a[MINDEX3(10, 10, 4, i1, i2, i3)] ~> Cell)))");
    __produces(
        "Group(range(2, 10, 1), fun i2 -> Group(range(0, 4, 1), fun i3 -> "
        "&a[MINDEX3(10, 10, 4, i1, i2, i3)] ~> Cell))");
    __reads(
        "Group(range(2, 10, 1), fun i2 -> Group(range(0, 4, 1), fun i3 -> "
        "&x[MINDEX3(10, 8, 4, i1, i2 - 2, i3)] ~> Cell))");
    for (int i2 = 2; i2 < 10; i2++) {
      __consumes(
          "_Uninit(Group(range(0, 4, 1), fun i3 -> &a[MINDEX3(10, 10, 4, i1, "
          "i2, i3)] ~> Cell))");
      __produces(
          "Group(range(0, 4, 1), fun i3 -> &a[MINDEX3(10, 10, 4, i1, i2, i3)] "
          "~> Cell)");
      __reads(
          "Group(range(0, 4, 1), fun i3 -> &x[MINDEX3(10, 8, 4, i1, i2 - 2, "
          "i3)] ~> Cell)");
      for (int i3 = 0; i3 < 4; i3++) {
        __consumes("_Uninit(&a[MINDEX3(10, 10, 4, i1, i2, i3)] ~> Cell)");
        __produces("&a[MINDEX3(10, 10, 4, i1, i2, i3)] ~> Cell");
        __reads("&x[MINDEX3(10, 8, 4, i1, i2 - 2, i3)] ~> Cell");
        a[MINDEX3(10, 10, 4, i1, i2, i3)] =
            x[MINDEX3(10, 8, 4, i1, i2 - 2, i3)];
      }
    }
  }
  __ghost(
      rewrite,
      "H1 := Group(range(0, 10, 1), fun i1 -> Group(range(2, 10, 1), fun i2 -> "
      "Group(range(0, 4, 1), fun i3 -> &x[MINDEX3(10, 8, 4, i1, i2 - 2, i3)] "
      "~> Cell))), H2 := _Uninit(x ~> Matrix3(10, 8, 4)), by := shift_groups");
  MFREE3(10, 8, 4, x);
  __ghost_end(focus);
  MFREE3(10, 10, 4, a);
    int z = 0;
  }
