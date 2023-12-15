#include <optitrust.h>

typedef int T;

void f(T* b) {
  __modifies("b ~> Matrix3(10, 10, 4)");

  // FIXME: CALLOC
  T* const a = (T* const) MALLOC3 (10, 10, 4, sizeof(T));

  __GHOST_BEGIN(focus, __with_reverse(
    [&]() {
      __consumes("_Uninit(Group(range(0, 10, 1), fun i -> "
                 "Group(range(0, 10, 1), fun j -> "
                 "Group(range(0, 4, 1), fun k -> "
                 "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))))");
      __produces("_Uninit(Group(range(0, 10, 1), fun i -> "
                 "Group(range(2, 10, 1), fun j -> "
                 "Group(range(0, 4, 1), fun k -> "
                 "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))))"
                 ", "
                 "Group(range(0, 10, 1), fun i -> Wand("
                 "  _Uninit("
                 "  Group(range(2, 10, 1), fun j -> "
                 "  Group(range(0, 4, 1), fun k -> "
                 "    &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))), "
                 "  _Uninit("
                 "  Group(range(0, 10, 1), fun j -> "
                 "  Group(range(0, 4, 1), fun k -> "
                 "    &a[MINDEX3(10,10,4,i,j,k)] ~> Cell)))"
                 "))");
      for (int i = 0; i < 10; i++) {
        __consumes("_Uninit("
                  "Group(range(0, 10, 1), fun j -> "
                  "Group(range(0, 4, 1), fun k -> "
                  "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell)))");
        __produces("_Uninit("
                  "Group(range(2, 10, 1), fun j -> "
                  "Group(range(0, 4, 1), fun k -> "
                  "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell)))"
                  ", "
                  "Wand("
                  "  _Uninit("
                  "  Group(range(2, 10, 1), fun j -> "
                  "  Group(range(0, 4, 1), fun k -> "
                  "    &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))), "
                  "  _Uninit("
                  "  Group(range(0, 10, 1), fun j -> "
                  "  Group(range(0, 4, 1), fun k -> "
                  "    &a[MINDEX3(10,10,4,i,j,k)] ~> Cell)))"
                  ")");
        __ghost(group_focus_subrange_uninit,
          "items := fun j -> Group(range(0, 4, 1), fun k -> &a[MINDEX3(10,10,4,i,j,k)] ~> Cell), "
          "start := 2, stop := 10, step := 1");
      }
    },
    [&]() {
      for (int i = 0; i < 10; i++) {
        __produces("_Uninit("
                  "Group(range(0, 10, 1), fun j -> "
                  "Group(range(0, 4, 1), fun k -> "
                  "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell)))");
        __consumes("_Uninit("
                  "Group(range(2, 10, 1), fun j -> "
                  "Group(range(0, 4, 1), fun k -> "
                  "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell)))"
                  ", "
                  "Wand("
                  "  _Uninit("
                  "  Group(range(2, 10, 1), fun j -> "
                  "  Group(range(0, 4, 1), fun k -> "
                  "    &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))), "
                  "  _Uninit("
                  "  Group(range(0, 10, 1), fun j -> "
                  "  Group(range(0, 4, 1), fun k -> "
                  "    &a[MINDEX3(10,10,4,i,j,k)] ~> Cell)))"
                  ")");
        __ghost(group_unfocus_subrange_uninit, "items := fun j -> Group(range(0, 4, 1), fun k -> &a[MINDEX3(10,10,4,i,j,k)] ~> Cell)");
      }
    }
  ), "");

  for (int i = 0; i < 10; i++) {
    __consumes("_Uninit(Group(range(2, 10, 1), fun j -> Group(range(0, 4, 1), fun k ->"
               "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell)))");
    __produces("Group(range(2, 10, 1), fun j -> Group(range(0, 4, 1), fun k ->"
               "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))");

    for (int j = 2; j < 10; j++) {
      __consumes("_Uninit(Group(range(0, 4, 1), fun k -> &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))");
      __produces("Group(range(0, 4, 1), fun k -> &a[MINDEX3(10,10,4,i,j,k)] ~> Cell)");

      for (int k = 0; k < 4; k++) {
        __consumes("_Uninit(&a[MINDEX3(10,10,4,i,j,k)] ~> Cell)");
        __produces("&a[MINDEX3(10,10,4,i,j,k)] ~> Cell");

        a[MINDEX3(10,10,4,i,j,k)] = 1;
      }
    }
  }
  __GHOST_END(focus);
  MFREE3(10, 10, 4, a);
/* FIXME:
  b = (T* const) CALLOC3 (10, 10, 4, sizeof(T));
  for (int i = 0; i < 10; i++) {
    __modifies("Group(range(0, 10, 1), fun j -> Group(range(0, 4, 1), fun k ->"
               "  &b[MINDEX3(10,10,4,i,j,k)] ~> Cell))");

    for (int j = 0; j < 10; j++) {
      __modifies("Group(range(0, 4, 1), fun k -> &b[MINDEX3(10,10,4,i,j,k)] ~> Cell)");

      for (int k = 0; k < 4; k++) {
        __modifies("&b[MINDEX3(10,10,4,i,j,k)] ~> Cell");

        b[MINDEX3(10,10,4,i,j,k)] = 1;
      }
    }
  }
  MFREE3(10, 10, 4, b);
*/
  int z = 0;
}
