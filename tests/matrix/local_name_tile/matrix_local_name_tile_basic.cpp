#include <optitrust.h>

typedef int T;

// TODO: calloc variant
void malloc_uninit_pre() {
  __pure();

  T* const a = (T* const) MALLOC3 (10, 10, 4, sizeof(T));

  __GHOST_BEGIN(focus, group2_focus_subrange_uninit,
    "items := fun i -> fun j -> Group(range(0, 4, 1), fun k -> &a[MINDEX3(10,10,4,i,j,k)] ~> Cell), "
    "sub_range := range(2, 10, 1)");

  for (int i = 0; i < 10; i++) {
    __writes("Group(range(2, 10, 1), fun j -> Group(range(0, 4, 1), fun k ->"
               "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))");

    for (int j = 2; j < 10; j++) {
      __writes("Group(range(0, 4, 1), fun k -> &a[MINDEX3(10,10,4,i,j,k)] ~> Cell)");

      for (int k = 0; k < 4; k++) {
        __writes("&a[MINDEX3(10,10,4,i,j,k)] ~> Cell");

        a[MINDEX3(10,10,4,i,j,k)] = 1;
      }
    }
  }
  __GHOST_END(focus);
  MFREE3(10, 10, 4, a);
  int z = 0;
}

// TODO: calloc variant
void malloc_uninit_post() {
  __pure();

  T* const a = (T* const) MALLOC1(10, sizeof(T));
  __GHOST_BEGIN(focus, group_focus_subrange_uninit,
    "items := fun i -> &a[MINDEX1(10,i)] ~> Cell, "
    "sub_range := range(2, 10, 1)");

  for (int i1 = 2; i1 < 10; i1++) {
    __writes("&a[MINDEX1(10,i1)] ~> Cell");

    a[MINDEX1(10,i1)] = 1;
  }

  for (int i = 2; i < 10; i++) {
    __modifies("&a[MINDEX1(10,i)] ~> Cell");

    a[MINDEX1(10,i)] += 1;
  }

  __GHOST_END(focus);
  MFREE1(10, a);
}

// TODO: calloc variant
void malloc_uninit_prepost() {
  __pure();

  T* const a = (T* const) MALLOC3 (10, 10, 4, sizeof(T));

  __GHOST_BEGIN(focus, group2_focus_subrange_uninit,
    "items := fun i -> fun j -> Group(range(0, 4, 1), fun k -> &a[MINDEX3(10,10,4,i,j,k)] ~> Cell), "
    "sub_range := range(2, 10, 1)");
  for (int i = 0; i < 10; i++) {
    __writes("Group(range(2, 10, 1), fun j -> Group(range(0, 4, 1), fun k ->"
               "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell))");

    for (int j = 2; j < 10; j++) {
      __writes("Group(range(0, 4, 1), fun k -> &a[MINDEX3(10,10,4,i,j,k)] ~> Cell)");

      for (int k = 0; k < 4; k++) {
        __writes("&a[MINDEX3(10,10,4,i,j,k)] ~> Cell");

        a[MINDEX3(10,10,4,i,j,k)] = 1;
      }
    }
  }
  __GHOST_END(focus);
  MFREE3(10, 10, 4, a);
}

/* FIXME:
void f(T* b) {
  __modifies("b ~> Matrix3(10, 10, 4)");

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
