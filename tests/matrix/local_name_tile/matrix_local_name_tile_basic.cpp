#include <optitrust.h>

typedef int T;

// TODO: calloc variant
void malloc_uninit_pre() {
  __pure();

  T* const a = (T* const) MALLOC3 (10, 10, 4, sizeof(T));

  __GHOST_BEGIN(focus, group2_focus_subrange_uninit,
    "items := fun i -> fun j -> for k in 0..4 -> &a[MINDEX3(10,10,4,i,j,k)] ~> Cell, "
    "sub_range := 2..10");

  for (int i = 0; i < 10; i++) {
    __xwrites("for j in 2..10 -> for k in 0..4 ->"
               "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell");

    for (int j = 2; j < 10; j++) {
      __strict();
      __xwrites("for k in 0..4 -> &a[MINDEX3(10,10,4,i,j,k)] ~> Cell");

      for (int k = 0; k < 4; k++) {
        __strict();
        __xwrites("&a[MINDEX3(10,10,4,i,j,k)] ~> Cell");

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
    "sub_range := 2..10");

  for (int i1 = 2; i1 < 10; i1++) {
    __strict();
    __xwrites("&a[MINDEX1(10,i1)] ~> Cell");

    a[MINDEX1(10,i1)] = 1;
  }

  for (int i = 2; i < 10; i++) {
    __strict();
    __xmodifies("&a[MINDEX1(10,i)] ~> Cell");

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
    "items := fun i -> fun j -> for k in 0..4 -> &a[MINDEX3(10,10,4,i,j,k)] ~> Cell, "
    "sub_range := 2..10");
  for (int i = 0; i < 10; i++) {
    __xwrites("for j in 2..10 -> for k in 0..4 ->"
               "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell");

    for (int j = 2; j < 10; j++) {
      __strict();
      __xwrites("for k in 0..4 -> &a[MINDEX3(10,10,4,i,j,k)] ~> Cell");

      for (int k = 0; k < 4; k++) {
        __strict();
        __xwrites("&a[MINDEX3(10,10,4,i,j,k)] ~> Cell");

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
    __xmodifies("for j in 0..10 -> for k in 0..4 ->"
               "  &b[MINDEX3(10,10,4,i,j,k)] ~> Cell");

    for (int j = 0; j < 10; j++) {
      __strict();
      __xmodifies("for k in 0..4 -> &b[MINDEX3(10,10,4,i,j,k)] ~> Cell");

      for (int k = 0; k < 4; k++) {
        __strict();
        __xmodifies("&b[MINDEX3(10,10,4,i,j,k)] ~> Cell");

        b[MINDEX3(10,10,4,i,j,k)] = 1;
      }
    }
  }
  MFREE3(10, 10, 4, b);
*/
