#include <optitrust.h>

typedef int T;
// FIXME: not supported yet
// T* b;

void f() {
  __pure();

  // TODO: deal with CALLOC
  T* const a = MALLOC3(T, 10, 10, 4);
  __GHOST_BEGIN(focusa, group2_focus_subrange_uninit,
    "items := fun i -> fun j -> for k in 0..4 -> &a[MINDEX3(10,10,4,i,j,k)] ~> Cell, "
    "sub_range := 2..10");
  for (int i = 0; i < 10; i++) {
    __xwrites("for j in 2..10 ->"
             "for k in 0..4 -> &a[MINDEX3(10,10,4,i,j,k)] ~> Cell");
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
  for (int i = 0; i < 10; i++) {
    __xmodifies("for j in 2..10 ->"
               "for k in 0..4 -> &a[MINDEX3(10,10,4,i,j,k)] ~> Cell");
    for (int j = 2; j < 10; j++) {
      __strict();
      __xmodifies("for k in 0..4 -> &a[MINDEX3(10,10,4,i,j,k)] ~> Cell");
      for (int k = 0; k < 4; k++) {
        __strict();
        __xmodifies("&a[MINDEX3(10,10,4,i,j,k)] ~> Cell");
        a[MINDEX3(10,10,4,i,j,k)] += 1;
      }
    }
  }
  __GHOST_END(focusa);
  free(a);

  T* const b = MALLOC3(T, 10, 10, 4);
  __GHOST_BEGIN(focusb, group2_focus_subrange_uninit,
    "items := fun i -> fun j -> for k in 0..4 -> &b[MINDEX3(10,10,4,i,j,k)] ~> Cell, "
    "sub_range := 2..10");
  for (int i = 0; i < 10; i++) {
    __xwrites("for j in 2..10 ->"
             "for k in 0..4 -> &b[MINDEX3(10,10,4,i,j,k)] ~> Cell");
    for (int j = 2; j < 10; j++) {
      __strict();
      __xwrites("for k in 0..4 -> &b[MINDEX3(10,10,4,i,j,k)] ~> Cell");
      for (int k = 0; k < 4; k++) {
        __strict();
        __xwrites("&b[MINDEX3(10,10,4,i,j,k)] ~> Cell");
        b[MINDEX3(10,10,4,i,j,k)] = 1;
      }
    }
  }
  __GHOST_END(focusb);
  free(b);

  T* const c = MALLOC3(T, 10, 10, 4);
  __GHOST_BEGIN(focusc, group2_focus_subrange_uninit,
    "items := fun i -> fun j -> for k in 0..4 -> &c[MINDEX3(10,10,4,i,j,k)] ~> Cell, "
    "sub_range := 2..10");
  for (int i = 0; i < 10; i++) {
    __xwrites("for j in 2..10 ->"
             "for k in 0..4 -> &c[MINDEX3(10,10,4,i,j,k)] ~> Cell");
    for (int j = 2; j < 10; j++) {
      __strict();
      __xwrites("for k in 0..4 -> &c[MINDEX3(10,10,4,i,j,k)] ~> Cell");
      for (int k = 0; k < 4; k++) {
        __strict();
        __xwrites("&c[MINDEX3(10,10,4,i,j,k)] ~> Cell");
        c[MINDEX3(10,10,4,i,j,k)] = 1;
      }
    }
    for (int j2 = 2; j2 < 10; j2++) {
      __strict();
      __xwrites("for k in 0..4 -> &c[MINDEX3(10,10,4,i,j2,k)] ~> Cell");
      for (int k2 = 0; k2 < 4; k2++) {
        __strict();
        __xwrites("&c[MINDEX3(10,10,4,i,j2,k2)] ~> Cell");
        c[MINDEX3(10,10,4,i,j2,k2)] = 2;
      }
    }
  }
  __GHOST_END(focusc);
  free(c);

  int z = 0;
}
