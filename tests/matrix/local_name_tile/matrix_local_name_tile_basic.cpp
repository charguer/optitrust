#include <optitrust.h>

typedef int T;

void f(T* b) {
  __modifies("b ~> Matrix3(10, 10, 4)");

  // FIXME: CALLOC
  T* const a = (T* const) MALLOC3 (10, 10, 4, sizeof(T));
  // TODO: group_focus_subrange
  for (int i = 0; i < 10; i++) {
    __consumes("_Uninit(Group(range(0, 10, 1), fun j -> Group(range(0, 4, 1), fun k ->"
               "  &a[MINDEX3(10,10,4,i,j,k)] ~> Cell)))");
    __produces("Group(range(0, 10, 1), fun j -> Group(range(0, 4, 1), fun k ->"
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
