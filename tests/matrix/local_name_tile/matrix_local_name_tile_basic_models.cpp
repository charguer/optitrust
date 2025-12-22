#include <optitrust_models.h>

// TODO: calloc variant
void malloc_uninit_pre() {
  __pure();

  int* const a = MALLOC3(int, 10, 10, 4);

  __GHOST_BEGIN(focus, group2_focus_subrange,
    "items := fun i -> fun j -> for k in 0..4 -> &a[MINDEX3(10,10,4,i,j,k)] ~> UninitCell, "
    "sub_range := 2..10");

  for (int i = 0; i < 10; i++) {
    __xwrites("for j in 2..10 -> for k in 0..4 ->"
               "  &a[MINDEX3(10,10,4,i,j,k)] ~~> i");

    for (int j = 2; j < 10; j++) {
      __strict();
      __xwrites("for k in 0..4 -> &a[MINDEX3(10,10,4,i,j,k)] ~~> i");

      for (int k = 0; k < 4; k++) {
        __strict();
        __xwrites("&a[MINDEX3(10,10,4,i,j,k)] ~~> i");

        a[MINDEX3(10,10,4,i,j,k)] = i;
      }
    }
  }
  __GHOST_END(focus);
  free(a);
  int z = 0;
}

// TODO: calloc variant
void malloc_uninit_post() {
  __pure();

  int* const a = MALLOC1(int, 10);
  __GHOST_BEGIN(focus, group_focus_subrange,
    "items := fun i -> &a[MINDEX1(10,i)] ~> UninitCell, "
    "sub_range := 2..10");

  for (int i1 = 2; i1 < 10; i1++) {
    __strict();
    __xwrites("&a[MINDEX1(10,i1)] ~~> 1");

    a[MINDEX1(10,i1)] = 1;
  }

  for (int i = 2; i < 10; i++) {
    __strict();
    __xconsumes("&a[MINDEX1(10,i)] ~~> 1");
    __xproduces("&a[MINDEX1(10,i)] ~~> 1 + i");

    a[MINDEX1(10,i)] += i;
  }

  __GHOST_END(focus);
  free(a);
}

// TODO: calloc variant
void malloc_uninit_prepost() {
  __pure();

  int* const a = MALLOC3(int, 10, 10, 4);

  __GHOST_BEGIN(focus, group2_focus_subrange,
    "items := fun i -> fun j -> for k in 0..4 -> &a[MINDEX3(10,10,4,i,j,k)] ~> UninitCell, "
    "sub_range := 2..10");
  for (int i = 0; i < 10; i++) {
    __xwrites("for j in 2..10 -> for k in 0..4 ->"
               "  &a[MINDEX3(10,10,4,i,j,k)] ~~> 1");

    for (int j = 2; j < 10; j++) {
      __strict();
      __xwrites("for k in 0..4 -> &a[MINDEX3(10,10,4,i,j,k)] ~~> 1");

      for (int k = 0; k < 4; k++) {
        __strict();
        __xwrites("&a[MINDEX3(10,10,4,i,j,k)] ~~> 1");

        a[MINDEX3(10,10,4,i,j,k)] = 1;
      }
    }
  }
  __GHOST_END(focus);
  free(a);
}

/* FIXME:
void f(T* b) {
  __modifies("b ~> Matrix3(10, 10, 4)");

  b = (T*) CALLOC3 (10, 10, 4, sizeof(T));
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
  free(b);
*/
