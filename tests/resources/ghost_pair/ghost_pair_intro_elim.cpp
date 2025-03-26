#include <optitrust.h>

void f() {
  __pure();

  int x = 0;

  int* const A = CALLOC1(int, 10);
  __GHOST_BEGIN(focusA, ro_matrix1_focus, "A, 0");
  x += A[MINDEX1(10, 0)];
  __GHOST_END(focusA);
  free(A);

  int* const B = CALLOC2(int, 8, 6);
  // LATER: Remove manually given items arguments, and fix them before ghost pair elimination using a transformation
  __GHOST_BEGIN(focusBi, group_focus, "items := fun i -> for j in 0..6 -> &B[MINDEX2(8, 6, i, j)] ~> Cell, i := 1");
  __GHOST_BEGIN(focusBj, ro_group_focus, "items := fun j -> &B[MINDEX2(8, 6, 1, j)] ~> Cell, i := 2");
  x += B[MINDEX2(8, 6, 1, 2)];
  __GHOST_END(focusBj);
  __GHOST_END(focusBi);

  free(B);
}
