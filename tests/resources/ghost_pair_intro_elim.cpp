#include "../../include/optitrust.h"

int main() {
  __pure();

  int x = 0;

  float* const A = (float* const) MALLOC1(10, sizeof(float));
  __GHOST_BEGIN(focusA, matrix1_ro_focus, "A, 0");
  x += A[MINDEX1(10, 0)];
  __GHOST_END(focusA);
  MFREE1(10, A);

  float* const B = (float* const) MALLOC2(8, 6, sizeof(float));
  // LATER: Remove manually given items arguments, and fix them before ghost pair elimination using a transformation
  __GHOST_BEGIN(focusBi, group_focus, "items := fun i -> Group(range(0, 6, 1), fun j -> &B[MINDEX2(8, 6, i, j)] ~> Cell), i := 1, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
  __GHOST_BEGIN(focusBj, group_ro_focus, "items := fun j -> &B[MINDEX2(8, 6, 1, j)] ~> Cell, i := 2, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
  x += B[MINDEX2(8, 6, 1, 2)];
  __GHOST_END(focusBj);
  __GHOST_END(focusBi);

  MFREE2(8, 6, B);
}
