#include <optitrust.h>

int main() {
  __pure();

  int x = 0;

  // This pattern of "chained" focus is also used in ghost_pair_minimize and ghost_pair_distribute. It heavily relies on the ordering of linear resources.

  float* const B = (float* const) CALLOC2(8, 6, sizeof(float));
  __GHOST_BEGIN(focusBi, group_ro_focus, "i := 1, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
  __GHOST_BEGIN(focusBj, group_ro_focus, "i := 2, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
  x += B[MINDEX2(8, 6, 1, 2)];
  __GHOST_END(focusBj);
  __GHOST_END(focusBi);

  MFREE2(8, 6, B);
}
