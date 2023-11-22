#include "../../../include/optitrust.h"

void init_var_evry_iter() {
  int x;
  for (int i = 0; i < 5; i++) {
    __sequentially_modifies("_Uninit(x ~> Cell)");
    x = 5;
    x++;
  }
}

void init_arr_evry_iter() {
  int* const x = (int* const)MALLOC1(3, sizeof(int));
  for (int i = 0; i < 6; i++) {
    __sequentially_modifies(
        "_Uninit(Group(range(0, 3, 1), fun j -> &x[j] ~> Cell))");
    for (int j = 0; j < 3; j++) {
      __consumes("_Uninit(&x[j] ~> Cell)");
      __produces("&x[j] ~> Cell");
      x[j] = 0;
    }
    for (int j = 0; j < 3; j++) {
      __modifies("&x[j] ~> Cell");
      x[j]++;
    }
  }
  MFREE1(3, x);
}
