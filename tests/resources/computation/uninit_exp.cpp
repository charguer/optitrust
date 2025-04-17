#include "../../../include/optitrust.h"

void init_var_evry_iter() {
  int x;
  for (int i = 0; i < 5; i++) {
    __strict();
    __smodifies("x ~> UninitCell");
    x = 5;
    x++;
  }
}

void init_arr_evry_iter() {
  int* const x = (int*)malloc(MSIZE1(3) * sizeof(int));
  for (int i = 0; i < 6; i++) {
    __strict();
    __smodifies("for j in 0..3 -> &x[j] ~> UninitCell");
    for (int j = 0; j < 3; j++) {
      __strict();
      __xwrites("&x[j] ~> Cell");
      x[j] = 0;
    }
    for (int j = 0; j < 3; j++) {
      __strict();
      __xmodifies("&x[j] ~> Cell");
      x[j]++;
    }
  }
  free(x);
}
