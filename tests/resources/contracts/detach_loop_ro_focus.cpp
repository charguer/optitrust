#include <optitrust.h>

void f(int* M) {
  __reads("M ~> Matrix1(10)");

  int acc = 0;
  for (int i = 0; i < 10; ++i) {
    __strict();
    __sequentially_modifies("&acc ~> Cell");
    __reads("&M[MINDEX1(10, i)] ~> Cell");
    acc += M[MINDEX1(10, i)];
  }
}
