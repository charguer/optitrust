#include "../../../include/optitrust.h"

void var(int* t) {
  __modifies("t ~> Matrix1(3)");

  int x = 0;
  for (int i = 0; i < 3; i++) {
    x = 3;
    t[MINDEX1(3, i)] = x;
  }
}

void arr(int* t, int* x) {
  __modifies("t ~> Matrix2(3, 5), x ~> Matrix1(5)");

  for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 5; j++) {
      x[MINDEX1(5, j)] = 3;
    }
    for (int j = 0; j < 5; j++) {
      t[MINDEX2(3, 5, i, j)] = x[MINDEX1(5, j)];
    }
  }
}


void test(int* t){
  __modifies("t ~> Matrix1(10)");

  int a = 5;
  int b = 6;
  for (int i = 0; i < 10; i++) {
    int r = i;
    for (int j = 0; j < 10; j++) {
      int s = i;
      int x = a + b;
      t[MINDEX1(10, i)] = i;
    }
  }
}
