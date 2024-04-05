#include <optitrust.h>

void f(int* t, int* u) {
  __reads("t ~> Matrix1(10)");
  __writes("u ~> Matrix1(10)");

  for (int i = 0; i < 10; i++) {
    __strict();
    __xreads("&t[MINDEX1(10, i)] ~> Cell");
    __xwrites("&u[MINDEX1(10, i)] ~> Cell");

    int x = t[MINDEX1(10, i)];
    u[MINDEX1(10, i)] = x;
    int z = x;
  }

  __ghost(matrix1_ro_focus, "t, 0");
  for (int l = 0; l < 5; l++) {
    __strict();
    __sreads("&t[MINDEX1(10, 0)] ~> Cell");

    for (int m = 0; m < 2; m++) {
      __strict();
      __sreads("&t[MINDEX1(10, 0)] ~> Cell");

      int x = l + m + t[MINDEX1(10, 0)];
    }
  }
  __ghost(matrix1_ro_unfocus, "t");

  for (int a = 0; a < 8; a++) {
    __strict();

    int y = 0;
    for (int b = 0; b < 5; b++) {
      __strict();

      for (int c = 0; c < 2; c++) {
        __strict();

        int x = a + b + c;
      }
      int z = 0;
    }
  }
}
/*
int test1() {
    for (int l = 0; l < 5; l++) {
      for (int m = 0; m < 2; m++) {
        int x = 0;
      }
    }
}
//    --> first hoist
int test2() {
  for (int l = 0; l < 5; l++) {
    int* x_step = (int*) MALLOC1(2, sizeof(int));
    for (int m = 0; m < 2; m++) {
      int& x = x_step[MINDEX1(2, m)];
      x = 0;
    }
  }
}
//    --> second hoist
int test3() {
  int* x_step_bis = (int*) MALLOC2(5, 2, sizeof(int));
  for (int l = 0; l < 5; l++) {
    int* x_step = &x_step_bis[MINDEX2(5, 2, l, 0)];
    // int* x_step = MREF2(x_step_bis, 5, 2, l, 0);
    for (int m = 0; m < 2; m++) {
      int& x = x_step[MINDEX1(2, m)];
      x = 0;
    }
  }
}
// (&x_step_bis[MINDEX2(5, 2, l, 0)])[MINDEX1(2, m)]
//    --> final
int test4() {
  int* x_step_bis = (int*) MALLOC2(5, 2, sizeof(int));
  for (int l = 0; l < 5; l++) {
    for (int m = 0; m < 2; m++) {
      int& x = x_step_bis[MINDEX2(5, 2, l, m)];
      x = 0;
    }
  }
}
*/

// Variable.inline simplification

// (&(&x_step_ter[MINDEX3(9, 5, 2, n, 0, 0))[MINDEX2(5, 2, l, 0)])[MINDEX1(2, m)]
// = (&x_step_ter[MINDEX3(9, 5, 2, n, l, 0)])[MINDEX1(2, m)]
// = x_step_ter[MINDEX3(9, 5, 2, n, l, m)

// (&x[MINDEXN(d1, .., dN, i1, 0, .., 0)])[MINDEX{N-1}(d2, .., dN, i2, .., iN)]
// = x[MINDEXN(d1, .., dN, i1, i2, .., iN)]

// (&x[MINDEXN(d1, .., dN, i1, i{N-M}, 0, .., 0)])[MINDEXM(d{N-M+1}, .., dN, i{N-M+1}, .., iN)]
// = x[MINDEXN(d1, .., dN, i1, .., iN)]
