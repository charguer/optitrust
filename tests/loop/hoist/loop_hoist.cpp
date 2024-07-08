#include <optitrust.h>

int *t;
int *u;

int main() {
  for (int i = 0; i < 10; i++) {
    int x = t[i];
    u[i] = x;
    int z = x;
  }

  for (int j = 2; j < 10; j++) {
    int y = t[j];
    u[j] = y + 1;
    y = u[j];
  }
  int total = 0;
  for (int k = 3; k < 7; k += 2) {
    int a = k + 1;
    int x = a + 1;
    int y = x + 1;
    total += y;
  }

  for (int l = 0; l < 5; l++) {
    for (int m = 0; m < 2; m++) {
      int x = l + m;
    }
  }

  for (int a = 0; a < 8; a++) {
    int y = 0;
    for (int b = 0; b < 5; b++) {
      for (int c = 0; c < 2; c++) {
        int x = a + b + c;
      }
      int z = 0;
    }
  }

  for (int mi = 0; mi < 8; mi++) {
    float* const m = (float*) MALLOC1(2, sizeof(float));
    MFREE1(2, m);
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
