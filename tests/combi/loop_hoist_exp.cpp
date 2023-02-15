#include "../../include/optitrust.h"

int* t;

int* u;

int main() {
  int* x_step1 = (int*)MALLOC1(10, sizeof(int));
  for (int i = 0; i < 10; i++) {
    int& x = x_step1[MINDEX1(10, i)];
    x = t[i];
    u[i] = x;
    int z = x;
  }
  int* y = (int*)MALLOC1(8, sizeof(int));
  for (int j = 2; j < 10; j++) {
    y[MINDEX1(8, j + -2)] = t[j];
    u[j] = y[MINDEX1(8, j + -2)] + 1;
    y[MINDEX1(8, j + -2)] = u[j];
  }
  int total = 0;
  int* xk = (int*)MALLOC1(5 / 2, sizeof(int));
  for (int k = 3; k < 7; k += 2) {
    int a = k + 1;
    int& x = xk[MINDEX1(5 / 2, (k + -3) / 2)];
    x = a + 1;
    int y = x + 1;
    total += y;
  }
  int* x = (int*)MALLOC2(5, 2, sizeof(int));
  for (int l = 0; l < 5; l++) {
    for (int m = 0; m < 2; m++) {
      x[MINDEX2(5, 2, l, m)] = l + m;
    }
  }
  int* xa = (int*)MALLOC3(8, 5, 2, sizeof(int));
  for (int a = 0; a < 8; a++) {
    int y = 0;
    for (int b = 0; b < 5; b++) {
      for (int c = 0; c < 2; c++) {
        xa[MINDEX3(8, 5, 2, a, b, c)] = a + b + c;
      }
      int z = 0;
    }
  }
}
