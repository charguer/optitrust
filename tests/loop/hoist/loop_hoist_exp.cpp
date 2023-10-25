#include "../../../include/optitrust.h"

int* t;

int* u;

int main() {
  int* const x_step1 = (int* const)MALLOC1(10, sizeof(int));
  for (int i = 0; i < 10; i++) {
    int* const x = &x_step1[MINDEX1(10, i)];
    x[MINDEX0()] = t[i];
    u[i] = x[MINDEX0()];
    int z = x[MINDEX0()];
  }
  MFREE1(10, x_step1);
  int* const y = (int* const)MALLOC1(8, sizeof(int));
  for (int j = 2; j < 10; j++) {
    y[MINDEX1(8, j - 2)] = t[j];
    u[j] = y[MINDEX1(8, j - 2)] + 1;
    y[MINDEX1(8, j - 2)] = u[j];
  }
  MFREE1(8, y);
  int total = 0;
  int* const xk = (int* const)MALLOC1(5 / 2, sizeof(int));
  for (int k = 3; k < 7; k += 2) {
    int a = k + 1;
    int* const x = &xk[MINDEX1(5 / 2, (k - 3) / 2)];
    x[MINDEX0()] = a + 1;
    int y = x[MINDEX0()] + 1;
    total += y;
  }
  MFREE1(5 / 2, xk);
  int* const x = (int* const)MALLOC2(5, 2, sizeof(int));
  for (int l = 0; l < 5; l++) {
    for (int m = 0; m < 2; m++) {
      x[MINDEX2(5, 2, l, m)] = l + m;
    }
  }
  MFREE2(5, 2, x);
  int* const xa = (int* const)MALLOC3(8, 5, 2, sizeof(int));
  for (int a = 0; a < 8; a++) {
    int y = 0;
    for (int b = 0; b < 5; b++) {
      for (int c = 0; c < 2; c++) {
        xa[MINDEX3(8, 5, 2, a, b, c)] = a + b + c;
      }
      int z = 0;
    }
  }
  MFREE3(8, 5, 2, xa);
  float* const m = (float* const)MALLOC1(2, sizeof(float));
  for (int mi = 0; mi < 8; mi++) {
  }
  MFREE1(2, m);
}
