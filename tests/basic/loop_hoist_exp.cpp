#include "../../include/optitrust.h"

int* t;

int* u;

int main() {
  int* x_step = (int*)MALLOC1(10, sizeof(int));
  int* z_step = (int*)MALLOC1(10, sizeof(int));
  for (int i = 0; i < 10; i++) {
    int& x = x_step[MINDEX1(10, i)];
    x = t[i];
    u[i] = x;
    int& z = z_step[MINDEX1(10, i)];
    z = x;
    int w = 0;
  }
  int* yl = (int*)MALLOC3(5, 4, 8 / 2, sizeof(int));
  for (int l = 0; l < 5; l++) {
    int* ym = &yl[MINDEX3(5, 4, 8 / 2, l, 0, 0)];
    for (int m = 2; m < 6; m++) {
      int* yn = &ym[MINDEX2(4, 8 / 2, m + -2, 0)];
      for (int n = 4; n < 11; n += 2) {
        int& y = yn[MINDEX1(8 / 2, (n + -4) / 2)];
        y = 0;
        u[m] = y;
      }
    }
  }
}
