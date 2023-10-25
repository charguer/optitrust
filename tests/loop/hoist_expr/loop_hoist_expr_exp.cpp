#include "../../../include/optitrust.h"

int* t;

int* u;

int main() {
  int* const t2 = (int* const)MALLOC1(10, sizeof(int));
  for (int i = 0; i < 10; i++) {
    t2[MINDEX1(10, i)] = t[i];
  }
  for (int i = 0; i < 10; i++) {
    int x = t2[MINDEX1(10, i)];
    u[i] = x;
    int z = x;
  }
  MFREE1(10, t2);
  int* const t02 = (int* const)MALLOC0(sizeof(int));
  t02[MINDEX0()] = t[0];
  for (int l = 0; l < 5; l++) {
    for (int m = 0; m < 2; m++) {
      int x = l + m + t02[MINDEX0()];
    }
  }
  MFREE0(t02);
  int* const a2 = (int* const)MALLOC1(8, sizeof(int));
  for (int a = 0; a < 8; a++) {
    a2[MINDEX1(8, a)] = a;
  }
  for (int a = 0; a < 8; a++) {
    int y = 0;
    for (int b = 0; b < 5; b++) {
      for (int c = 0; c < 2; c++) {
        int x = a2[MINDEX1(8, a)] + b + c;
      }
      int z = 0;
    }
  }
  MFREE1(8, a2);
}
