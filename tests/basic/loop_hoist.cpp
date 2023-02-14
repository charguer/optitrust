#include "../../include/optitrust.h"

int *t;
int *u;

int main() {
  for (int i = 0; i < 10; i++) {
    int x;
    x = t[i];
    u[i] = x;
    int z;
    z = x;
    int w = 0;
  }

  for (int l = 0; l < 5; l++) {
    for (int m = 2; m < 6; m++) {
      for (int n = 4; n < 11; n += 2) {
        int y;
        y = 0;
        u[m] = y;
      }
    }
  }

  // Question:
  // hoist:
  // - int* y_step = (int*) MALLOC1(2, sizeof(int));
  // - becomes:
  //   - int* y_step_step = (int*) MALLOC2(5, 2, sizeof(int));
  // vs:
  // - int* y_step;
  //   y_step = (int*) MALLOC1(2, sizeof(int));
  // - becomes:
  //   - int* y_step_step = (int**) MALLOC1(5, sizeof(int*));
}