#include <optitrust.h>

int main() {
  int* const x_step = (int*)malloc(MSIZE1(4) * sizeof(int));
  for (int i = 0; i < 4; i++) {
    int* const x = &x_step[MINDEX1(4, i)];
    x[MINDEX0()] = 2 * i;
  }
  free(x_step);
}
