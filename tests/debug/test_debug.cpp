#include "../../include/optitrust.h"
#include <stdio.h>

int min(int a, int b) { return a < b ? a : b; }

void test() {
  float* s = (float*)MALLOC1(32, sizeof(float));

  for (int i = 0; i < 32; i++) {
    s[MINDEX1(32, i)] = 0.0f;
  }

  for (int j = 0; j < 30; j++) {
    printf("%f\n", s[MINDEX1(32, j - 2)]);
  }

  free(s);
}

int main() {
a: {
  test();
  }
b: {
  test();
  }
c: {
  test();
  }
  return 0;
}