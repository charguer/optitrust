#include <stdio.h>

int main() {
  int N = 10;
  int C = 2;
  int D = 2;
  int s = 0;
  for (int ci = 0; (ci < C); ci++) {
    for (int i = ci; (i < N); i += C) {
      s += i;
    }
  }
  for (int cj = 0; (cj < C); cj++) {
    for (int j = (cj * 2); (j < N); j += (C * 2)) {
      s += j;
    }
  }
  printf("%d\n", s);
  return 0;
}