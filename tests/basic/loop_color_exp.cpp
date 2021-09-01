#include <stdio.h>

int main() {
  int N = 10;
  int C = 2;
  int D = 2;
  int total1 = 0;
  int total2 = 0;
  for (int ci = 0; (ci < C); ci++) {
    for (int i = ci; (i < N); i += C) {
      total1 += i;
    }
  }
  printf("%d\n", total1);
  for (int cj = 0; (cj < C); cj++) {
    for (int j = (cj * 2); (j < N); j += (C * 2)) {
      total2 += j;
    }
  }
  printf("%d\n", total2);
  return 0;
}