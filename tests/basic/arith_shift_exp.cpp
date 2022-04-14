#include <stdio.h>

const int N = 5;

int main() {
  double t[N];
  for (int i = 0; i < N; i++) {
    t[i] = i + 3.14 - i;
  }
  double s = 0;
  for (int i = 0; i < N; i++) {
    s += t[i] + i;
  }
  printf("%d\n", s);
}

float* u;

int other() {
  for (int i = 0; i < N; i++) {
    double a = (double)u[i] + i;
    double b = 3.14 * a + 0.68;
    u[i] = (float)(b + i);
  }
}
