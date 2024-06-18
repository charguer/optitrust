#include <stdio.h>

const int N = 5;

int main() {
  double t[N];
  for (int i = 0; i < N; i++) {
    t[i] = (double)i + 3.14 - i;
  }
  double s = 0.;
  for (int i = 0; i < N; i++) {
    s += t[i] + i;
  }
  printf("%f\n", s);
}

int other(float* u) {
  for (int i = 0; i < N; i++) {
    float a = (double)u[i] + i;
    float b = (float)(3.14 * (double)a + 0.68);
    u[i] = (float)(b + i);
  }
}
