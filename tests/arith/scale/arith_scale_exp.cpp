#include <stdio.h>

int main() {
  const int N = 5;
  double t[N];
  for (int i = 0; i < N; i++) {
    t[i] = ((double)i + 3.14) / i;
  }
  double s = (double)0;
  for (int i = 0; i < N; i++) {
    s += t[i] * i;
  }
  printf("%f\n", s);
}

void other(float* u) {
  const int N = 5;
  for (int i = 0; i < N; i++) {
    float a = (double)u[i] * i;
    float b = (float)(3.14 * (double)a + 0.68);
    u[i] = (float)(b * i);
  }
}
