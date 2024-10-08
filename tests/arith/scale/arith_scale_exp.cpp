#include <stdio.h>

int main() {
  const int N = 5;
  double t[N];
  for (int i = 0; i < N; i++) {
    t[i] = exact_div((i + 3.14), i);
  }
  double s = 0;
  for (int i = 0; i < N; i++) {
    s += t[i] * i;
  }
  printf("%f\n", s);
}

float* u;

void other() {
  const int N = 5;
  for (int i = 0; i < N; i++) {
    double a = (double)u[i] * i;
    double b = 3.14 * a + 0.68;
    u[i] = (float)(b * i);
  }
}
