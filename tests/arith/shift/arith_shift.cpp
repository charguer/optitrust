#include <stdio.h>

const int N = 5;

int main() {
   double t[N];
   for (int i = 0; i < N; i++) {
      t[i] = i + 3.14; // internally: set(access(t,i), i + 3.14)
   }
   double s = 0.;
   for (int i = 0; i < N; i++) {
      s += t[i];  // internally: set(s, app(+, [get(s); get(access(t,i))]))
   }
   printf("%f\n", s);
}

int other(float* u) {
   for (int i = 0; i < N; i++) {
      float a = u[i];
      float b = 3.14 * a + 0.68;
      u[i] = b;
   }
}
