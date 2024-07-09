#include <stdio.h>

const int N = 5;

int main() {
   double t[N];
   for (int i = 0; i < N; i++) {
      t[i] = i + 3.14; // internally: set(access(t,i), i + 3.14)
   }
   double s = 0;
   for (int i = 0; i < N; i++) {
      s += t[i];  // internally: set(s, app(+, [get(s); get(access(t,i))]))
   }
   printf("%f\n", s);
}

float* u;

void other() {
   for (int i = 0; i < N; i++) {
      double a = u[i];
      double b = 3.14 * a + 0.68;
      u[i] = b;
   }
}
