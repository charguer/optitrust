#include <stdio.h>

int main() {
   const int N = 5;

   double t[N];

   for (int i = 0; i < N; i++) {
      t[i] = i + 3.14; // internally: set(access(t,i), i + 3.14)
   }
   double s = 0;
   for (int i = 0; i < N; i++) {
      s += t[i];  // internally: set(s, app(+, [get(s); get(access(t,i))]))
   }
   printf("%d\n", s);
}


float* u;

int other() {
   const int N = 5;
   for (int i = 0; i < N; i++) {
      double a = u[i];
      double b = 3.14 * a + 0.68;
      u[i] = b;
   }
}

/* TODO: acceleration

void accel(int n, double* x, double* v, int m, double* e, double dt, int d, double q) {
  // modifies x ~> Array(n)
  // reads e ~> Array(m)
  for (int t = 0; t < d; t++) {
    for (int i = 0; i < n; i++) {
      int c = (((int) x[i]) + m) % m;
      double a = q / m * e[c];
      v[i] += a * dt;
      x[i] += v[i] * dt;
    }
  }
}

void accel(int n, double* x, double* v, int m, double* e, double dt, int d, double q) {
  double* V = malloc(..);
  for (int i = 0; i < n; i++) {
    V[i] = v[i] * dt;
  }
  double k = q / m * dt * dt;
  for (int t = 0; t < d; t++) {
    for (int i = 0; i < n; i++) {
      int c = (((int) x[i]) + m) % m;
      double a = k * e[c];
      V[i] += a;
      x[i] += V[i];
    }
  }
  for (int i = 0; i < n; i++) {
    v[i] = V[i] / dt;
  }
}

*/
