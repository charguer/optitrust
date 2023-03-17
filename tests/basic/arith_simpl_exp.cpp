#include "../../include/optitrust.h"

int f(int x) { return x; }

int g(int x) { return x + 1; }

void simpl_in_depth() {
  int x = g(3 + f(6)) + 2 * g(5);
  int y = g(3 + f(6)) + 2 * g(5);
}

int main() {
  double a;
  double b;
  double c;
  double d;
  double e;
  double r;
  double s;
  double t;
  double u;
  double v;
  double w;
  double x;
  double y;
  double z;
  x = 2 + 2;
  x = 2 * b;
  x = a + 2 * b - 3 * c - d;
  x = a + a + 2 * b + 1;
  x = 3 + 5. + a + a + 2 * b - 3 * c;
  x = 3 * 5. * a * b * b;
  x = a / (b * b * c * d);
  y = 3;
  y = 5 + 3.2 + 5.2;
  y = 2 * a;
  y = -a - 4 * b;
  y = -a + b + c;
  z = a * a;
  z = 1. / b;
  t = 6 * a;
  t = 2 * a;
  t = 6 * b;
  t = 1. / b;
  u = a * b + a * c;
  u = a * c + a * d + b * c + b * d;
  u = 5 * a * 2 * b + 5 * a * 3 * c + 5 * a * 4 * d;
  v = c / (d * d * e);
  v = b * c;
  v = a + d;
  v = d;
  v = d;
  v = a + c;
  w = a + a * 3 + f(b * f(c + c) / b);
  int ls;
  ls = x + 2;
  ls = x;
  for (int ls2 = 2; ls2 < 12; ls2++) {
    ls = 10;
  }
  int n, m, p, q;
  q = exact_div(10, 2);
  q = m;
  q = n;
  q = exact_div(n, (m * p));
  q = exact_div(n, p);
  q = n;
  q = n * 4 * 32;
  q = n * m * 32;
  q = n / (m * p);
  q = m;
  q = m;
  q = m / n;
  q = m;
  q = m;
  q = m / n;
  q = m / n;
  q = m / (n * n);
  q = n * m / p;
  q = n / m * m;
  q = n;
  return 0;
}
