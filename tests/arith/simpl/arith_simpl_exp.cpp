#include <optitrust.h>

int f(int x) {
  __pure();
  return x;
}

int g(int x) {
  __pure();
  return x + 1;
}

int eff(int* p) {
  __modifies("p ~> Cell");
  return 0;
}

void simpl_in_depth() {
  __pure();
  int x = g(3 + f(6)) + 2 * g(5);
  int y = g(3 + f(6)) + 2 * g(5);
}

void reify_with_resources(int* p) {
  __modifies("p ~> Cell");
  int rei;
  int rej;
  int rek;
  const int x = 1;
  int y = 2;
  rei = x + x;
  rei = x + y;
  rei = y + y;
  rei = 0 * eff(p);
  rei = f(1) + f(1);
  rei = f(1) + g(1) + x * y + eff(p) * f(1) * x * y;
  rej = f(1) + g(1) + x * y + eff(p) * f(1) * x * y;
  rek = x + y + y;
}

void simple() {
  __pure();
  const int a = 1;
  const int b = 2;
  int ra;
  ra = 2 * b;
}

void test_int(int a, int b, int c, int d, int e) {
  __pure();
  int s;
  int t;
  int u;
  int v;
  int w;
  int x;
  int y;
  int z;
  x = 2 + 2;
  x = 2 * b;
  x = a + 2 * b - 3 * c - d;
  x = a + b - a;
  x = a + a + 2 * b + 1;
  x = 3 + 5 + a + a + 2 * b - 3 * c;
  x = 3 * 5 * a * b * b;
  x = a / (b * b * c) / d;
  y = 3;
  y = 2 * a;
  y = -a - 4 * b;
  y = -a + b + c;
  y = 4;
  z = a * a;
  z = a / b / a;
  t = 6 * a;
  t = 2 * a;
  t = (a + (3 + 2) * a) / a * b;
  t = a / (b * a * b * (1 / b));
  t = exact_div(4 * a, 2);
  t = exact_div(a, (2 * 3));
  u = a * b + a * c;
  u = 2 * b + 2 * c;
  u = exact_div(2 * a, 3) + 2 * 3;
  u = a + 2 * 3;
  u = a * c + a * d + b * c + b * d;
  u = 5 * a * 2 * b + 5 * a * 3 * c + 5 * a * 4 * d;
  u = a + a * b + a * c;
  v = a / b * c / (a * d * d / b * e);
  v = b * c;
  v = a / (b / c) * (b / c) + d * c / b * (b / c);
  v = a * (b / c) * (d / (b * a * a / (c * e))) * (a / e);
  v = a * d / (a / e) * (1 / e);
  v = a / (1 / e) * (1 / e) + b * (c / (b / e)) * (1 / e);
  w = a + a * 3 + f(b * f(c + c) / b);
  int p;
  p = 4;
  p = 4;
  p = a - 2;
  int ci;
  ci = 10;
  ci = 4 + a + a;
  ci = 5;
  ci = 8 * a;
  ci = 4;
  ci = 1;
  ci = 2;
  ci = 0;
  ci = 11;
}

void test_double(double a, double b, double c, double d, double e) {
  __pure();
  double s;
  double t;
  double u;
  double v;
  double w;
  double x;
  double y;
  double z;
  x = 2. + 2.;
  x = 2. * b;
  x = a + b * 2. - 3. * c - d;
  x = a + a + 2. * b + 1.;
  x = 3. + 5. + a + a + b * 2. - 3. * c;
  x = 3. * 5. * a * b * b;
  x = a / (b * b * c * d);
  y = 1. + 2.;
  y = 1. + 3.2 + 4. + 5.2;
  y = 2 * a;
  y = 2. * a - 3. * a + b - 5. * b;
  y = 3. * a + 2. * b - b + c - 4. * a;
  y = 2 * 2.;
  z = a * a;
  z = 1. / b;
  t = a + (3. + 2.) * a;
  t = 2 * a;
  t = (a + (3. + 2.) * a) * b / a;
  t = 1. / b;
  u = a * b + a * c;
  u = 2. * b + 2. * c;
  u = a * c + a * d + b * c + b * d;
  u = 5. * a * 2. * b + 5. * a * 3. * c + 5. * a * 4. * d;
  u = a + a * b + a * c;
  v = c / (d * d * e);
  v = b * c;
  v = a + d;
  v = d;
  v = d;
  v = a + c;
  double cd;
  cd = 10.4;
  cd = 0.584893048128;
}

void test_float() {
  __pure();
  float f;
  f = -2.f / 12.f;
  f = 0.f;
}

void loop_shift(int x) {
  __pure();
  int ls;
  ls = x + 2;
  ls = x;
  for (int ls2 = 2; ls2 < 12; ls2++) {
    __strict();
    __smodifies("&ls ~> Cell");
    ls = 10;
  }
}

void integer_division(int n, int m, int p) {
  __pure();
  int q;
  int eu;
  int eur;
  q = 5;
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
  eu = n;
  eu = n + m;
  eu = n + 2 * m;
  eur = m + n;
  eur = n + q * p;
}

void more_ops() {
  __pure();
  int ci;
  ci = 16;
  ci = 64;
  ci = 11;
  ci = 15;
  ci = 4;
  ci = 3;
}

void impurity(int* p) {
  __modifies("p ~> Cell");
  int re = 0;
  int rf = 0;
  const int a = 1;
  const int b = 1;
  re = 0 * eff(p);
  rf = (a + b) * eff(p);
}

void purity(int* p) {
  __pure();
  int re = 0;
  int rf = 0;
  const int a = 1;
  const int b = 1;
  re = 2 * f(1) + 2 * a;
  re = 0;
  rf = a * f(1) + b * f(1);
}

void alloc() {
  __pure();
  int* const arr = (int*)malloc(MSIZE1(exact_div(1024, 32)) * sizeof(int));
  for (int q = 0; q < exact_div(1024, 32); q++) {
    __strict();
    __xwrites("&arr[MINDEX1(exact_div(1024, 32), q)] ~> Cell");
    arr[MINDEX1(exact_div(1024, 32), q)] = 0;
  }
  free(arr);
}
