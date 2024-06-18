#include <optitrust.h>

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
  x = (double)(2 + 2);
  x = (double)2 * b;
  x = a + b * (double)2 - (double)3 * c - d;
  x = a + a + (double)2 * b + (double)1;
  x = (double)3 + 5. + a + a + b * (double)2 - (double)3 * c;
  x = (double)3 * 5. * a * b * b;
  x = a / (b * b * c * d);
  y = (double)(1 + 2);
  y = (double)1 + 3.2 + (double)4 + 5.2;
  y = (double)2 * a;
  y = (double)2 * a - (double)3 * a + b - (double)5 * b;
  y = (double)3 * a + (double)2 * b - b + c - (double)4 * a;
  y = (double)2 - (double)-2;
  z = a * a;
  z = 1. / b;
  t = a + (double)(3 + 2) * a;
  t = (double)2 * a;
  t = (a + (double)(3 + 2) * a) * b / a;
  t = 1. / (b * (double)1);
  u = a * b + a * c;
  u = a * c + a * d + b * c + b * d;
  u = (double)5 * a * (double)2 * b + (double)5 * a * (double)3 * c +
      (double)5 * a * (double)4 * d;
  v = c / (d * d * e);
  v = b * c;
  v = a + d;
  v = d;
  v = d;
  v = a + c;
  w = a + a * (double)3 + (double)f((int)(b * (double)f((int)(c + c)) / b));
  float f;
  f = -2.f / 12.f;
  f = 0.f;
  int ls;
  ls = (int)((double)0 + x - (double)-2);
  ls = (int)((double)0 + x);
  for (int ls2 = 2; ls2 < 12; ls2++) {
    ls = 10;
  }
  int n, m, p, q;
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
  p = 4;
  p = 4;
  p = m - 2;
  int* arr = (int*)MALLOC1(32, sizeof(int));
  free(arr);
  arr = (int*)malloc(sizeof(int[exact_div(1024, 32)]));
  for (q = 0; q < 32; q++) {
    arr[q] = 0;
  }
  free(arr);
  int ci;
  ci = 10;
  ci = 4 + n + n;
  ci = 5;
  ci = 16;
  ci = 64;
  ci = 11;
  ci = 15;
  ci = 4;
  ci = 3;
  ci = 8 * n;
  ci = 4;
  ci = 1;
  ci = 2;
  ci = 0;
  ci = 11;
  double cd;
  cd = 10.4;
  cd = 6.6 + a + (double)1 + a + (double)3;
  cd = 2.5 * (double)3 - (double)(10 / 4);
  cd = 2.5 * (double)3 - 0.25 * (double)10;
  cd = 0.584893048128;
  return 0;
}
