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

int reify_with_resources(int* p) {
  __modifies("p ~> Cell");
  int rei;
  int rej;
  int rek;
  const int x = 1;
  int y = 2;
  rei = __ARITH(x + x, "Sum{(1,{b}); (1,{b})} where {{b: x}}");
  rei = __ARITH(x + y, "Sum{(1,{d}); (1,{c})} where {{d: x;  c: *y}}");
  rei = __ARITH(y + y, "Sum{(1,{e}); (1,{e})} where {{e: *y}}");
  rei = __ARITH(0 * eff(p), "Prod{(1,0); (1,{f})} where {{f^NR^ND: eff(p)}}");
  rei = __ARITH(f(1) + f(1), "Sum{(1,{g}); (1,{g})} where {{g: f(1)}}");
  rei = __ARITH(
      f(1) + g(1) + x * y + eff(p) * f(1) * x * y,
      "Sum{(1,Sum{(1,Sum{(1,{j}); (1,{l})}); (1,Prod{(1,{i}); (1,{h})})}); "
      "(1,Prod{(1,Prod{(1,Prod{(1,{k}); (1,{j})}); (1,{i})}); (1,{h})})} where "
      "{{l: g(1);  k^NR^ND: eff(p);  j: f(1);  i: x;  h: *y}}");
  rej = __ARITH(f(1) + g(1) + x * y + eff(p) * f(1) * x * y,
                "Sum{(1,{o: f(1)}); (1,{q: g(1)}); (1,Prod{(1,{n: x}); (1,{m: "
                "*y})}); (1,Prod{(1,{p^NR^ND: eff(p)}); (1,{o: f(1)}); (1,{n: "
                "x}); (1,{m: *y})})}");
  rek = x + y + y;
}

int simple() {
  __pure();
  const double a = 1;
  const double b = 2;
  int ra;
  ra = 2 * b;
}

int main() {
  __pure();
  double a;
  double b;
  double c;
  double d;
  double e;
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
  y = 4;
  z = a * a;
  z = 1. / b;
  t = 6 * a;
  t = 2 * a;
  t = 6 * a * b / a;
  t = 1. / b;
  t = exact_div(4 * a, 2);
  t = exact_div(a, (2 * 3));
  u = a * b + a * c;
  u = 2 * b + 2 * c;
  u = exact_div(2 * a, 3) + 2 * 3;
  u = a + 2 * 3;
  u = a * c + a * d + b * c + b * d;
  u = 5 * a * 2 * b + 5 * a * 3 * c + 5 * a * 4 * d;
  u = a + a * b + a * c;
  v = c / (d * d * e);
  v = b * c;
  v = a + d;
  v = d;
  v = d;
  v = a + c;
  w = a + a * 3 + f(b * f(c + c) / b);
  float f;
  f = -2.f / 12.f;
  f = 0.f;
  int ls;
  ls = x + 2;
  ls = x;
  for (int ls2 = 2; ls2 < 12; ls2++) {
    __strict();
    __smodifies("&ls ~> Cell");
    ls = 10;
  }
  int n;
  int m;
  int p;
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
  p = 4;
  p = 4;
  p = m - 2;
  int ci;
  ci = 10;
  ci = 4 + n + n;
  ci = 5;
  ci = 8 * n;
  ci = 4;
  ci = 1;
  ci = 2;
  ci = 0;
  ci = 11;
  double cd;
  cd = 10.4;
  cd = 0.584893048128;
  return 0;
}

void more_ops() {
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
  int* arr = (int*)MALLOC1(exact_div(1024, 32), sizeof(int));
  free(arr);
  arr = (int*)malloc(sizeof(int[exact_div(1024, 32)]));
  for (int q = 0; q < exact_div(1024, 32); q++) {
    arr[q] = 0;
  }
  free(arr);
}
