#include <optitrust.h>

int f(int x) {
  __pure();
  return x;
}

int g(int x) {
  __pure();
  return x+1;
}

int eff(int* p) {
  __modifies("p ~> Cell");
  //*p++;
  return 0;
}

void simpl_in_depth () {
  __pure();
  // FIXME: simplified to 2 * g(5), could be wrong if not pure.
  int x = g(1 + 2 + f(2 + 4)) + g(5) + g(5);
  int y = g(1 + 2 + f(2 + 4)) + g(5) + g(5);
}

/* LATER
int reify_without_resources(int* p) {
  int rei;
  int a = 1;
  const int b = 2;
  rei = f(1) + g(1) + a * b + eff(p) * f(1) * a * b;
}
*/

void reify_with_resources(int* p) {
  __modifies("p ~> Cell");
  int rei; int rej; int rek;
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
  ra = a + b + b - a; // simplifiable because redundant
}

void test_int(int a, int b, int c, int d, int e) {
  __pure();
  int s; int t; int u; int v; int w; int x; int y; int z;

  // test parsing and normalizing
  x = 2 + 2;
  x = 2 * b;
  x = a + b * 2 - (3 * c) - d;
  x = a + b - a;
  x = a + ((a + 2*b) + 1);
  x = 3 + 5 + a + (a + b*2 - 3*c);
  x = 3 * 5 * a * b * b;
  x = a / (b * b* c) / d;

  // test gather sum
  y = 1 + 2; // = 3
  y = a + a + a - a; // = 2*a
  y = 2*a - 3*a + b - 5*b; // = - a -4*b
  y = 3*a + 2*b - b + c - 4*a ; // = -a + b + c
  y = (2 + a) - (a + -2); // = 4

  // TODO: arithmetic computations with operations on constants

  // test gather prod
  z = a * a * a / a; // = a * a
  z = a / b / a * b / b; // = 1 / b

  // test gather nested
  t = (a + (3 + 2)*a); // = 6 * a
  t = a + b * a / b; // = 2 * a
  t = (a + (3 + 2)*a) / a * b; // = 6 * b
  t = a / (b * a * b) / (1 / b); // = 1. / b
  t = 4 * exact_div(a, 2); // = exact_div(4 * a, 2)
  t = exact_div(exact_div(a, 2), 3); // = exact_div(a, 2 * 3)

  // test expand
  u = a * (b + c); // = a * b + a * c
  u = 2 * (b + c); // = 2 * b + 2 * c
  u = 2 * (exact_div(a, 3) + 3); // = exact_div(2 * a, 2) + 2 * 3
  u = 2 * (exact_div(a, 2) + 3); // = a + 2 * 3
  u = (a + b) * (c + d); // = a * c + a * d + b * c + b * d
  u = 5*a * (2*b + 3*c + 4*d);
  u = a + a * (b + c); // = a + a * b + c

  // test expand from pic demo
  v = (a / b * c / (a * d * d / b * e)); // = c / (d * d * e)
  v = (a * b * c / a); // = b * c
  v = ((a / (b / c)) + d * c / b) * (b / c); // = a + d
  v = ((a * ((b / c) * (d / ((((b * a) * a) / c) / e)))) * (a / e)); // = d
  v = (((a * d) / (a / e)) * (1 / e)); // = d
  v = (((a / (1 / e)) + (b * (c / (b / e )))) * (1 / e)); // = a + c

  // test for recursion in atoms (LATER)
  w = a + a * 3 + f(b * f(c + c) / b); // = 4*a + f(f(2*c

  int p;
  // test integer addition
  p = (2 + a) - (a + -2); // = 4
  p = 4 + b - b; // = 4
  p = a + -2;

  // compute int
  int ci;
  ci = 5 + 5; // = 10
  ci = 3 + a + 1 + a; // = n + n + 4
  ci = 2 * 3 - 1; // = 5
  ci = a * 4 * 2; // = 8 * n

  // compute int div
  ci = exact_div(8, 2); // = 4
  ci = exact_div(exact_div(12,3), 4); // = 1
  ci = 8 / 3; // = 2
  ci = (8 / 3) / 3; // = 0
  ci = 2 * 3 - 1 + exact_div(8, 2) + 8 / 3; // = 11

  // LATER: compute casts from/to int/double
}

void test_double(double a, double b, double c, double d, double e) {
  __pure();
  double s; double t; double u; double v; double w; double x; double y; double z;

  // test parsing and normalizing
  x = 2. + 2.;
  x = 2. * b;
  x = a + b * 2. - (3. * c) - d;
  x = a + ((a + 2.*b) + 1.);
  x = 3. + 5. + a + (a + b*2. - 3.*c);
  x = 3. * 5. * a * b * b;
  x = a / (b * b * c) / d;

  // test gather sum
  y = 1. + 2.; // = 3
  y = 1. + 3.2 + 4. + 5.2; // =
  y = a + a + a - a; // = 2*a
  y = 2.*a - 3.*a + b - 5.*b; // = - a -4*b
  y = 3.*a + 2.*b - b + c - 4.*a ; // = -a + b + c
  y = (2. + a) - (a + -2.); // = 4

  // TODO: arithmetic computations with operations on constants

  // test gather prod
  z = a * a * a / a; // = a * a
  z = a / b / a * b / b; // = 1 / b

  // test gather nested
  t = (a + (3. + 2.) * a); // = 6 * a
  t = a + b * a / b; // = 2 * a
  t = (a + (3. + 2.)*a) / a * b; // = 6 * b
  t = a / (b * a * b) / (1. / b); // = 1. / b

  // test expand
  u = a * (b + c); // = a * b + a * c
  u = 2. * (b + c); // = 2 * b + 2 * c
  u = (a + b) * (c + d); // = a * c + a * d + b * c + b * d
  u = 5.*a * (2.*b + 3.*c + 4.*d);
  u = a + a * (b + c); // = a + a * b + c

  // test expand from pic demo
  v = (a / b * c / (a * d * d / b * e)); // = c / (d * d * e)
  v = (a * b * c / a); // = b * c
  v = ((a / (b / c)) + d * c / b) * (b / c); // = a + d
  v = ((a * ((b / c) * (d / ((((b * a) * a) / c) / e)))) * (a / e)); // = d
  v = (((a * d) / (a / e)) * (1. / e)); // = d
  v = (((a / (1. / e)) + (b * (c / (b / e )))) * (1. / e)); // = a + c
                                                            //
  double cd;
  cd = 5.2 + 5.2; // = 10.4
  cd = (10.5 / 2.2) / (2.4 * 3.4); // = .584893048
}

void test_float() {
  __pure();
  float f;
  f = -2.f / 12.f;
  f = 0.0f * f;
}

void loop_shift(int x) {
  __pure();
  int ls;
  // test for loop shift
  ls = 0 + x - (-2); // = x + 2
  ls = 0 + x; // = x
  for (int ls2 = 0 + 2; ls2 < 10 + 2; ls2++) {
    ls = 0 + 12 + (-2); // = 10
  }
}

void integer_division(int n, int m, int p) {
  __pure();
  int q; int eu; int eur;
  q = exact_div(5 + 5, 2); // = 5
  q = exact_div(n * m, n); // = m, would also be true if non-exact division
  q = exact_div(n, m) * m; // = n, true because when b divides a
  q = exact_div(exact_div(n, m), p); // = exact_div(n, m * p)
  q = exact_div(exact_div(n, m), p) * m; // = exact_div(n, p)
  q = exact_div(exact_div(n, m), p) * (m * p); // = n
  q = exact_div((n * m * 4 * 32), m); // = n * 4 * 32
  q = exact_div((n * m * 4 * 32), 4); // = n * m * 32
  q = (n / m) / p; // n / (m * p);
  q = (n * m) / n; // = m
  q = (m * n) / n; // = m
  q = (m * n) / (n * n); // = m / n
  q = (m * n * n) / (n * n); // = m
  q = (n * m * n) / (n * n); // = m
  q = (n * m * n) / (n * n * n); // = m / n
  q = (n * m * n * n) / (n * n * n * n); // = m / n
  q = (n * m * n * n) / (n * n * n * n * n); // = m / (n * n)
  q = (n * m * n) / (p * n); // = (n * m) / p
  q = (n / m) * m; // cannot simplify because n might not be divisible by m
  q = (n / 1) / 1;  // = n

  // test euclidian
  eu = (n / m) * m + (n % m); // = n
  eu = m + (n % m) + m * (n / m); // = m + n
  eu = n % m + (2 + n / m) * m; // = n + 2 * m
  eur = m % n + (1 + m / n) * n; // = m + n
  eur = (exact_div((n - m), p) + q) * p + m; // = n + p * q

}

void more_ops() {
  __pure();
  int ci;
  ci = 1 << 4; // = 16
  ci = 1024 >> 4; // = 64
  ci = 14 ^ 5; // = 11 because 1110b ^ 101b = 1011b
  ci = 14 | 5; // = 15
  ci = 14 & 5; // = 4
  ci = 15 % 4; // = 3
}

void impurity(int* p) {
  __modifies("p ~> Cell");
  int re = 0;
  int rf = 0;
  int const a = 1;
  int const b = 1;
  // re = eff(p) + eff(p); // does not typecheck because overlapping effects
  re = 0 * eff(p); // cannot be simplified because non-deletable
  rf = (a + b) * eff(p); // cannot expand because non-redundant
}


void purity(int* p) {
  __pure();
  int re = 0;
  int rf = 0;
  int const a = 1;
  int const b = 1;
  re = f(1) + f(1) + a + a; // = 2*f(1) + 2*a // ok because redundant
  re = 0 * f(1) + 0 * a; // = 0 // ok because deletable
  rf = (a + b) * f(1); // = a * f(1) + b * f(1) // ok because redundant
}

void alloc() {
  __pure();
  int* const arr = MALLOC1(int, exact_div(1024, 32));
  for (int q = 0; q < exact_div(1024, 32); q++) { // q < 32
    __xwrites("&arr[MINDEX1(exact_div(1024, 32), q)] ~> Cell");
    arr[MINDEX1(exact_div(1024, 32), q)] = 0;
  }
  free(arr);
}

