#include "../../include/optitrust.h"

int f(int x) {
  return x;
}

int g(int x) {
  return x+1;
}

void simpl_in_depth () {
  // FIXME: simplified to 2 * g(5), could be wrong if not pure.
  int x = g(1 + 2 + f(2 + 4)) + g(5) + g(5);
  int y = g(1 + 2 + f(2 + 4)) + g(5) + g(5);
}

int main()
{
  double a; double  b; double  c; double d; double  e;
  double r; double  s; double t; double  u; double  v; double w; double  x; double  y; double z;

  // test parsing and normalizing
  x = 2 + 2;
  x = 2 * b;
  x = a + b * 2 - (3 * c) - d;
  x = a + ((a + 2*b) + 1);
  x = 3 + 5.0 + a + (a + b*2 - 3*c);
  x = 3 * 5.0 * a * b * b;
  x = a / (b * b* c) / d;

  // test gather sum
  y = 1 + 2; // = 3
  y = 1 + 3.2 + 4 + 5.2; // = 5 + 3.2 + 5.2
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

  // test expand
  u = a * (b + c); // = a * b + a * c
  u = (a + b) * (c + d); // = a * c + a * d + b * c + b * d
  u = 5*a * (2*b + 3*c + 4*d);

  // test expand from pic demo
  v = (a / b * c / (a * d * d / b * e)); // = c / (d * d * e)
  v = (a * b * c / a); // = b * c
  v = ((a / (b / c)) + d * c / b) * (b / c); // = a + d
  v = ((a * ((b / c) * (d / ((((b * a) * a) / c) / e)))) * (a / e)); // = d
  v = (((a * d) / (a / e)) * (1. / e)); // = d
  v = (((a / (1. / e)) + (b * (c / (b / e )))) * (1. / e)); // = a + c


  // test for recursion in atoms (LATER)
  w = a + a * 3 + f(b * f(c + c) / b); // = 4*a + f(f(2*c))

  // test for loop shift
  int ls;
  ls = 0 + x - (-2); // = x + 2
  ls = 0 + x; // = x
  for (int ls2 = 0 + 2; ls2 < 10 + 2; ls2++) {
    ls = 0 + 12 + (-2); // = 10
  }

  // test integer division
  int n, m, p, q;
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

  // test integer addition
  p = (2 + m) - (m + -2); // = 4
  p = 4 + n - n; // = 4

  int* arr = (int*) MALLOC1(exact_div(1024, 32), sizeof(int));
  free(arr);
  arr = (int*) malloc(sizeof(int[exact_div(1024, 32)]));
  for (q = 0; q < exact_div(1024, 32); q++) { // q < 32
    arr[q] = 0;
  }
  free(arr);

  // compute int
  int ci;
  ci = 5 + 5; // = 10
  ci = 3 + n + 1 + n; // = n + n + 4
  ci = 2 * 3 - 1; // = 5
  ci = 1 << 4; // = 16
  ci = 1024 >> 4; // = 64
  ci = 14 ^ 5; // = 11 because 1110b ^ 101b = 1011b
  ci = 14 | 5; // = 15
  ci = 14 & 5; // = 4
  ci = 15 % 4; // = 3
  ci = n * 4 * 2; // = n * 8

  // compute int div
  ci = exact_div(8, 2); // = 4
  ci = exact_div(exact_div(12,3), 4); // = 1
  ci = 8 / 3; // = 2
  ci = (8 / 3) / 3; // = 0
  ci = 2 * 3 - 1 + exact_div(8, 2) + 8 / 3; // = 11

  // compute double
  double cd;
  cd = 5.2 + 5.2; // = 10.4
  cd = 3.2 + a + 1 + a + 3 + 3.4; // = a + a + 10.6
  cd = 2.5 * 3 - 10 / 4; // = 5.5  (because using integer division)
  cd = 2.5 * 3 - 10 / 4.0; // = 5.0  (because using float division)
  cd = (10.5 / 2.2) / (2.4 * 3.4); // = .584893048

  // LATER: compute casts from/to int/double


  return 0;
}

