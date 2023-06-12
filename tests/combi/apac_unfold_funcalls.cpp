#include <stdlib.h>

int f(int i) {
  return i;
}

int g(int i, int j) {
  return i+j;
}

void e(int i) {
  return;
}

int * q (int v) {
  int * val = (int *) malloc(sizeof(int));
  *val = v;
  return val;
}

void z(int * a, int * b) {
  *a = 2 * (*b);
  return;
}

void h() {
  const int a = f(1);
  int b;
  b = f(f(1));
  int c = g(f(1), f(2));
  e(f(2));
  z(q(5), &c);
}