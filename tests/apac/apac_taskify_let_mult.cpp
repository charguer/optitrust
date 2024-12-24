#include <stdlib.h>

void f(int * tab) { tab[0] += 42; }

void g(const int * tab) { }

void h(const int * const tab) { }

void p(int * v) { int a = 15; int b = a + 2; int c = a + b + (*v)++; }

void foo(int * bar, int val) {
  int i, j = 0;
  int * a, ** b;
  const int * c, ** d, * const e = NULL;
  f(bar);
  for(i = 0; i < val; i++) {
    bar[i] += 2;
    p(&a[i]);
    p(&a[i]);
  }
  g(c);
  h(e);
}
