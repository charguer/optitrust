#include <stdlib.h>

void f(int * tab) { tab[0] += 42; }

void g(int * tab) { }

void h(int * tab) { }

void p(int * v) { int a = 15; int b = a + 2; int c = a + b + (*v)++; }

void r(int v, int z) { int a = 15 + z, b = a + 2, c = a + b + v++; }

int w(int a) {  return a + 1; }

void c(int * tab, int size) {
  int i;
  int l = size;
  int r, s = 1;
  f(tab);
  for(i = 0; i < size; i++) {
    tab[i] += 2;
    p(&tab[i]);
    p(&tab[i]);
  }
  int * p = tab;
  while (l > 1 || s) {
    r = w(l);
    *p = r;
    p++;
    l /= r;
    s = 0;
  }
  g(tab);
  h(tab);
}

int main() {
  int * t = (int *) malloc(4*sizeof(int)); 
  c(t, 4);
  free(t);
  return 0;
}
