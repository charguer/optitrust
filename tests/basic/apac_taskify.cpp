#include <stdlib.h>

void f(int * tab) { tab[0] += 42; }

void g(int * tab) { }

void h(int * tab) { }

void p(int v) { int a = 15; int b = a + 2; int c = a + b + v++; }

void r(int v, int z) { int a = 15 + z, b = a + 2, c = a + b + v++; }

void c(int * tab, int size) {
  f(tab);
  int i;
  for(i = 0; i < size; i++) {
    tab[i] += 2;
    p(tab[i]);
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
