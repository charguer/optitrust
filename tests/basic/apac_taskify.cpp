#include <stdlib.h>

void f(int * tab) { tab[0] += 42; }

void g(int * tab) { }

void h(int * tab) { }

void p(int v) { }

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
