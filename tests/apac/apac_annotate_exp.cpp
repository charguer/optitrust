#include <stdlib.h>

#include "apac_profiling.hpp"
void f(int* tab) { tab[0] += 42; }

void g(const int* tab) {}

void h(const int* tab) {}

void p(int v) {
  int a = 15;
  int b = a + 2;
  int c = a + b + v++;
}

void r(int v, int z) { int a = 15 + z, b = a + 2, c = a + b + v++; }

void c(int* tab, int size) {
  {
    apac_s __apac_section17;
    apac_s __apac_section16;
    apac_s __apac_section22;
    apac_s __apac_section21;
    apac_s __apac_section12;
    __apac_section12.initialize("12");
    __apac_section12.before();
    f(tab);
    __apac_section12.after();
    int i;
    for (i = 0; i < size; i++) {
      tab[i] += 2;
      __apac_section21.initialize("21");
      __apac_section21.add(tab[i]);
      __apac_section21.before();
      p(tab[i]);
      __apac_section21.after();
      __apac_section22.initialize("22");
      __apac_section22.add(tab[i]);
      __apac_section22.before();
      p(tab[i]);
      __apac_section22.after();
    }
    __apac_section16.initialize("16");
    __apac_section16.before();
    g(tab);
    __apac_section16.after();
    __apac_section17.initialize("17");
    __apac_section17.before();
    h(tab);
    __apac_section17.after();
  __apac_exit:;
  }
}

int main() {
  apac_s __apac_section_main;
  __apac_section_main.initialize("__apac_section_main");
  __apac_section_main.before();
  int* t = (int*)malloc(4 * sizeof(int));
  c(t, 4);
  free(t);
  __apac_section_main.after();
  return 0;
}
