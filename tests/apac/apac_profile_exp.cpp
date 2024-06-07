#include <stdlib.h>

#include "apac_profiler.hpp"
void f(int* tab) { tab[0] += 42; }

void g(const int* const tab) {}

void h(const int* const tab) {}

void p(int v) {
  int a = 15;
  int b = a + 2;
  int c = a + b + v++;
}

void r(int v, const int z) { int a = 15 + z, b = a + 2, c = a + b + v++; }

void c(int* tab, const int size) {
  /*@__apac_task_group*/ {
    ApacProfilerSection apac_profsection1("14-14", 0, 1);
    apac_profsection1.addParam("tab", tab);
    apac_profsection1.beforeCall();
    f(tab);
    apac_profsection1.afterCall();
    int i;
    ApacProfilerSection apac_profsection5("apac_profsection5", 1, 2);
    apac_profsection5.addParam("size", size);
    apac_profsection5.addParam("i", i);
    apac_profsection5.addParam("tab", tab);
    apac_profsection5.beforeCall();
    for (i = 0; i < size; i++) {
      ApacProfilerSection apac_profsection2("17-17", 1, 1);
      apac_profsection2.addParam("i", i);
      apac_profsection2.addParam("tab", tab);
      apac_profsection2.beforeCall();
      tab[i] += 2;
      apac_profsection2.afterCall();
      ApacProfilerSection apac_profsection3("18-18", 1, 1);
      apac_profsection3.addParam("i", i);
      apac_profsection3.addParam("tab", tab);
      apac_profsection3.beforeCall();
      p(tab[i]);
      apac_profsection3.afterCall();
      ApacProfilerSection apac_profsection4("19-19", 1, 1);
      apac_profsection4.addParam("i", i);
      apac_profsection4.addParam("tab", tab);
      apac_profsection4.beforeCall();
      p(tab[i]);
      apac_profsection4.afterCall();
    }
    apac_profsection5.afterCall();
    ApacProfilerSection apac_profsection6("21-21", 1, 0);
    apac_profsection6.addParam("tab", tab);
    apac_profsection6.beforeCall();
    g(tab);
    apac_profsection6.afterCall();
    ApacProfilerSection apac_profsection7("22-22", 1, 0);
    apac_profsection7.addParam("tab", tab);
    apac_profsection7.beforeCall();
    h(tab);
    apac_profsection7.afterCall();
  __apac_exit:;
  } /*__apac_task_group@*/
}

int main() {
  int* t = (int*)malloc(4 * sizeof(int));
  c(t, 4);
  free(t);
  return 0;
}
