#include <stdlib.h>

int f(int i) { return i; }

int g(int i, int j) { return i + j; }

void e(int i) { return; }

int* q(int v) {
  int* val = (int*)malloc(sizeof(int));
  *val = v;
  return val;
}

void z(int* a, int* b) {
  *a = 2 * *b;
  return;
}

void h() {
  int __var_72;
  __var_72 = f(1);
  const int a = __var_72;
  int b;
  int __var_74;
  __var_74 = f(1);
  b = f(__var_74);
  int __var_76;
  __var_76 = f(1);
  int __var_78;
  __var_78 = f(2);
  int __var_80;
  __var_80 = g(__var_76, __var_78);
  int c = __var_80;
  int __var_82;
  __var_82 = f(2);
  e(__var_82);
  int* __var_84;
  __var_84 = q(5);
  z(__var_84, &c);
}
