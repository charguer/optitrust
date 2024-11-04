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
  int a;
  a = f(1);
  int b;
  int __apac_var1;
  __apac_var1 = f(1);
  b = f(__apac_var1);
  int __apac_var2;
  __apac_var2 = f(1);
  int __apac_var3;
  __apac_var3 = f(2);
  int c;
  c = g(__apac_var2, __apac_var3);
  int __apac_var4;
  __apac_var4 = f(2);
  e(__apac_var4);
  int* __apac_var5;
  __apac_var5 = q(5);
  z(__apac_var5, &c);
}
