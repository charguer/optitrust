#include <stdio.h>

int i, j;

void bar(int * a) { (*a)++; }

void rab(int a) { a++; }

void oof() { printf("%d\n", j); }

void func() {
  i += 1;
  rab(i++);
}

void foo() {
  func();
  func();
  bar(&i);
  i--;
  i = j + 5;
}
