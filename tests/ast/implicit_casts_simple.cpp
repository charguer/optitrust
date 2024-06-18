#include <optitrust.h>

int add(int a, int b) {
  return a + b;
}

int main() {
  int k = 1;
  int a = add(k, k);
  float x = a;
  x + 1;
  int b = x;
  int c = (int)x;
  int y = (float)c;
  if (1) {
    int a = true;
  }
}

void bar(size_t s) {}

void foo() {
  int a = 0;
  bar(a);
  size_t b = a;
  int c = a + b;
}

void ghost(float* A, int n) {
  __modifies("A ~> Matrix1(n)");
  __GHOST_BEGIN(rev_ghost, matrix1_ro_focus, "");
  __GHOST_END(rev_ghost);
}
