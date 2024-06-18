#include <optitrust.h>

int add(int a, int b) { return a + b; }

int main() {
  int k = 1;
  int a = add(k, k);
  float x = cast<int, float>(a);
  x + cast<int, float>(1);
  int b = cast<float, int>(x);
  int c = cast<float, int>(x);
  int y = cast<float, int>(cast<int, float>(c));
  if (cast<int, bool>(1)) {
    int a = cast<bool, int>(true);
  }
}

void bar(size_t s) {}

void foo() {
  int a = 0;
  bar(cast<int, long unsigned int>(a));
  size_t b = cast<int, long unsigned int>(a);
  int c = cast<long unsigned int, int>((cast<int, long unsigned int>(a) + b));
}

void ghost(float* A, int n) {
  __modifies("A ~> Matrix1(n)");
  const __ghost_fn rev_ghost = __ghost_begin(matrix1_ro_focus, "");
  __ghost_end(rev_ghost);
}
