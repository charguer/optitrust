#include "cpp_ast.h"

vect + (const vect& a, const vect& b) {
  vect r = {a.x + b.x, a.y + b.y};
  return r;
}

int h(int x) { return x; }

int main() {
  int c = 0;
  vect a = {0, 0};
  vect b = {0, 0};
  vect c = a + b;
}

template <typename A>
class queue {
  A item;
};

class test {
  static void Partition(int* outPivot, queue<int> data) { return; }
};

template <typename T>
void foo(T) {}

void a() { foo(45); }
