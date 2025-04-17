#include <optitrust_models.h>

void incr(int* a) {
  __requires("v: int");
  __consumes("a ~~> v");
  __produces("a ~~> v + 1");
  *a = *a + 1;
}

void incr_twice(int* k) {
  __requires("v: int");
  __consumes("k ~~> v");
  // __produces("k ~~> v + 2"); // Cannot unify
  __produces("k ~~> v + 1 + 1");
  incr(k);
  incr(k);
  // Manage rewrite
}

int main() {}
