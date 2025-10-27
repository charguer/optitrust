#include <optitrust_models.h>

void test_model(int a) {
  __pure();

  int b = a + 1 - 1;
  // __call_with(bla(a), "a + 1 - 1");
  // b ~~> a
  __ghost(assert_hprop, "&b ~~> a + 1 - 1");
}
