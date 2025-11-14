#include <optitrust_models.h>

void test_model(int a) {
  __pure();
  int b = ({
    int arith_res = a;
    __ghost([&]() {
      __requires("res: int");
      __consumes("&arith_res ~~> res");
      __produces("&arith_res ~~> a + 1 - 1");
      __admitted();
    });
    const int arith_res1 = arith_res;
    arith_res1;
  });
  __ghost(assert_hprop, "H := &b ~~> a + 1 - 1");
}
