#include <optitrust_models.h>

void write0(int* p) {
  __writes("p ~~> 0");
  *p = 0;
}

int ret0() {
  __ensures("__is_true(_Res == 0)");
  return 0;
}

void write0_ter(int* p) {
  __writes("p ~~> 0");
  const int x = ret0();
  *p = x;
  __ghost(rewrite_linear, "inside := fun v -> p ~~> v, from := x, to := 0");
}
