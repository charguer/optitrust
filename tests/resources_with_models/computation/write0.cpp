#include <optitrust_models.h>

void write0(int* p) {
  __consumes("p ~> UninitCell");
  __produces("p ~~> 0");
  *p = 0;
}

int ret0() {
  __ensures("_Res = 0");
  return 0;
}
/*
int ret0_bis() {
  __ensures("out: _Res = 0");
  return 0;
}

void write0_bis(int* p) {
  __consumes("p ~> UninitCell");
  __produces("p ~~> 0");
  *p = ret0_bis(); __bind("rw");
  __ghost(rewrite_linear, "inside := fun v -> p ~~> v, by := rw");
}
*/
void write0_ter(int* p) {
__consumes("p ~> UninitCell");
__produces("p ~~> 0");
  const int x = ret0();
 *p = x;
  __ghost(rewrite_linear, "inside := fun v -> p ~~> v, from := x, to := 0");
}
