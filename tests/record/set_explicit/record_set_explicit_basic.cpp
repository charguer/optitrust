#include "optitrust.h"

typedef struct {
  int x;
  int y;
} vect;

typedef struct {
  int weight;
  vect pos;
  vect speed;
} obj;

REGISTER_STRUCT_ACCESS(x)
REGISTER_STRUCT_ACCESS(y)
REGISTER_STRUCT_ACCESS(weight)
REGISTER_STRUCT_ACCESS(pos)
REGISTER_STRUCT_ACCESS(speed)

vect f() {
  __pure();
  __admitted();
  return {1,1};
}

void g() {
  __pure();

  vect p = {0,0};
  vect b;
  b = p;
  vect u;
  obj a = {0,{0,0},{0,0}};
  __ghost([&] {
    __consumes("&a ~> Cell");
    __produces("&a.weight ~> Cell");
    __produces("&a.pos ~> Cell");
    __produces("&a.speed ~> Cell");
    __admitted();
  }, "");
  u = a.pos;
  __ghost([&] {
    __produces("&a ~> Cell");
    __consumes("&a.weight ~> Cell");
    __consumes("&a.pos ~> Cell");
    __consumes("&a.speed ~> Cell");
    __admitted();
  }, "");
  // FIXME: vect t[2];
  vect* const t = (vect* const) MALLOC1(2, sizeof(vect));
  vect p2 = p;
  __GHOST_BEGIN(f, group_uninit_focus, "i := 0");
  t[MINDEX1(2, 0)] = p2;
  __GHOST_END(f);
  obj c;
  c = a;
  MFREE1(2, t);
}
