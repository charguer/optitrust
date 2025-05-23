#include "optitrust.h"

typedef struct {
  int x;
  int y; }
vect;

typedef struct {
  int weight;
  vect pos;
  vect speed;
} obj;

vect f() {
  return {1,1};
}

void g() {
  __pure();

  vect p = {0,0};
  vect s;
  __ghost([&] {
    __consumes("&p ~> Cell");
    __produces("&p.x ~> Cell");
    __produces("&p.y ~> Cell");
    __admitted();
  }, "");
  __ghost([&] {
    __consumes("&s ~> UninitCell");
    __produces("&s.x ~> UninitCell");
    __produces("&s.y ~> UninitCell");
    __admitted();
  }, "");
  s.x = p.x;
  s.y = p.y;
  const vect s2 = { s.x + 2, s.y + 2 };
  __ghost([&] {
    __produces("&p ~> Cell");
    __consumes("&p.x ~> Cell");
    __consumes("&p.y ~> Cell");
    __admitted();
  }, "");
  __ghost([&] {
    __produces("&s ~> Cell");
    __consumes("&s.x ~> Cell");
    __consumes("&s.y ~> Cell");
    __admitted();
  }, "");

  obj a;
  __ghost([&] {
    __consumes("&a ~> UninitCell");
    __produces("&a.weight ~> UninitCell");
    __produces("&a.pos ~> UninitCell");
    __produces("&a.speed ~> UninitCell");
    __admitted();
  }, "");
  a.weight = 0;
  a.pos = p;
  a.speed = s;
  __ghost([&] {
    __produces("&a ~> Cell");
    __consumes("&a.weight ~> Cell");
    __consumes("&a.pos ~> Cell");
    __consumes("&a.speed ~> Cell");
    __admitted();
  }, "");

  const obj b = { .weight = 0, .pos = p, .speed = s};
  const obj b2 = { .pos = b.pos, .speed = b.speed, .weight = b.weight };
  const obj b3 = { b.weight, b.pos, b.speed };
}
