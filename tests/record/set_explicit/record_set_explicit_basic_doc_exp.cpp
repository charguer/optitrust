#include "optitrust.h"

typedef struct {
  int x;
  int y;
} vect;

void f() {
  __pure();
  vect a;
  vect b = {0, 0};
  __ghost([&]() {
    __consumes("&a ~> UninitCell");
    __produces("&a.x ~> UninitCell");
    __produces("&a.y ~> UninitCell");
    __admitted();
  });
  __ghost([&]() {
    __requires("#_1: _Fraction");
    __consumes("_RO(#_1, &b ~> UninitCell)");
    __produces(
        "Wand(_RO(#_1, &b.x ~> UninitCell), _RO(#_1, &b ~> UninitCell))");
    __produces("_RO(#_1, &b.x ~> UninitCell)");
    __admitted();
  });
  __ghost([&]() {
    __requires("#_1: _Fraction");
    __consumes("_RO(#_1, &b ~> UninitCell)");
    __produces(
        "Wand(_RO(#_1, &b.y ~> UninitCell), _RO(#_1, &b ~> UninitCell))");
    __produces("_RO(#_1, &b.y ~> UninitCell)");
    __admitted();
  });
  a.x = b.x;
  a.y = b.y;
  __ghost([&]() {
    __consumes("&a.x ~> UninitCell");
    __consumes("&a.y ~> UninitCell");
    __produces("&a ~> UninitCell");
    __admitted();
  });
  __ghost([&]() {
    __requires("#_1: _Fraction");
    __consumes(
        "Wand(_RO(#_1, &b.x ~> UninitCell), _RO(#_1, &b ~> UninitCell))");
    __consumes("_RO(#_1, &b.x ~> UninitCell)");
    __produces("_RO(#_1, &b ~> UninitCell)");
    __admitted();
  });
  __ghost([&]() {
    __requires("#_1: _Fraction");
    __consumes(
        "Wand(_RO(#_1, &b.y ~> UninitCell), _RO(#_1, &b ~> UninitCell))");
    __consumes("_RO(#_1, &b.y ~> UninitCell)");
    __produces("_RO(#_1, &b ~> UninitCell)");
    __admitted();
  });
}
