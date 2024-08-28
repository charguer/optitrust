#include "optitrust.h"

typedef struct {
  int x;
  int y;
} vect;

int main() {
  __pure();
  vect a;
  vect b;
  __ghost(
      [&]() {
        __consumes("_Uninit(&a ~> Cell)");
        __produces("_Uninit(&a.x ~> Cell)");
        __produces("_Uninit(&a.y ~> Cell)");
        __admitted();
      },
      "");
  __ghost(
      [&]() {
        __requires("#_1: _Fraction");
        __consumes("_RO(#_1, &b ~> Cell)");
        __produces("Wand(_RO(#_1, &b.x ~> Cell), _RO(#_1, &b ~> Cell))");
        __produces("_RO(#_1, &b.x ~> Cell)");
        __admitted();
      },
      "");
  __ghost(
      [&]() {
        __requires("#_1: _Fraction");
        __consumes("_RO(#_1, &b ~> Cell)");
        __produces("Wand(_RO(#_1, &b.y ~> Cell), _RO(#_1, &b ~> Cell))");
        __produces("_RO(#_1, &b.y ~> Cell)");
        __admitted();
      },
      "");
  a.x = b.x;
  a.y = b.y;
  __ghost(
      [&]() {
        __consumes("&a.x ~> Cell");
        __consumes("&a.y ~> Cell");
        __produces("&a ~> Cell");
        __admitted();
      },
      "");
  __ghost(
      [&]() {
        __requires("#_1: _Fraction");
        __consumes("Wand(_RO(#_1, &b.x ~> Cell), _RO(#_1, &b ~> Cell))");
        __consumes("_RO(#_1, &b.x ~> Cell)");
        __produces("_RO(#_1, &b ~> Cell)");
        __admitted();
      },
      "");
  __ghost(
      [&]() {
        __requires("#_1: _Fraction");
        __consumes("Wand(_RO(#_1, &b.y ~> Cell), _RO(#_1, &b ~> Cell))");
        __consumes("_RO(#_1, &b.y ~> Cell)");
        __produces("_RO(#_1, &b ~> Cell)");
        __admitted();
      },
      "");
}
