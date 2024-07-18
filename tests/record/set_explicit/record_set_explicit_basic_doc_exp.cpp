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
        __consumes("&a ~> Cell");
        __produces("&a.x ~> Cell");
        __produces("&a.y ~> Cell");
        __admitted();
      },
      "");
  __ghost(
      [&]() {
        __consumes("&b ~> Cell");
        __produces("&b.x ~> Cell");
        __produces("&b.y ~> Cell");
        __admitted();
      },
      "");
  a.x = b.x;
  a.y = b.y;
  __ghost(
      [&]() {
        __consumes("&b.x ~> Cell");
        __consumes("&b.y ~> Cell");
        __produces("&b ~> Cell");
        __admitted();
      },
      "");
  __ghost(
      [&]() {
        __consumes("&a.x ~> Cell");
        __consumes("&a.y ~> Cell");
        __produces("&a ~> Cell");
        __admitted();
      },
      "");
}
