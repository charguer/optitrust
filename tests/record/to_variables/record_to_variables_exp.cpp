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

vect f() { return (vect){1, 1}; }

void g() {
  __pure();
  vect p = {0, 0};
  int sX;
  int sY;
  __ghost([&]() {
    __consumes("&p ~> Cell");
    __produces("&p.x ~> Cell");
    __produces("&p.y ~> Cell");
    __admitted();
  });
  __ghost([&]() {
    __modifies("_Uninit(&sX ~> Cell)");
    __modifies("_Uninit(&sY ~> Cell)");
    __admitted();
  });
  sX = p.x;
  sY = p.y;
  const vect s2 = {sX + 2, sY + 2};
  __ghost([&]() {
    __consumes("&p.x ~> Cell");
    __consumes("&p.y ~> Cell");
    __produces("&p ~> Cell");
    __admitted();
  });
  __ghost([&]() {
    __modifies("&sX ~> Cell");
    __modifies("&sY ~> Cell");
    __admitted();
  });
  int aWeight;
  vect aPos;
  vect aSpeed;
  __ghost([&]() {
    __modifies("_Uninit(&aWeight ~> Cell)");
    __modifies("_Uninit(&aPos ~> Cell)");
    __modifies("_Uninit(&aSpeed ~> Cell)");
    __admitted();
  });
  aWeight = 0;
  aPos = p;
  aSpeed = (vect){sX, sY};
  __ghost([&]() {
    __modifies("&aWeight ~> Cell");
    __modifies("&aPos ~> Cell");
    __modifies("&aSpeed ~> Cell");
    __admitted();
  });
  const int bWeight = 0;
  const vect bPos = p;
  const vect bSpeed = {sX, sY};
  const obj b2 = {bWeight, bPos, bSpeed};
  const obj b3 = {bWeight, bPos, bSpeed};
}
