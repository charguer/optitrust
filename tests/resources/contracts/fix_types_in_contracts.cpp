#include <optitrust.h>

typedef struct {
  int x;
  int y;
} point;

int x(point* p) {
  __reads("&p->x ~> Cell");
  return p->x;
}

int x2(point* p) {
  __reads("&p[MINDEX1(5, 2)].x ~> Cell");
  return p[MINDEX1(5, 2)].x;
}
