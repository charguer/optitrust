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

template <typename T, typename U>
U* __struct_access_x(T* v) {
  __pure();
  __admitted();
  return &v->x;
}

template <typename T, typename U>
U __struct_get_x(T v) {
  __pure();
  __admitted();
  return v.x;
}

template <typename T, typename U>
U* __struct_access_y(T* v) {
  __pure();
  __admitted();
  return &v->y;
}

template <typename T, typename U>
U __struct_get_y(T v) {
  __pure();
  __admitted();
  return v.y;
}

template <typename T, typename U>
U* __struct_access_weight(T* v) {
  __pure();
  __admitted();
  return &v->weight;
}

template <typename T, typename U>
U __struct_get_weight(T v) {
  __pure();
  __admitted();
  return v.weight;
}

template <typename T, typename U>
U* __struct_access_pos(T* v) {
  __pure();
  __admitted();
  return &v->pos;
}

template <typename T, typename U>
U __struct_get_pos(T v) {
  __pure();
  __admitted();
  return v.pos;
}

template <typename T, typename U>
U* __struct_access_speed(T* v) {
  __pure();
  __admitted();
  return &v->speed;
}

template <typename T, typename U>
U __struct_get_speed(T v) {
  __pure();
  __admitted();
  return v.speed;
}

vect f() {
  __pure();
  __admitted();
  return (vect){1, 1};
}

void g() {
  __pure();
  vect p = {0, 0};
  vect b;
  __ghost(
      [&]() {
        __consumes("&b ~> Cell");
        __produces("&b.x ~> Cell");
        __produces("&b.y ~> Cell");
        __admitted();
      },
      "");
  __ghost(
      [&]() {
        __consumes("&p ~> Cell");
        __produces("&p.x ~> Cell");
        __produces("&p.y ~> Cell");
        __admitted();
      },
      "");
  b.x = p.x;
  b.y = p.y;
  __ghost(
      [&]() {
        __consumes("&p.x ~> Cell");
        __consumes("&p.y ~> Cell");
        __produces("&p ~> Cell");
        __admitted();
      },
      "");
  __ghost(
      [&]() {
        __consumes("&b.x ~> Cell");
        __consumes("&b.y ~> Cell");
        __produces("&b ~> Cell");
        __admitted();
      },
      "");
  vect u;
  obj a = {0, {0, 0}, {0, 0}};
  __ghost(
      [&]() {
        __consumes("&a ~> Cell");
        __produces("&a.weight ~> Cell");
        __produces("&a.pos ~> Cell");
        __produces("&a.speed ~> Cell");
        __admitted();
      },
      "");
  u = a.pos;
  __ghost(
      [&]() {
        __consumes("&a.weight ~> Cell");
        __consumes("&a.pos ~> Cell");
        __consumes("&a.speed ~> Cell");
        __produces("&a ~> Cell");
        __admitted();
      },
      "");
  vect* const t = (vect* const)MALLOC1(2, sizeof(vect));
  vect p2 = p;
  const __ghost_fn f = __ghost_begin(group_uninit_focus, "i := 0");
  __ghost(
      [&]() {
        __consumes("_Uninit(&t[MINDEX1(2, 0)] ~> Cell)");
        __produces("_Uninit(&t[MINDEX1(2, 0)].x ~> Cell)");
        __produces("_Uninit(&t[MINDEX1(2, 0)].y ~> Cell)");
        __admitted();
      },
      "");
  __ghost(
      [&]() {
        __consumes("&p2 ~> Cell");
        __produces("&p2.x ~> Cell");
        __produces("&p2.y ~> Cell");
        __admitted();
      },
      "");
  t[MINDEX1(2, 0)].x = p2.x;
  t[MINDEX1(2, 0)].y = p2.y;
  __ghost(
      [&]() {
        __consumes("&p2.x ~> Cell");
        __consumes("&p2.y ~> Cell");
        __produces("&p2 ~> Cell");
        __admitted();
      },
      "");
  __ghost(
      [&]() {
        __consumes("&t[MINDEX1(2, 0)].x ~> Cell");
        __consumes("&t[MINDEX1(2, 0)].y ~> Cell");
        __produces("&t[MINDEX1(2, 0)] ~> Cell");
        __admitted();
      },
      "");
  __ghost_end(f);
  obj c;
  __ghost(
      [&]() {
        __consumes("&c ~> Cell");
        __produces("&c.weight ~> Cell");
        __produces("&c.pos ~> Cell");
        __produces("&c.speed ~> Cell");
        __admitted();
      },
      "");
  __ghost(
      [&]() {
        __consumes("&a ~> Cell");
        __produces("&a.weight ~> Cell");
        __produces("&a.pos ~> Cell");
        __produces("&a.speed ~> Cell");
        __admitted();
      },
      "");
  c.weight = a.weight;
  __ghost(
      [&]() {
        __consumes("&c.pos ~> Cell");
        __produces("&c.pos.x ~> Cell");
        __produces("&c.pos.y ~> Cell");
        __admitted();
      },
      "");
  __ghost(
      [&]() {
        __consumes("&a.pos ~> Cell");
        __produces("&a.pos.x ~> Cell");
        __produces("&a.pos.y ~> Cell");
        __admitted();
      },
      "");
  c.pos.x = a.pos.x;
  c.pos.y = a.pos.y;
  __ghost(
      [&]() {
        __consumes("&a.pos.x ~> Cell");
        __consumes("&a.pos.y ~> Cell");
        __produces("&a.pos ~> Cell");
        __admitted();
      },
      "");
  __ghost(
      [&]() {
        __consumes("&c.pos.x ~> Cell");
        __consumes("&c.pos.y ~> Cell");
        __produces("&c.pos ~> Cell");
        __admitted();
      },
      "");
  __ghost(
      [&]() {
        __consumes("&c.speed ~> Cell");
        __produces("&c.speed.x ~> Cell");
        __produces("&c.speed.y ~> Cell");
        __admitted();
      },
      "");
  __ghost(
      [&]() {
        __consumes("&a.speed ~> Cell");
        __produces("&a.speed.x ~> Cell");
        __produces("&a.speed.y ~> Cell");
        __admitted();
      },
      "");
  c.speed.x = a.speed.x;
  c.speed.y = a.speed.y;
  __ghost(
      [&]() {
        __consumes("&a.speed.x ~> Cell");
        __consumes("&a.speed.y ~> Cell");
        __produces("&a.speed ~> Cell");
        __admitted();
      },
      "");
  __ghost(
      [&]() {
        __consumes("&c.speed.x ~> Cell");
        __consumes("&c.speed.y ~> Cell");
        __produces("&c.speed ~> Cell");
        __admitted();
      },
      "");
  __ghost(
      [&]() {
        __consumes("&a.weight ~> Cell");
        __consumes("&a.pos ~> Cell");
        __consumes("&a.speed ~> Cell");
        __produces("&a ~> Cell");
        __admitted();
      },
      "");
  __ghost(
      [&]() {
        __consumes("&c.weight ~> Cell");
        __consumes("&c.pos ~> Cell");
        __consumes("&c.speed ~> Cell");
        __produces("&c ~> Cell");
        __admitted();
      },
      "");
  MFREE1(2, t);
}
