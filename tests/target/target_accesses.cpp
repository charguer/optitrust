#include <stdio.h>

typedef struct {
  int x;
  int y; }
vect;

typedef struct {
  int weight;
  vect pos;
  vect speed;
} obj;

const int N = 5;
double t[N];

void arrays() {
   for (int i = 0; i < N; i++) {
      t[i] = i + 3.14; // internally: set(access(t,i), i + 3.14)
   }
   double s = 0;
   for (int i = 0; i < N; i++) {
      s += t[i];  // internally: set(s, app(+, [get(s); get(access(t,i))]))
   }
   printf("%d\n", s);
}

float* u;


void structs() {
  vect p;
  p.x = 0;
  p.y = 0;
  vect q;
  q.x = p.x;
  q.y = p.y;
  obj a;
  a.weight = 0;
  a.pos = {0, 0};
  a.speed.x = 0;
  a.speed.y = 0;
  vect u;
  u.x = a.pos.x;
  u.y = a.pos.y;
  obj* b = &a;
  b->pos.x = q.x;
  b->pos.y = q.y;

}
