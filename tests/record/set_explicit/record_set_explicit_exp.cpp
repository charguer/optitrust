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

int main() {
  vect p;
  p.x = 0;
  p.y = 0;
  vect b;
  b.x = p.x;
  b.y = p.y;
  obj a;
  a.weight = 0;
  a.pos = (vect){0, 0};
  a.speed.x = 0;
  a.speed.y = 0;
  vect u;
  u.x = a.pos.x;
  u.y = a.pos.y;
  const obj c = {0, {0, 0}, {0, 0}};
  obj* const v = &a;
  v->weight = c.weight;
  v->pos = c.pos;
  v->speed = c.speed;
}
