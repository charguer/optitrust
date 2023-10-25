typedef struct {
  int x;
  int y;
} vect;

typedef struct {
  int weight;
  vect rel_pos;
  vect rel_speed;
} obj;

vect f() { return (vect){1, 1}; }

int main() {
  vect p = {0, 0};
  vect b;
  b.x = p.x;
  b.y = p.y;
  vect u;
  obj a = {0, {0, 0}, {0, 0}};
  u.x = a.rel_pos.x;
  u.y = a.rel_pos.y;
  vect t[2];
  vect p2 = p;
  t[0].x = p2.x;
  t[0].y = p2.y;
  obj c;
  c.weight = a.weight;
  c.rel_pos.x = a.rel_pos.x;
  c.rel_pos.y = a.rel_pos.y;
  c.rel_speed.x = a.rel_speed.x;
  c.rel_speed.y = a.rel_speed.y;
  return 0;
}
