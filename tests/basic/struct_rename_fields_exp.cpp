typedef struct {
  int rel_x;
  int rel_y;
} vect;

typedef struct {
  int weight;
  vect rel_pos;
  vect rel_speed;
} obj;

vect f() { return {1, 1}; }

int main() {
  vect p = {0, 0};
  vect b;
  b.rel_x = p.rel_x;
  b.rel_y = p.rel_y;
  vect u;
  obj a = {0, {0, 0}, {0, 0}};
  u.rel_x = a.rel_pos.rel_x;
  u.rel_y = a.rel_pos.rel_y;
  vect t[2];
  vect p2 = p;
  t[0].rel_x = p2.rel_x;
  t[0].rel_y = p2.rel_y;
  obj c;
  c.weight = a.weight;
  c.rel_pos.rel_x = a.rel_pos.rel_x;
  c.rel_pos.rel_y = a.rel_pos.rel_y;
  c.rel_speed.rel_x = a.rel_speed.rel_x;
  c.rel_speed.rel_y = a.rel_speed.rel_y;
  return 0;
}