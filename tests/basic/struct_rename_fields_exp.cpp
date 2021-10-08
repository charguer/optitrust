typedef struct {
  int x_rel;
  int y_rel;
} vect;

typedef struct {
  int weight_rel;
  vect pos_rel;
  vect speed_rel;
} obj;

vect f() { return {1, 1}; }

int main() {
  vect p = {0, 0};
  vect b;
  b.x_rel = p.x_rel;
  b.y_rel = p.y_rel;
  vect u;
  obj a = {0, {0, 0}, {0, 0}};
  u.x_rel = a.pos_rel.x_rel;
  u.y_rel = a.pos_rel.y_rel;
  vect t[2];
  vect p2 = p;
  t[0].x_rel = p2.x_rel;
  t[0].y_rel = p2.y_rel;
  obj c;
  c.weight_rel = a.weight_rel;
  c.pos_rel.x_rel = a.pos_rel.x_rel;
  c.pos_rel.y_rel = a.pos_rel.y_rel;
  c.speed_rel.x_rel = a.speed_rel.x_rel;
  c.speed_rel.y_rel = a.speed_rel.y_rel;
  return 0;
}