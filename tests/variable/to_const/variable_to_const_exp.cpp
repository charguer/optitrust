int main() {
  const int x = 3;
  const int y = x + x;
  const int z = x + y + 4;
  return z;
}

typedef struct {
  int x;
  int y;
} vect;

typedef struct {
  vect pos;
  vect speed;
} part;

void test_vect() {
  const vect v = {0, 1};
  const vect w = v;
  int a = v.x;
  int b = v.y;
  const part p = {v, w};
  vect ppos = p.pos;
  int c = p.pos.x;
  part q[2] = {{v, w}, {v, w}};
  int d = q[0].pos.x;
  part* const r = &q[0];
  int e = r[0].pos.x;
  int* const f = nullptr;
  int fv = *f;
  int* const g = nullptr;
  for (int i = 0; i < 2; i++) {
    g[1] = 0;
    int gv = g[1];
  }
}
