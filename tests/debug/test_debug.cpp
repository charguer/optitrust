int main() {
  int x = 3;
  int y = x + x;
  int z = x + y + 4;
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

int test_vect(){
  vect v = { 0, 1 };
  vect w = v;
  int a = v.x;
  int b = v.y;
  part p = { v, w };
  vect ppos = p.pos;
  int c = p.pos.x;
  part q[2] = { { v, w }, { v, w } };
  int d = q[0].pos.x;
  part* r = &q[0];
  int e = r[0].pos.x;
}
