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

void test_vect(){
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

  int* f = 0;
  int fv = *f;
  int* g = 0;
  for (int i = 0; i < 2; i++) {
    g[1] = 0;
    int gv = *(&g[1]);
  }
}

/* check failures LATER

int test_write(int* p) {
  *p = 4;
}

int test_write(int* const p) {
}

int test_fail() {
  int bad = 5; // cannot to_const this variable
  test_write(&bad);
  // bad = 6;
}
*/
