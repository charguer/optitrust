typedef struct {
  int x;
  int y;
  int z;
} vect;

vect vect_add(vect v1, vect v2) {
  return {(v1.x + v2.x), (v1.y + v2.y), (v1.z + v2.z)};
}

vect vect_mul(int d, vect v) { return {(d * v.x), (d * v.y), (d * v.z)}; }

int f(int x) {
  int a = (x + x);
  return (a + a);
}

int f1(int x) {
  int a = (x + x);
  return a;
}

int g(int x) {
  if ((x > 0))
    return 1;
  else
    return 2;
}

int h(int x) {
  if ((x > 0))
    return 1;
  return 2;
}

void m(int *p) { (*p)++; }

int main() {
  int x = 3;
  int y = f1(x);
  int z = g(x);
  if ((x > 0)) {
    int u = 1;
    goto exit_body;
  }
  u = 2;
exit_body:;
  int *q = new int;
  (*q)++;
  int a0 = (f1(x) + f1(x));
  z = (a0 + a0);
  vect a = {0, 1};
  vect b = {3, 4, 5};
  vect c = {((b.x) + (x * (a.x))), ((b.y) + (x * (a.y))),
            ((b.z) + (x * (a.z)))};
  return 0;
}
