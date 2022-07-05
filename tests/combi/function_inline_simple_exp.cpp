typedef struct {
  int x;
  int y;
  int z;
} vect;

vect vect_add(vect v1, vect v2) {
  return (vect){v1.x + v2.x, v1.y + v2.y, v1.z + v2.z};
}

vect vect_mul(int d, vect v) { return (vect){d * v.x, d * v.y, d * v.z}; }

int f(int x) {
  int a = x + x;
  return a + a;
}

int g(int x) {
  if (x > 0)
    return 1;
  else
    return 2;
}

int h(int x) {
  if (x > 0) return 1;
  return 2;
}

void m(int* p) { (*p)++; }

int main() {
  int x = 3;
  int a0 = x + x;

  int y = a0 + a0;
  int z;
  if (x > 0)
    z = 1;
  else
    z = 2;

  int s = y + z;
  if (x > 0) *__TEMP_Optitrust = 1;
  goto exit_body;
exit_body:;
  int u = 2;
  int* q;
  (*q)++;

  vect a = {0, 1, 2};
  vect b = {3, 4, 5};

  vect c = {b.x + x * a.x, b.y + x * a.y, b.z + x * a.z};
  return 0;
}

vect vect_op(vect v) {
  if (true) {
    return v;
  }
  return v;
}

vect vect_op2(vect v) {
  vect res = {0, 0};
  res.x = 1;
  return res;
}

void test_const_ret() {
  int x = 3;
  int a1 = x + x;

  const int y = a1 + a1;
  int z;
  if (x > 0)
    z = 1;
  else
    z = 2;

  int s = y + z;
  const vect t = {0, 1};

  const vect w = {t.x + x * t.x, t.y + x * t.y, t.z + x * t.z};
  if (true) {
    *(a1 + a1) = w;
    goto exit_body;
  }
exit_body:;
  vect res = {0, 0};
  res.x = 1;

  vect w2 = res;
}

void test_const_arg() {
  const int x = 3;
  int a2 = x + x;

  int y = a2 + a2;
  int z = g(x);
  int s = y + z;
}
