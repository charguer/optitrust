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
  int z = x > 0 ? 1 : 2;
  int s = y + z;
  int res;
  if (x > 0) /*no-brace*/ {
    res = 1;
    goto exit;
  }
  res = 2;
exit:;
  int u = res;
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
  const int z = x > 0 ? 1 : 2;
  int s = y + z;
  const vect t = {0, 1};
  const vect v = {t.x + x * t.x, t.y + x * t.y, t.z + x * t.z};
  vect res;
  if (true) {
    res = v;
    goto exit;
  }
  res = v;
exit:;
  const vect w = res;
  vect res1 = {0, 0};
  res1.x = 1;
  vect w2 = res1;
}

void test_const_arg() {
  const int x = 3;
  int a2 = x + x;
  int y = a2 + a2;
  int z = g(x);
  int s = y + z;
}
