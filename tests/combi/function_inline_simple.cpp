typedef struct {
  int x;
  int y;
  int z;
} vect;

vect vect_add(vect v1, vect v2) {
  return { v1.x + v2.x, v1.y + v2.y, v1.z + v2.z };
}

vect vect_mul(int d, vect v) {
  return { d * v.x, d * v.y, d * v.z };
}


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
  if (x > 0)
    return 1;
  return 2;
}

void m(int* p) {
  (*p)++;
}

int main() {
  int x = 3;
  int y = f(x);
  int z = g(x);
  int s = y + z;

  int u = h(x);
  int *q;
  m(q);

  vect a = {0,1,2};
  vect b = {3,4,5};
  vect c = vect_add (b, vect_mul(x, a));
  return 0;
}

vect vect_op(vect v) {
  if (true) {
    return v;
  }
  return v;
}

vect vect_op2(vect v) {
  vect res = {0,0};
  res.x = 1;
  return res;
}

void test_const_ret() {
  int x = 3;
  const int y = f(x);
  const int z = g(x);
  int s = y + z;
  const vect t = { 0, 1 };
  // const vect u = vect_mul(x, t);
  const vect v = vect_add(t, vect_mul(x, t));
  const vect w = vect_op(v);
  vect w2 = vect_op2(v);
  /*
    vect res = {0,0};
    res.x = 1;
    vect optitrust_r = res;
    vect w2 = optitrust_r;

    // do this step (possibly on the fly), unless const vect w2 and there are "goto exit"

    vect res = {0,0};
    res.x = 1;
    vect w2 = res;

    // do this step if previous step succeeded AND the "return" expr is a variable
    // AND (both w2 and res are const OR both are not const)

    vect w2 = {0,0};
    w2.x = 1;
  */
}

void test_const_arg() {
  const int x = 3;
  int y = f(x);
  int z = g(x);
  int s = y + z;
}

