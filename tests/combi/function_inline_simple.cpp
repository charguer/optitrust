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

int f1(int x) {
  int a = x + x;
  return a;
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
  // int y = f1(x);
  int z = g(x);
  int u = h(x);
  int *q = new int(3);
  m(q);
  z = f(f1(x));

  vect a = {0,1};
  vect b = {3,4,5};
  vect c;
  c = vect_add (b, vect_mul(x, a));
  return 0;
}
