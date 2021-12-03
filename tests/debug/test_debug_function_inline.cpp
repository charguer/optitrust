typedef struct {
  int x;
  int y;
} vect;

int foo(vect v) { return v.x; }

int demo() {
  const vect a = { 0, 1 };
  int ax = foo(a);
  vect c = a;
  int cx = foo(c);
}


typedef struct {
  int x;
  int y;
  int z;
} vect3;

vect3 vect3_add(vect3 v1, vect3 v2) {
  vect3 res = { v1.x + v2.x, v1.y + v2.y, v1.z + v2.z };
  return res;
}

int vect3_mul(int d, vect3 v) {
  return d * v.x;
}



int main () {
  int x = 3;

  vect3 a = {0,1};
  int y = vect3_mul (x, a);
  return 0;
}