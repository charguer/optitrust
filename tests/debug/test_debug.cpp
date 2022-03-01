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

int main() {
  int x = 3;
  vect a = {0,1,2};
  vect b = {3,4,5};
  vect c = vect_add (b, vect_mul(x, a));
  return 0;
}