typedef struct {
  int x;
  int y;
  int z;
} vect;

vect vect_add(vect v1, vect v2) {
  return {(v1.x + v2.x), (v1.y + v2.y), (v1.z + v2.z)};
}

vect vect_mul(int i, vect v) { return {(i * v.x), (i * v.y), (i * v.z)}; }

int main() {
  vect a = {0, 1};
  int x = 1;
  vect b = {3, 4, 5};
  vect c = {(b.x + (x * a.x)), (b.y + (x * a.y)), (b.z + (x * a.z))};
  return 0;
}
