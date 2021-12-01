typedef struct {
  int x;
  int y;
  int z;
} vect;

vect vect_add(vect v1, vect v2) {
  return { v1.x + v2.x, v1.y + v2.y, v1.z + v2.z };
}

int vect_mul(int d, vect v) {
  return d * v.x;
}



int main () {
  int x = 3;
 
  vect a = {0,1};
  int y = vect_mul (x, a);
  return 0;
}