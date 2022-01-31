/*
void addr_array_cell() {
  int p[2];
  int* n = &p[0];
}*/
/*
int main() {

  int x = 4;
  x = 6;
  int y = 10;
  x = 7;
  int z = x;

  int t[2];
  t[0] = 3;
  t[1] = 4;
  int a;
  a = t[0];
  return 0;
}

*/

typedef struct {int x; int y; } vect;

int f(int* t, int** u, vect w) {
  vect v = { 0, 0 };
  v.x = w.x;
  t[0] = 0;
  u[0][0] = 0;
  int** r;
  r[0][0] = 0;


}