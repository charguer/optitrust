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
/*

typedef struct {int x; int y; } vect;

int f(int* t, int** u, vect w) {
  vect v = { 0, 0 };
  v.x = w.x;
  t[0] = 0;
  u[0][0] = 0;
  int** r;
  r[0][0] = 0;


}

*/
typedef struct { int x; int y; } vect;

typedef struct { vect pos; vect speed; } particle;

int main() {
 vect v = {0,1};
 vect v1 = {0,1};
 vect* p = &v;
 particle p1 = {{0,1}, {1,2}};
 particle p2;
 
 p1.pos = p2.pos;
 p1.pos.x = p2.pos.x;

 v = v1;
 v.x = v.y;
 (*p).x = (*p).y;
 p->x = p->y;
}
  
