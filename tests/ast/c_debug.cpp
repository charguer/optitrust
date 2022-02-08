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
// typedef struct { int x; int y; } vect;

// typedef struct { vect pos; vect speed; } particle;

// typedef int T[2];

// typedef vect U[2];

// int main() {
//  vect v = {0,1};
//  vect v1 = {0,1};
//  vect* p = &v;
//  particle p1 = {{0,1}, {1,2}};
//  particle p2;
 
//  p1.pos = p2.pos;
//  p1.pos.x = p2.pos.x;

//  v = v1;
//  v.x = v.y;
//  (*p).x = (*p).y;
//  p->x = p->y;
// }
  


typedef struct {
  int x;
  int y;
} vect;

typedef int T[2];


// Not supported yet

typedef vect U[2];

typedef struct {
  T t[2];
  int g;
} particle;

void f(int x, int y, T t[2]){
  t[0][0] = 0;
  t[0][1] = 0;
  t[1][0] = 0;
  t[1][1] = 0;
}

void g(int x, int y, U t[2]){
  t[0][0].x = 0;
}

 int main(){
  // Simple array access
  T t[2];
  t[0][1] = 4;
  t[1][0] = 5;
  t[0][0] = 1;
  t[1][1] = 2;

  // Array access with struct access
  U u[2];
  u[0][1].x = 5;
  u[0][1].y = u[0][1].x + 6;
  u[1][1].x = 5;
  u[1][1].y = u[1][1].x + 6;

  // NOTE: uncomment the line below triggers a legitimate error,
  //       because "u" is used without an access to it.
  // g(0,0,u);

  // Struct access with array access
  particle p;
  p.t[0][1] = 9;
  p.t[1][1] = 2;

  // Struct of array access with array access
  particle ps[5];
  ps[3].t[0][1] = 8;
  ps[3].t[1][0] = 10;

/*
 array_access(t, i::rest)
 array_acces(ta, rest)=> if rest is empty, then it's only ta.

  smart constructor (base, accesses)  for building an array_access :
     if accessses = [] then base else array_access(base,accesses)
*/

  vect v[2];
  vect a = {1,2};
  v[0] = a;
  v[1] = a;

  return 0;
}