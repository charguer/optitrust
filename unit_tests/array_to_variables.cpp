
typedef int T[2];
typedef struct {
  int x;
  int y;
} vect;

typedef vect U[2];

// Not supported yet
typedef struct {
  T t[2];
  int g;
} particle;

int main(){
  T t[2];
  t[0][1] = 4;
  t[1][0] = 5;
  t[0][0] = 1;
  t[1][1] = 2;

  U u[2];
  u[0][1].x = 5;
  u[0][1].y = u[0][1].x + 6;
  

  particle p;
  p.t[0][1] = 9;
  p.t[1][1] = 2;

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
  v[0] = {1,2};
  v[1] = {3,4};

  return 0;
}