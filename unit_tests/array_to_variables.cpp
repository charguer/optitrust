v
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
  //t[0] = {1,2};
  t[0][1]  = 4;
  // t[0][0] = 1;
  // T ta,tb;
  // ta[1] = 1;
  U u[2];
  u[0][1].x = 5;
  u[0][1].y = u[0][1].x + 6;
  

  particle p;
  p.t[0][1] = 9;
  // match ".t"  with t a field from a struct of type particle
  // access_path(x, front @ field_access (Type "particle", Fieldname "t")::array_index(i)::tail)
  // ==> access_path(x, front @ field_access (Type "particle", Fieldname "ta")::tail)

  particle ps[5];
  p[3].t[0][1] = 8;
  // access(p, (array_access 3)::(field_access "t")::(array_access 0)::(array_access 1)::[])

  // p.ta[1] = 9;
/*
 array_access(t, i::rest)
 array_acces(ta, rest)=> if rest is empty, then it's only ta.

  smart constructor (base, accesses)  for building an array_access :
     if accessses = [] then base else array_access(base,accesses)
*/
  // ta[0] = 1:
  //int u[2] = {1,2};
  //t[0] = u;

  //t[0] = { 11][2]
  vect v[2];
  v[0] = {1,2};
  v[1] = {3,4};

  return 0;
}