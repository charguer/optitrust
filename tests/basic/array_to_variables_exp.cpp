typedef struct {
  int x;
  int y;
} vect;

typedef int T[2];

typedef vect U[2];

typedef struct {
  T t[2];
  int g;
} particle;

void f(int x, int y, T t[2]) {
  t[0][0] = 0;
  t[0][1] = 0;
  t[1][0] = 0;
  t[1][1] = 0;
  x = 0;
  y = 1;
}

void g(int x, int y, U t[2]) {
  t[0][0].x = 0;
  x = 0;
  y = 1;
}

int main() {
  T t[2];
  t[0][1] = 4;
  t[1][0] = 5;
  t[0][0] = 1;
  t[1][1] = 2;
  U ub;
  U ua;
  ua[1].x = 5;
  ua[1].y = (ua[1].x + 6);
  ub[1].x = 5;
  ub[1].y = (ub[1].x + 6);
  particle p;
  p.t[0][1] = 9;
  p.t[1][1] = 2;
  particle ps[5];
  ps[3].t[0][1] = 8;
  ps[3].t[1][0] = 10;
  vect vb;
  vect va;
  vect a = {1, 2};
  va = a;
  vb = a;
  return 0;
}