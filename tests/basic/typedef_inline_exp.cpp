typedef unsigned int uint;

typedef double const cdouble;

typedef struct {
  unsigned int x;
  unsigned int y;
} vect;

typedef vect myvect;

typedef int **mat2d;

typedef int ***mat3d;

double const f(uint n, myvect v, int ***m) { return 2.; }

int main() {
  uint x;
  uint t[3];
  cdouble y1 = 1.;
  myvect v;
  int **m;
  int ***M;
  int ****T;
}