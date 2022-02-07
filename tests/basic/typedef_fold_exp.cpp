typedef unsigned int uint;

typedef const double cdouble;

typedef struct {
  uint x;
  uint y;
} vect;

typedef int **mat2d;

typedef mat2d *mat3d;

int main() {
  unsigned int x;
  unsigned int t[3];
  int a = 1;
  int b;
  cdouble y1 = 1.;
  const double y2 = 2.;
  vect v;
  mat2d m;
  mat3d M;
  mat3d *T;
}
