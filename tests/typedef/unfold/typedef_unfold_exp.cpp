typedef unsigned int uint;

typedef const double cdouble;

typedef struct {
  unsigned int x;
  unsigned int y;
} vect;

typedef vect myvect;

typedef int** mat2d;

typedef int*** mat3d;

const double f(unsigned int n, myvect v, int*** m) { return 2.; }

int main() {
  unsigned int x;
  unsigned int t[3];
  cdouble y1 = 1.;
  myvect v;
  int** m;
  int*** M;
  int**** T;
}
