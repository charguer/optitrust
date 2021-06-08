typedef unsigned int uint;

typedef const double cdouble;

typedef struct { uint x; uint y; } vect;

typedef vect myvect; // typedef to be removed

typedef int** mat2d;

typedef int*** mat3d; // typedef to be removed

cdouble f(uint n, myvect v, mat3d m) {
   return 2.0;
}

int main() {
  uint x;
  uint t[3];
  cdouble y1 = 1.0, t2 = 2.0;
  myvect v;
  mat2d m;
  mat3d M;
  mat3d* T;
}

