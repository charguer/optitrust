typedef unsigned int uint;

typedef double const cdouble;

typedef struct { unsigned int x; unsigned int y; } vect;

typedef int** mat2d;

typedef int*** mat3d; // to be folded before mat2d, else M would be of type mat2d*

int main() {
  unsigned int x;
  unsigned int t[3];
  int a = 1;
  int b;
  const double y1 = 1.0;
  double const y2 = 2.0;
  vect v;
  int** m;
  int*** M;
  int**** T;
}


