typedef unsigned int uint;

typedef const double cdouble;

typedef struct { unsigned int x; unsigned int y; } vect;

typedef int** mat2d;

// it's not entirely clear what is the meaning of 'const' in the return type, but let's see whether it compiles.
const double f(unsigned int, vect v, int***) {
   return 2.0;
}

int main() {
  unsigned int x;
  unsigned int t[3];
  const double y1 = 1.0, y2 = 2.0;
  vect v;
  int** m;
  int*** M;
  int**** T;
}

