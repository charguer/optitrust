const int CHUNKSIZE = 128;

int t[CHUNKSIZE];

typedef struct {
  double x;
  double y;
  double z;
} vect;

vect vect_mul(double d, vect v) {
  return {(d * v.x), (d * v.y), (d * v.z)};
}

int main() {
  int a = 0;
  int& b = a;
  const int r = 0;
  const int & s = r;

  const int & t = a;
  return b + 1;   /* not supported*/
}

// gcc -std=c++11 c_small_test.cpp
