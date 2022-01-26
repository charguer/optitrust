
const int CHUNKSIZE = 128;

int t[CHUNKSIZE];
int u[2*CHUNKSIZE+1];


typedef struct {
  double x;
} foo;

typedef struct /*vect*/ {
  double x;
  double y;
  double z;
} vect, vect2;


struct list {
  int head;
  struct list* tail;
};

/*
typedef struct tlist {
  int head;
  tlist* tail;
} tlist;
  gcc -std=c99 c_small_test.cpp

*/

int main() {
  const vect2 v = {0,0,0};
  const double y = v.x;
  return 1;
}

typedef enum {  A, B } bar;

vect vect_mul(double d, vect v) {
  return {(d * v.x), (d * v.y), (d * v.z)};
}

// gcc -std=c++11 c_small_test.cpp