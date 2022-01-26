
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
} vect, vec2;

int main() {
  const vec2 v = {0,0,0};
  const double y = v.x;
  return 1;
}

typedef enum {  A, B } bar;

