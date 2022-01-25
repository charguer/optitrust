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
