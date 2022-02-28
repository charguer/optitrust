typedef struct {

  double x;
  double y;
  double z;
} vect;


typedef struct {

 vect pos;
 vect speed;
} particle;

typedef struct {
  double v[8];
} double_nbCorners;

typedef struct {
  vect v[8];
} vect_nbCorners;


double_nbCorners cornerInterpolationCoeff(vect pos) {
  const double rX = pos.x;
  const double rY = pos.y;
  const double rZ = pos.z;
  const double cX = 1. + -1. * rX;
  const double cY = 1. + -1. * rY;
  const double cZ = 1. + -1. * rZ;
  double_nbCorners r;
  r.v[0] = cX * cY * cZ;
  r.v[1] = cX * cY * rZ;
  r.v[2] = cX * rY * cZ;
  r.v[3] = cX * rY * rZ;
  r.v[4] = rX * cY * cZ;
  r.v[5] = rX * cY * rZ;
  r.v[6] = rX * rY * cZ;
  r.v[7] = rX * rY * rZ;
  return r;
}
