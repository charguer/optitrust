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
  r.v[0] = (1. + (-1.) * rX) * (1. + (-1.) * rY) * (1. + (-1.) * rZ);
  r.v[1] = (1. + (-1.) * rX) * (1. + (-1.) * rY) * (0. + 1. * rZ);
  r.v[2] = (1. + (-1.) * rX) * (0. + 1. * rY) * (1. + (-1.) * rZ);
  r.v[3] = (1. + (-1.) * rX) * (0. + 1. * rY) * (0. + 1. * rZ);
  r.v[4] = (0. + 1. * rX) * (1. + (-1.) * rY) * (1. + (-1.) * rZ);
  r.v[5] = (0. + 1. * rX) * (1. + (-1.) * rY) * (0. + 1. * rZ);
  r.v[6] = (0. + 1. * rX) * (0. + 1. * rY) * (1. + (-1.) * rZ);
  r.v[7] = (0. + 1. * rX) * (0. + 1. * rY) * (0. + 1. * rZ);
  return r;
}
