typedef struct {
  double x;
  double y;
  double z;
} vect;

const int nbCorners = 8;

typedef struct {
  double val[nbCorners];
} double_nbCorners;


double_nbCorners cornerInterpolationCoeff(vect pos) {
  double rx = pos.x;
  double ry = pos.y;
  double rz = pos.z;
  return { {
    (1. + -1. * rx) * (1. + -1. * ry) * (1. + -1. * rz),
    (1. + -1. * rx) * (1. + -1. * ry) * (0. + 1. * rz),
    (1. + -1. * rx) * (0. + 1. * ry) * (1. + -1. * rz),
    (1. + -1. * rx) * (0. + 1. * ry) * (0. + 1. * rz),
    (0. + 1. * rx) * (1. + -1. * ry) * (1. + -1. * rz),
    (0. + 1. * rx) * (1. + -1. * ry) * (0. + 1. * rz),
    (0. + 1. * rx) * (0. + 1. * ry) * (1. + -1. * rz),
    (0. + 1. * rx) * (0. + 1. * ry) * (0. + 1. * rz),
  } };
}