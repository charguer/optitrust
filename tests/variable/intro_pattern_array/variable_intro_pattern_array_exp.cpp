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
  const double rx = pos.x;
  const double ry = pos.y;
  const double rz = pos.z;
  double coef_x[8] = {1., 1., 1., 1., 0., 0., 0., 0.};
  double sign_x[8] = {-1., -1., -1., -1., 1., 1., 1., 1.};
  double coef_y[8] = {1., 1., 0., 0., 1., 1., 0., 0.};
  double sign_y[8] = {-1., -1., 1., 1., -1., -1., 1., 1.};
  double coef_z[8] = {1., 0., 1., 0., 1., 0., 1., 0.};
  double sign_z[8] = {-1., 1., -1., 1., -1., 1., -1., 1.};
  return (double_nbCorners){
      {(coef_x[0] + sign_x[0] * rx) * (coef_y[0] + sign_y[0] * ry) *
           (coef_z[0] + sign_z[0] * rz),
       (coef_x[1] + sign_x[1] * rx) * (coef_y[1] + sign_y[1] * ry) *
           (coef_z[1] + sign_z[1] * rz),
       (coef_x[2] + sign_x[2] * rx) * (coef_y[2] + sign_y[2] * ry) *
           (coef_z[2] + sign_z[2] * rz),
       (coef_x[3] + sign_x[3] * rx) * (coef_y[3] + sign_y[3] * ry) *
           (coef_z[3] + sign_z[3] * rz),
       (coef_x[4] + sign_x[4] * rx) * (coef_y[4] + sign_y[4] * ry) *
           (coef_z[4] + sign_z[4] * rz),
       (coef_x[5] + sign_x[5] * rx) * (coef_y[5] + sign_y[5] * ry) *
           (coef_z[5] + sign_z[5] * rz),
       (coef_x[6] + sign_x[6] * rx) * (coef_y[6] + sign_y[6] * ry) *
           (coef_z[6] + sign_z[6] * rz),
       (coef_x[7] + sign_x[7] * rx) * (coef_y[7] + sign_y[7] * ry) *
           (coef_z[7] + sign_z[7] * rz)}};
}
