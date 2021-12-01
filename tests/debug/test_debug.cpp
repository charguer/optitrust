double const areaX = 10.;

double const areaY = 10.;

double const areaZ = 10.;

double const stepDuration = 0.2;

double const particleCharge = 10.;

double const particleMass = 5.;

int const gridX = 64;

int const gridY = 64;

int const gridZ = 64;

int const nbCells = ((gridX * gridY) * gridZ);

double const cellX = (areaX / gridX);

double const cellY = (areaY / gridY);

double const cellZ = (areaZ / gridZ);

const int nbCorners = 8;

typedef struct {
  double x;
  double y;
  double z;
} vect;

typedef struct {
  double v[nbCorners];
} double_nbCorners;

int int_of_double(double a) { return ((int)a - (a < 0.)); }


double_nbCorners vect8_mul(double const a, const double_nbCorners data) {
  double_nbCorners res;
  for (int k = 0; (k < nbCorners); k++) {
    res.v[k] = (a * data.v[k]);
  }
  return res;
}

double_nbCorners cornerInterpolationCoeff(vect pos) {
  double coefX[8] = {1., 1., 1., 1., 0., 0., 0., 0.};
  double signX[8] = {(-1.), (-1.), (-1.), (-1.), 1., 1., 1., 1.};
  double coefY[8] = {1., 1., 0., 0., 1., 1., 0., 0.};
  double signY[8] = {(-1.), (-1.), 1., 1., (-1.), (-1.), 1., 1.};
  double coefZ[8] = {1., 0., 1., 0., 1., 0., 1., 0.};
  double signZ[8] = {(-1.), 1., (-1.), 1., (-1.), 1., (-1.), 1.};
  int iX = int_of_double((pos.x / cellX));
  double rX = ((pos.x - (iX * cellX)) / cellX);
  int iY = int_of_double((pos.y / cellY));
  double rY = ((pos.y - (iY * cellY)) / cellY);
  int iZ = int_of_double((pos.z / cellZ));
  double rZ = ((pos.z - (iZ * cellZ)) / cellZ);
  double_nbCorners r;
  for (int k = 0; (k < nbCorners); k++) {
    r.v[k] = (((coefX[k] + (signX[k] * rX)) * (coefY[k] + (signY[k] * rY))) *
              (coefZ[k] + (signZ[k] * rZ)));
  }
  return r;
}


int main() {
  vect a = {0.0, 0.0, 0.0};

  double_nbCorners coeffs2 =
              cornerInterpolationCoeff(a);

  double_nbCorners deltaChargeOnCorners =
    vect8_mul (particleCharge, coeffs2);

  return 0;
}