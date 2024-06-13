#include <optitrust.h>


// =========================================================
// Vector representation

typedef struct {
  double x, y, z;
} vect;

vect vect_add(vect v1, vect v2) {
  return (vect) { v1.x + v2.x, v1.y + v2.y, v1.z + v2.z };
}

vect vect_mul(double d, vect v) {
  return (vect) { d * v.x, d * v.y, d * v.z };
}

// =========================================================
// Particle representation

typedef struct {
  vect pos;
  vect speed;
  double charge;
  double mass;
} particle;

// =========================================================
// Grid representation

const double areaX = 10.;
const double areaY = 10.;
const double areaZ = 10.;

const int gridX = 64;
const int gridY = 64;
const int gridZ = 64;

const int nbCells = ((gridX * gridY) * gridZ);
const double cellX = (areaX / gridX);
const double cellY = (areaY / gridY);
const double cellZ = (areaZ / gridZ);

const int maxPartsPerCell = 1000000;

// TODO: support return in type check to remove addmitteds

// =========================================================
// Interpolation formulae

// from double to int
int int_of_double(double a) {
  __pure();
  __admitted();

  return (int) a - (a < 0.);
}

double relativePosX(double x) {
  __pure();
  __admitted();

  int iX = int_of_double(x / cellX);
  return (x - iX * cellX) / cellX;
}
double relativePosY(double y) {
  __pure();
  __admitted();

  int iY = int_of_double(y / cellY);
  return (y - iY * cellY) / cellY;
}
double relativePosZ(double z) {
  __pure();
  __admitted();

  int iZ = int_of_double(z / cellZ);
  return (z -  iZ * cellZ) / cellZ;
}

const int nbCorners = 8;

typedef struct { int v[nbCorners]; } int_nbCorners;

typedef struct { double v[nbCorners]; } double_nbCorners;

typedef struct { vect v[nbCorners]; } vect_nbCorners;

double_nbCorners cornerInterpolationCoeff(vect pos) {
  __pure();
  __admitted();

  const double rX = relativePosX(pos.x);
  const double rY = relativePosY(pos.y);
  const double rZ = relativePosZ(pos.z);
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

vect matrix_vect_mul(const double_nbCorners coeffs, const vect_nbCorners matrix) {
  __pure();
  __admitted();

  vect res = { 0., 0., 0. };
  for (int k = 0; k < nbCorners; k++) {
    res = vect_add(res, vect_mul(coeffs.v[k], matrix.v[k]));
  }
  return res;
}


// =========================================================
// Coordinates of a cell

typedef struct {
  int iX;
  int iY;
  int iZ;
} coord;

int cellOfCoord(int i, int j, int k) {
  __pure();
  __admitted();

  return MINDEX3(gridX, gridY, gridZ, i, j, k);
}

int idCellOfPos(vect pos) {
  __pure();
  __admitted();

  int iX = int_of_double((pos.x / cellX));
  int iY = int_of_double((pos.y / cellY));
  int iZ = int_of_double((pos.z / cellZ));
  return cellOfCoord(iX, iY, iZ);
}

// =========================================================
// Core loop


int simulate_core(double stepDuration,
  particle* curBag, int* curBagSize,
  particle* nextBag, int* nextBagSize,
  vect_nbCorners fieldAtCorners, int idStep, int idCell)
{
  __requires("in_range(idCell, 0..nbCells)");
  __reads("curBag ~> Matrix2(nbCells, maxPartsPerCell)");
  __modifies("nextBag ~> Matrix2(nbCells, maxPartsPerCell)");
  __modifies("curBagSize ~> Matrix1(nbCells)");
  __modifies("nextBagSize ~> Matrix1(nbCells)");
  __reads("fieldAtCorners ~> Matrix1(nbCorners)");

  __GHOST_BEGIN(focus1, matrix1_ro_focus, "curBagSize, idCell");
  int nbParts = curBagSize[MINDEX1(nbCells, idCell)];
  for (int idPart = 0; idPart < nbParts; idPart++) {
    __ghost(assume, "in_range(idPart, 0..maxPartsPerCell)"); // TODO: remove
    __GHOST_BEGIN(focus2, matrix2_ro_focus, "curBag, idCell, idPart");
    particle* p = &curBag[MINDEX2(nbCells, maxPartsPerCell, idCell, idPart)];
    __GHOST_END(focus2);

    // Interpolate the field based on the position relative to the corners of the cell
    double_nbCorners coeffs = cornerInterpolationCoeff(p->pos);
    vect fieldAtPos = matrix_vect_mul(coeffs, fieldAtCorners);

    // Compute the acceleration: F = m*a and F = q*E  gives a = q/m*E
    vect accel = vect_mul(p->charge / p->mass, fieldAtPos);

    // Compute the new speed and position for the particle.
    vect speed2 = vect_add(p->speed, vect_mul(stepDuration, accel));
    vect pos2 = vect_add(p->pos, vect_mul(stepDuration, speed2));
    particle p2 = { pos2, speed2 };

    // Compute the location of the cell that now contains the particle
    int idCellNext = idCellOfPos(pos2);

    // Push the updated particle into the bag associated with its target cell
    __GHOST_BEGIN(focus3, matrix1_focus, "nextBagSize, idCellNext");
    int idPartNext = nextBagSize[MINDEX1(nbCells, idCellNext)]++;
    __GHOST_END(focus3);
    __GHOST_BEGIN(focus4, matrix2_focus, "nextBag, idCellNext, idPartNext");
    nextBag[MINDEX2(nbCells, maxPartsPerCell, idCellNext, idPartNext)] = p2;
    __GHOST_END(focus4);
  }
  curBagSize[MINDEX1(nbCells, idCell)] = 0;
  __GHOST_END(focus1);
}

// =========================================================

// 1. dérouler matmuls
// 2. mise à l'échelle données
//   -- http://www.chargueraud.org/research/2022/optitrust/optitrust.pdf sect 4.5

// --optional
// 3. AoS -> SoA
// 4. AoSoA

// --optional
// 5. loop split
// 6. add modulo-2 on bag structures
