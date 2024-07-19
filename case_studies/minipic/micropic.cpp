#include <optitrust.h>


// =========================================================
// Vector representation

typedef struct {
  double x, y, z;
} vect;

REGISTER_STRUCT_ACCESS(x)
REGISTER_STRUCT_ACCESS(y)
REGISTER_STRUCT_ACCESS(z)
/*
template<typename T> T __struct_access_x(T* v) {
  __pure();
  __admitted();
  return v.x;
}

template<typename T> T __struct_access_y(T* v) {
  __pure();
  __admitted();
  return v.y;
}

template<typename T> T __struct_access_y(T* v) {
  __pure();
  __admitted();
  return v.z;
}
*/
vect vect_add(vect v1, vect v2) {
  __pure();
  __admitted();

  return (vect) { v1.x + v2.x, v1.y + v2.y, v1.z + v2.z };
}

vect vect_mul(double d, vect v) {
  __pure();
  __admitted();

  return (vect) { d * v.x, d * v.y, d * v.z };
}

// =========================================================
// Particle representation

typedef struct {
  vect pos;
  vect speed;
  /*
  Scaling makes no sense if charge/mass vary ?
  double charge;
  double mass;
  */
} particle;

REGISTER_STRUCT_ACCESS(pos)
REGISTER_STRUCT_ACCESS(speed)
REGISTER_STRUCT_ACCESS(charge)
REGISTER_STRUCT_ACCESS(mass)

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

// const int maxPartsPerCell = 1000000;

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

double* cornerInterpolationCoeff(vect pos) {
  __produces("_Res ~> Matrix1(nbCorners)");
  __admitted();

  const double rX = relativePosX(pos.x);
  const double rY = relativePosY(pos.y);
  const double rZ = relativePosZ(pos.z);
  const double cX = 1. + -1. * rX;
  const double cY = 1. + -1. * rY;
  const double cZ = 1. + -1. * rZ;
  double* const r = (double* const) MALLOC1(nbCorners, sizeof(double));
  r[0] = cX * cY * cZ;
  r[1] = cX * cY * rZ;
  r[2] = cX * rY * cZ;
  r[3] = cX * rY * rZ;
  r[4] = rX * cY * cZ;
  r[5] = rX * cY * rZ;
  r[6] = rX * rY * cZ;
  r[7] = rX * rY * rZ;
  return r;
}

vect matrix_vect_mul(double* coeffs, vect* matrix) {
  __reads("coeffs ~> Matrix1(nbCorners)");
  __reads("matrix ~> Matrix1(nbCorners)");

  vect res = { 0., 0., 0. };
  for (int k = 0; k < nbCorners; k++) {
    __xreads("&coeffs[MINDEX1(nbCorners, k)] ~> Cell");
    __xreads("&matrix[MINDEX1(nbCorners, k)] ~> Cell");

    res = vect_add(res, vect_mul(coeffs[MINDEX1(nbCorners, k)], matrix[MINDEX1(nbCorners, k)]));
  }

  __admitted();
  return res;
}


// =========================================================
// Coordinates of a cell
/*
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

  const int iX = int_of_double((pos.x / cellX));
  const int iY = int_of_double((pos.y / cellY));
  const int iZ = int_of_double((pos.z / cellZ));
  return cellOfCoord(iX, iY, iZ);
}
*/
// =========================================================
// Core loop

void simulate_single_cell(double stepDuration,
  particle* particles, int nbParticles,
  vect* fieldAtCorners, int nbSteps,
  double particleCharge, double particleMass)
{
  __modifies("particles ~> Matrix1(nbParticles)");
  __reads("fieldAtCorners ~> Matrix1(nbCorners)");

  for (int idStep = 0; idStep < nbSteps; idStep++) {
    for (int idPart = 0; idPart < nbParticles; idPart++) {
      __xmodifies("&particles[MINDEX1(nbParticles, idPart)] ~> Cell");

      /* TODO?
      particle *const p = &particles[MINDEX1(nbParticles, idPart)]; */
      __ghost([&] {
        __consumes("&particles[MINDEX1(nbParticles, idPart)] ~> Cell");
        __produces("&particles[MINDEX1(nbParticles, idPart)].pos ~> Cell");
        __produces("&particles[MINDEX1(nbParticles, idPart)].speed ~> Cell");
        __admitted();
      }, "");

      // Interpolate the field based on the position relative to the corners of the cell
      double* const coeffs = cornerInterpolationCoeff(particles[MINDEX1(nbParticles, idPart)].pos);
      const vect fieldAtPos = matrix_vect_mul(coeffs, fieldAtCorners);
      MFREE1(nbCorners, coeffs);

      // Compute the acceleration: F = m*a and F = q*E  gives a = q/m*E
      const vect accel = vect_mul(particleCharge / particleMass, fieldAtPos);

      // Compute the new speed and position for the particle.
      const vect speed2 = vect_add(particles[MINDEX1(nbParticles, idPart)].speed, vect_mul(stepDuration, accel));
      const vect pos2 = vect_add(particles[MINDEX1(nbParticles, idPart)].pos, vect_mul(stepDuration, speed2));

      // const particle p2 = { .pos = pos2, .speed = speed2, .charge = p.charge, .mass = p.mass };
      particles[MINDEX1(nbParticles, idPart)].pos = pos2;
      particles[MINDEX1(nbParticles, idPart)].speed = speed2;
      __ghost([&] {
        __produces("&particles[MINDEX1(nbParticles, idPart)] ~> Cell");
        __consumes("&particles[MINDEX1(nbParticles, idPart)].pos ~> Cell");
        __consumes("&particles[MINDEX1(nbParticles, idPart)].speed ~> Cell");
        __admitted();
      }, "");
    }
  }
}

/*
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
  const int nbParts = curBagSize[MINDEX1(nbCells, idCell)];
  // __ghost(assume, "in_range(nbParts, 0..maxPartsPerCell)");
  for (int idPart = 0; idPart < nbParts; idPart++) {
    __ghost(assume, "in_range(idPart, 0..maxPartsPerCell)"); // TODO: remove
    __GHOST_BEGIN(focus2, matrix2_ro_focus, "curBag, idCell, idPart");
    const particle p = curBag[MINDEX2(nbCells, maxPartsPerCell, idCell, idPart)];
    __GHOST_END(focus2);

    // Interpolate the field based on the position relative to the corners of the cell
    const double_nbCorners coeffs = cornerInterpolationCoeff(p.pos);
    const vect fieldAtPos = matrix_vect_mul(coeffs, fieldAtCorners);

    // Compute the acceleration: F = m*a and F = q*E  gives a = q/m*E
    const vect accel = vect_mul(p.charge / p.mass, fieldAtPos);

    // Compute the new speed and position for the particle.
    const vect speed2 = vect_add(p.speed, vect_mul(stepDuration, accel));
    const vect pos2 = vect_add(p.pos, vect_mul(stepDuration, speed2));
    const particle p2 = { pos2, speed2 };

    // Compute the location of the cell that now contains the particle
    // TODO: wrap around pos2
    const int idCellNext = idCellOfPos(pos2);
    __ghost(assume, "in_range(idCellNext, 0..nbCells)"); // TODO: remove

    // Push the updated particle into the bag associated with its target cell
    __GHOST_BEGIN(focus3, matrix1_focus, "nextBagSize, idCellNext");
    const int idPartNext = nextBagSize[MINDEX1(nbCells, idCellNext)]++;
    __GHOST_END(focus3);
    __ghost(assume, "in_range(idPartNext, 0..maxPartsPerCell)"); // TODO: remove
    __GHOST_BEGIN(focus4, matrix2_focus, "nextBag, idCellNext, idPartNext");
    nextBag[MINDEX2(nbCells, maxPartsPerCell, idCellNext, idPartNext)] = p2;
    __GHOST_END(focus4);
  }
  curBagSize[MINDEX1(nbCells, idCell)] = 0;
  __GHOST_END(focus1);
}
*/

// =========================================================

// 1. dérouler matmuls
// 1.5. set_explicit + simpl_proj
// 2. mise à l'échelle données
//   -- http://www.chargueraud.org/research/2022/optitrust/optitrust.pdf sect 4.5

// --optional
// 3. AoS -> SoA
// 4. AoSoA

// --optional
// 5. loop split
// 6. add modulo-2 on bag structures
