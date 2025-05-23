#include <optitrust.h>


// =========================================================
// Vector representation

typedef struct {
  double x, y, z;
} vect;

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

// =========================================================
// Grid representation

const double areaX = 10.;
const double areaY = 10.;
const double areaZ = 10.;

const int gridX = 64;
const int gridY = 64;
const int gridZ = 64;

const int nbCells = ((gridX * gridY) * gridZ);
const double cellX = (areaX / (double)gridX);
const double cellY = (areaY / (double)gridY);
const double cellZ = (areaZ / (double)gridZ);

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

void corner_interpolation_coeff(vect pos, double* r) {
  __writes("r ~> Matrix1(nbCorners)");

  const double rX = relativePosX(pos.x);
  const double rY = relativePosY(pos.y);
  const double rZ = relativePosZ(pos.z);
  const double cX = 1. + -1. * rX;
  const double cY = 1. + -1. * rY;
  const double cZ = 1. + -1. * rZ;
  // these ghosts will not be necessary once we have autofocus on array accesses
  __ghost([&] {
    __consumes("r ~> UninitMatrix1(nbCorners)");
    __produces("&r[MINDEX1(8, 0)] ~> UninitCell");
    __produces("&r[MINDEX1(8, 1)] ~> UninitCell");
    __produces("&r[MINDEX1(8, 2)] ~> UninitCell");
    __produces("&r[MINDEX1(8, 3)] ~> UninitCell");
    __produces("&r[MINDEX1(8, 4)] ~> UninitCell");
    __produces("&r[MINDEX1(8, 5)] ~> UninitCell");
    __produces("&r[MINDEX1(8, 6)] ~> UninitCell");
    __produces("&r[MINDEX1(8, 7)] ~> UninitCell");
    __admitted();
  }, "");
  r[MINDEX1(8, 0)] = cX * cY * cZ;
  r[MINDEX1(8, 1)] = cX * cY * rZ;
  r[MINDEX1(8, 2)] = cX * rY * cZ;
  r[MINDEX1(8, 3)] = cX * rY * rZ;
  r[MINDEX1(8, 4)] = rX * cY * cZ;
  r[MINDEX1(8, 5)] = rX * cY * rZ;
  r[MINDEX1(8, 6)] = rX * rY * cZ;
  r[MINDEX1(8, 7)] = rX * rY * rZ;
  __ghost([&] {
    __consumes("&r[MINDEX1(8, 0)] ~> Cell");
    __consumes("&r[MINDEX1(8, 1)] ~> Cell");
    __consumes("&r[MINDEX1(8, 2)] ~> Cell");
    __consumes("&r[MINDEX1(8, 3)] ~> Cell");
    __consumes("&r[MINDEX1(8, 4)] ~> Cell");
    __consumes("&r[MINDEX1(8, 5)] ~> Cell");
    __consumes("&r[MINDEX1(8, 6)] ~> Cell");
    __consumes("&r[MINDEX1(8, 7)] ~> Cell");
    __produces("r ~> Matrix1(nbCorners)");
    __admitted();
  }, "");
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

  __admitted(); // TODO: remove?
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

void simulate_single_cell(double deltaT,
  particle* particles, int nbParticles,
  vect* fieldAtCorners, int nbSteps,
  double pCharge, double pMass)
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
      // TODO: __ghost(particle_open, "&particles[MINDEX1(nbParticles, idPart)]");
      /*
      LATER: Generate for every record:

      __GHOST(particle_open) {
        __requires("p : ptr<particle>");
        __consumes("p ~> Cell");
        __produces("p.pos ~> Cell");
        __produces("p.speed ~> Cell");
        __admitted();
      }, "");

      + ghost_inline
      */

      // Interpolate the field based on the position relative to the corners of the cell
      double* const coeffs = MALLOC1(double, nbCorners);
      corner_interpolation_coeff(particles[MINDEX1(nbParticles, idPart)].pos, coeffs);
      const vect fieldAtPos = matrix_vect_mul(coeffs, fieldAtCorners);
      free(coeffs);

      // Compute the acceleration: F = m*a and F = q*E  gives a = q/m*E
      const vect accel = vect_mul(pCharge / pMass, fieldAtPos);

      // Compute the new speed and position for the particle.
      const vect speed2 = vect_add(particles[MINDEX1(nbParticles, idPart)].speed, vect_mul(deltaT, accel));
      const vect pos2 = vect_add(particles[MINDEX1(nbParticles, idPart)].pos, vect_mul(deltaT, speed2));

      // const particle p2 = { .pos = pos2, .speed = speed2, .charge = p.charge, .mass = p.mass };
      particles[MINDEX1(nbParticles, idPart)].pos = pos2;
      particles[MINDEX1(nbParticles, idPart)].speed = speed2;
      __ghost([&] {
        __consumes("&particles[MINDEX1(nbParticles, idPart)].pos ~> Cell");
        __consumes("&particles[MINDEX1(nbParticles, idPart)].speed ~> Cell");
        __produces("&particles[MINDEX1(nbParticles, idPart)] ~> Cell");
        __admitted();
      }, "");
    }
  }
}

/*
int simulate_core(double deltaT,
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

  __GHOST_BEGIN(focus1, ro_matrix1_focus, "curBagSize, idCell");
  const int nbParts = curBagSize[MINDEX1(nbCells, idCell)];
  // __ghost(assume, "in_range(nbParts, 0..maxPartsPerCell)");
  for (int idPart = 0; idPart < nbParts; idPart++) {
    __ghost(assume, "in_range(idPart, 0..maxPartsPerCell)"); // TODO: remove
    __GHOST_BEGIN(focus2, ro_matrix2_focus, "curBag, idCell, idPart");
    const particle p = curBag[MINDEX2(nbCells, maxPartsPerCell, idCell, idPart)];
    __GHOST_END(focus2);

    // Interpolate the field based on the position relative to the corners of the cell
    const double_nbCorners coeffs = corner_interpolation_coeff(p.pos);
    const vect fieldAtPos = matrix_vect_mul(coeffs, fieldAtCorners);

    // Compute the acceleration: F = m*a and F = q*E  gives a = q/m*E
    const vect accel = vect_mul(p.charge / p.mass, fieldAtPos);

    // Compute the new speed and position for the particle.
    const vect speed2 = vect_add(p.speed, vect_mul(deltaT, accel));
    const vect pos2 = vect_add(p.pos, vect_mul(deltaT, speed2));
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
