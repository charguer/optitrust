
#include "pic_demo.h"
#include "pic_demo_aux.h"

// #include "particle_chunk_alloc.h"

// --------- Grid coordinate functions

int int_of_double(double a) {
  return (int) a - (a < 0.);
}

int wrap(int gridSize, int a) {
  return (a % gridSize + gridSize) % gridSize;
}

// --------- Grid Representation

#define nbCorners 8 // const int nbCorners = 8;

int cellOfCoord(int i, int j, int k) {
  return MINDEX3(gridX,gridY,gridZ,i,j,k);
}

// [idCellOfPos(pos)] computes the id of the cell that contains a position,
// assumed to be inside the grid (wrapAround needs to be called first)
int idCellOfPos(vect pos) {
  int iX = int_of_double(pos.x / cellX);
  int iY = int_of_double(pos.y / cellY);
  int iZ = int_of_double(pos.z / cellZ);
  return cellOfCoord(iX, iY, iZ);
}

vect wrapAround(vect pos) {
  // TODO ARTHUR: test if it is negative, althought with optimizations it's not needed
  double x = fmod(pos.x + areaX, areaX);
  double y = fmod(pos.y + areaY, areaY);
  double z = fmod(pos.z + areaZ, areaZ);
  return (vect) { x,y,z };
}

// relativePosX(x) computes the distance to the right-top-most
// nearest corner, divided by the width of a cell
double relativePosX(double x) {
  int iX = int_of_double(x / cellX);
  return (x - iX * cellX) / cellX;
}
double relativePosY(double y) {
  int iY = int_of_double(y / cellY);
  return (y - iY * cellY) / cellY;
}
double relativePosZ(double z) {
  int iZ = int_of_double(z / cellZ);
  return (z -  iZ * cellZ) / cellZ;
}
typedef struct {
  int iX;
  int iY;
  int iZ;
} coord;

coord coordOfCell(int idCell) {
  const int iZ = idCell % gridZ;
  const int iXY = idCell / gridZ;
  const int iY = iXY % gridY;
  const int iX = iXY / gridY;
  return (coord) { iX, iY, iZ };
}

typedef struct {
  int v[nbCorners];
} int_nbCorners;

typedef struct {
  double v[nbCorners];
} double_nbCorners;

typedef struct {
  vect v[nbCorners];
} vect_nbCorners;

int_nbCorners indicesOfCorners(int idCell) {
  const coord coord = coordOfCell(idCell);
  const int x = coord.iX;
  const int y = coord.iY;
  const int z = coord.iZ;
  const int x2 = wrap(gridX, x+1);
  const int y2 = wrap(gridY, y+1);
  const int z2 = wrap(gridZ, z+1);
  return (int_nbCorners) {
    cellOfCoord(x,y,z),
    cellOfCoord(x,y,z2),
    cellOfCoord(x,y2,z),
    cellOfCoord(x,y2,z2),
    cellOfCoord(x2,y,z),
    cellOfCoord(x2,y,z2),
    cellOfCoord(x2,y2,z),
    cellOfCoord(x2,y2,z2),
  };
}

// --------- Operations for field and charges

vect_nbCorners getFieldAtCorners(int idCell, vect* field) {
  const int_nbCorners indices = indicesOfCorners(idCell);
  vect_nbCorners res;
  for (int k = 0; k < nbCorners; k++) {
    res.v[k] = field[indices.v[k]];
  }
  return res;
}

// Total charge of the particles already placed in the cell for the next time step
// charge are also accumulated in the corners of the cells

void accumulateChargeAtCorners(double* deposit, int idCell, double_nbCorners charges) {
  const int_nbCorners indices = indicesOfCorners(idCell);
  for (int k = 0; k < nbCorners; k++) {
    deposit[indices.v[k]] += charges.v[k];
  }
}

// given the relative position inside a cell, with coordinates in the range [0,1],
// compute the coefficient for interpolation at each corner;
// the value for one corner is proportional to the volume between the particle
// and the opposite corner.

double_nbCorners cornerInterpolationCoeff(vect pos) {
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
  vect res = { 0., 0., 0. };
  for (int k = 0; k < nbCorners; k++) {
    res = vect_add(res, vect_mul(coeffs.v[k], matrix.v[k]));
  }
  return res;
}

// --------- Poisson solver

void computeRhoFromDeposit() { // reads [double* deposit], writes [double*** rho]
  // Each unit of particle in the deposit increases the charge density by 'factor'
  double factor = averageChargeDensity * nbCells / nbParticles; // = particleCharge / cellVolume;
  for (int i = 0; i < gridX; i++) {
    for (int j = 0; j < gridY; j++) {
      for (int k = 0; k < gridZ; k++) {
        rho[i][j][k] = factor * deposit[cellOfCoord(i,j,k)];
#ifdef DEBUG_CHARGE
        printf("rho[%d][%d][%d] = %lf\n", i, j, k, rho[i][j][k]);
#endif
      }
    }
  }
}

void resetDeposit() {
  for (int idCell = 0; idCell < nbCells; idCell++) {
    deposit[idCell] = 0;
  }
}

// [updateFieldUsingDeposit()] computes the electric field
// based on contributions of the particle in the [deposit] array.
// It clears the deposit array when done.
void updateFieldUsingDeposit() { // reads [double* deposit], writes [vect* field]
  computeRhoFromDeposit();
  computeFieldFromRho();
  resetDeposit();
}

// --------- Allocate and dellocate structures

void allocateStructures() {
  // TRACE("Allocate\n");

  allocateStructuresForPoissonSolver();

  // Allocate array for deposit
  deposit = (double*) malloc(nbCells * sizeof(double));

  // Allocate the field, not initialized in this function
  field = (vect*) malloc(nbCells * sizeof(vect));

  // Allocate bagsNext and bagsCur with empty bags in every cell
  bagsCur = (bag*) malloc(nbCells * sizeof(bag));
  bagsNext = (bag*) malloc(nbCells * sizeof(bag));
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_init_initial(&bagsCur[idCell]);
    bag_init_initial(&bagsNext[idCell]);
  }
}

void deallocateStructures() {
  // TRACE("Deallocate\n");

  deallocateStructuresForPoissonSolver();

  // Free the chunks
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_free_initial(&bagsCur[idCell]);
    bag_free_initial(&bagsNext[idCell]);
  }

  // Free arrays
  free(bagsCur);
  free(bagsNext);
  free(field);
}



// --------- Initialization

void computeConstants() {
  nbCells = gridX * gridY * gridZ;
  cellX = areaX / gridX;
  cellY = areaY / gridY;
  cellZ = areaZ / gridZ;
  const double cellVolume = cellX * cellY * cellZ;  // = totalVolume / nbCells
  const double totalVolume = nbCells * cellVolume; // = areaX * areaY * areaZ;
  const double totalCharge = averageChargeDensity * totalVolume;
  const double totalMass = averageMassDensity * totalVolume;
  particleCharge = totalCharge / nbParticles; // = averageChargeDensity * nbCells * cellVolume / nb_particles
  particleMass = totalMass / nbParticles;
  // Note: (particleCharge / particleMass) = (totalCharge / totalMass) = (averageChargeDensity / averageMassDensity) = -1.0
}

void addParticle(CHECKER_ONLY_COMMA(int idParticle) double x, double y, double z, double vx, double vy, double vz) {
  // Build the particle object
  const vect pos = { x, y, z };
  const vect speed = { vx, vy, vz };
  const particle particle = { pos, speed, CHECKER_ONLY(idParticle) };

  // Store the particle in the bag of the cell that contains the particle
  const int idCell = idCellOfPos(pos);
  bag_push_initial(&bagsCur[idCell], particle);

  // Deposit the charge of the particle at the corners of the target cell
  double_nbCorners contribs = cornerInterpolationCoeff(pos);
  accumulateChargeAtCorners(deposit, idCell, contribs);
}

// --------- Steps

// The Leaf-Frog method is used to obtain an order-2 interpolation for the
// differential equation; it consists of precomputing, for half-of-step backwards,
// the evolution of the particles speed.
void stepLeapFrog() {
  // TRACE("Computing initial poisson and leap-frog step\n");
  // Poisson solver to compute field at time zero, and reset deposit
  updateFieldUsingDeposit();
  // A leap-frog is half a step backwards
  double negHalfStepDuration = -0.5 * stepDuration;
  // For each cell from the grid
  for (int idCell = 0; idCell < nbCells; idCell++) {
    // Consider the bag of particles in that cell
    bag* b = &bagsCur[idCell];
    // Compute fields at corners of the cell
    vect_nbCorners field_at_corners = getFieldAtCorners(idCell, field);
    // For each particle in that cell

    bag_iter bag_it;
    for (particle* p = bag_iter_begin(&bag_it, b); p != NULL; p = bag_iter_next_common(&bag_it, false)) {
        double_nbCorners coeffs = cornerInterpolationCoeff(p->pos);
        vect fieldAtPos = matrix_vect_mul(coeffs, field_at_corners);
        vect accel = vect_mul(particleCharge / particleMass, fieldAtPos);
        p->speed = vect_add(p->speed, vect_mul(negHalfStepDuration, accel));
    }
  }
}

void step() {
  // For each cell from the grid
  for (int idCell = 0; idCell < nbCells; idCell++) {

    // Read the electric field that applies to the corners of the cell considered
    vect_nbCorners field_at_corners = getFieldAtCorners(idCell, field);

    // Consider the bag of particles in that cell
    bag* b = &bagsCur[idCell];

    bag_iter bag_it;
    // int k=0;
    for (particle* p = bag_iter_begin(&bag_it, b); p != NULL; p = bag_iter_next_common(&bag_it, true)) {

      // Interpolate the field based on the position relative to the corners of the cell
      double_nbCorners coeffs = cornerInterpolationCoeff(p->pos);
      vect fieldAtPos = matrix_vect_mul(coeffs, field_at_corners);

      // Compute the acceleration: F = m*a and F = q*E  gives a = q/m*E
      vect accel = vect_mul(particleCharge / particleMass, fieldAtPos);

      // Compute the new speed and position for the particle.
      vect speed2 = vect_add(p->speed, vect_mul(stepDuration, accel));
      vect pos2 = vect_add(p->pos, vect_mul(stepDuration, speed2));
      pos2 = wrapAround(pos2);
      particle p2 = { pos2, speed2, CHECKER_ONLY(p->id) };

      // Compute the location of the cell that now contains the particle
      int idCell2 = idCellOfPos(pos2);

      // Push the updated particle into the bag associated with its target cell
      bag_push(&bagsNext[idCell2], p2);

      // Deposit the charge of the particle at the corners of the target cell
      double_nbCorners contribs = cornerInterpolationCoeff(pos2);
      accumulateChargeAtCorners(deposit, idCell2, contribs);
    }
    bag_init_initial(b);
  }

  // For the next time step, the contents of bagNext is moved into bagCur (which is empty)
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_swap(&bagsCur[idCell], &bagsNext[idCell]);
  }

  // Update the new field based on the deposit array, then reset this array
  updateFieldUsingDeposit();
}


// --------- Main

  int main(int argc, char** argv) {

  loadParameters(argc, argv);

  computeConstants();

  allocateStructures();

  resetDeposit();

  createParticles();

  stepLeapFrog();

#if defined(PRINTPERF) || defined(PRINTSTEPS)
  double timeStart = omp_get_wtime();
#endif
#ifdef PRINTSTEPS
  double nextReport = timeStart + 1.0;
#endif

// Foreach time step
  for (int idStep = 0; idStep < nbSteps; idStep++) {
#ifdef PRINTSTEPS
    if (omp_get_wtime() > nextReport) {
      nextReport += 1.0;
      printf("Step %d\n", idStep);
    }
#endif
    step();
  }

#if defined(PRINTPERF)
  reportPerformance(timeStart);
#endif

#ifdef CHECKER
  reportParticlesState();
#endif

  deallocateStructures();

  free(deposit);
}

