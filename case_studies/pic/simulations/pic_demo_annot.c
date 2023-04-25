#include "pic_demo.h"
#include "pic_demo_aux.h"
#include "optitrust.h"

// --------- Grid coordinate functions

inline int int_of_double(double a) {
  return (int) a - (a < 0.);
}

inline int wrap(int gridSize, int a) {
  return (a % gridSize + gridSize) % gridSize;
}

// --------- Grid Representation

const int nbCorners = 8;

/// reads gridX + gridY + gridZ -> cell int
inline int cellOfCoord(int i, int j, int k) {
  return MINDEX3(gridX, gridY, gridZ, i, j, k);
}

// [idCellOfPos(pos)] computes the id of the cell that contains a position,
// assumed to be inside the grid (wrapArea needs to be called first)
/// reads
///   cellX + cellY + cellZ -> cell double
///   gridX + gridY + gridZ -> cell int
int idCellOfPos(vect pos) {
  const int iX = int_of_double(pos.x / cellX);
  const int iY = int_of_double(pos.y / cellY);
  const int iZ = int_of_double(pos.z / cellZ);
  return cellOfCoord(iX, iY, iZ);
}

double fwrap(double gridWidth, double x) {
  const double r = fmod(x, gridWidth);
  if (r >= 0) {
    return r;
  } else {
    return r + gridWidth;
  }
}

/// reads areaX + areaY + areaZ -> cell double
vect wrapArea(vect pos) {
  const double x = fwrap(areaX, pos.x);
  const double y = fwrap(areaY, pos.y);
  const double z = fwrap(areaZ, pos.z);
  return (vect) { x,y,z };
}

// relativePosX(x) computes the distance to the right-top-most
// nearest corner, divided by the width of a cell
/// reads cellX -> cell double
inline double relativePosX(double x) {
  const int iX = int_of_double(x / cellX);
  return (x - iX * cellX) / cellX;
}
/// reads cellY -> cell double
inline double relativePosY(double y) {
  const int iY = int_of_double(y / cellY);
  return (y - iY * cellY) / cellY;
}
/// reads cellZ -> cell double
inline double relativePosZ(double z) {
  const int iZ = int_of_double(z / cellZ);
  return (z -  iZ * cellZ) / cellZ;
}

typedef struct {
  int iX;
  int iY;
  int iZ;
} coord;

/// reads gridZ + gridY -> cell int
inline coord coordOfCell(int idCell) {
  const int iZ = idCell % gridZ;
  const int iXY = idCell / gridZ;
  const int iY = iXY % gridY;
  const int iX = iXY / gridY;
  return (coord) { iX, iY, iZ };
}

typedef struct {
  int v[8];
} int_nbCorners;

typedef struct {
  double v[8];
} double_nbCorners;

typedef struct {
  vect v[8];
} vect_nbCorners;

/// reads gridX + gridY + gridZ -> cell int
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

/// reads gridX + gridY + gridZ -> cell int
///       (nbCorners -> int)
///       field -> array vect
vect_nbCorners getFieldAtCorners(int idCell, vect* field) {
  const int_nbCorners indices = indicesOfCorners(idCell);
  vect_nbCorners res;
  for (int idCorner = 0; idCorner < nbCorners; idCorner++) {
    res.v[idCorner] = field[indices.v[idCorner]];
  }
  return res;
}

// Total charge of the particles already placed in the cell for the next time step
// charge are also accumulated in the corners of the cells
/// reads
///   (nbCorners -> int)
///   gridX + gridY + gridZ -> cell int
/// modifies deposit -> array double
void accumulateChargeAtCorners(double* deposit, int idCell, double_nbCorners charges) {
  const int_nbCorners indices = indicesOfCorners(idCell);
  for (int idCorner = 0; idCorner < nbCorners; idCorner++) {
    deposit[indices.v[idCorner]] += charges.v[idCorner];
  }
}

// given the relative position inside a cell, with coordinates in the range [0,1],
// compute the coefficient for interpolation at each corner;
// the value for one corner is proportional to the volume between the particle
// and the opposite corner.
/// reads cellX + cellY + cellZ -> cell double
double_nbCorners cornerInterpolationCoeff(vect pos) {
  const double rX = relativePosX(pos.x);
  const double rY = relativePosY(pos.y);
  const double rZ = relativePosZ(pos.z);

  const double cX = 1. - rX;
  const double cY = 1. - rY;
  const double cZ = 1. - rZ;

  // const double cX = 1. + -1. * rX;
  // const double cY = 1. + -1. * rY;
  // const double cZ = 1. + -1. * rZ;
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

/// reads (nbCorners -> int)
vect matrix_vect_mul(const double_nbCorners coeffs, const vect_nbCorners matrix) {
  vect res = { 0., 0., 0. };
  for (int idCorner = 0; idCorner < nbCorners; idCorner++) {
    res = vect_add(res, vect_mul(coeffs.v[idCorner], matrix.v[idCorner]));
  }
  return res;
}

// --------- Poisson solver

void computeRhoFromDeposit() { // reads [double* deposit], writes [double*** rho]
  // Each unit of particle in the deposit increases the charge density by 'factor'
  const double factor = averageChargeDensity * nbCells / nbParticles; // = particleCharge / cellVolume;
  for (int i = 0; i < gridX; i++) {
    for (int j = 0; j < gridY; j++) {
      for (int k = 0; k < gridZ; k++) {
        rho[i][j][k] = factor * deposit[cellOfCoord(i,j,k)];
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
  allocateStructuresForPoissonSolver();

  // Allocate array for deposit
  deposit = (double*) malloc(nbCells * sizeof(double));

  // Allocate the field, not initialized in this function
  field = (vect*) malloc(nbCells * sizeof(vect));

  // Allocate bagsNext and bagsCur with empty bags in every cell
  bagsCur = (bag*) malloc(nbCells * sizeof(bag));
  bagsNext = (bag*) malloc(nbCells * sizeof(bag));
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_init(&bagsCur[idCell]);
    bag_init(&bagsNext[idCell]);
  }
}

void deallocateStructures() {
  deallocateStructuresForPoissonSolver();

  // Free the chunks
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_free(&bagsCur[idCell]);
    bag_free(&bagsNext[idCell]);
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
  vect pos = { x, y, z };
  vect speed = { vx, vy, vz };
  particle p = { pos, speed, CHECKER_ONLY(idParticle) };

  // Store the particle in the bag of the cell that contains the particle
  const int idCell = idCellOfPos(pos);
  bag_push(&bagsCur[idCell], p);

  // Deposit the charge of the particle at the corners of the target cell
  double_nbCorners contribs = cornerInterpolationCoeff(pos);
  accumulateChargeAtCorners(deposit, idCell, contribs);
}

// --------- Steps

// The Leaf-Frog method is used to obtain an order-2 interpolation for the
// differential equation; it consists of precomputing, for half-of-step backwards,
// the evolution of the particles speed.
void stepLeapFrog() {
  // Poisson solver to compute field at time zero, and reset deposit
  updateFieldUsingDeposit();
  // A leap-frog is half a step backwards
  const double negHalfStepDuration = -0.5 * stepDuration;
  // For each cell from the grid
  for (int idCell = 0; idCell < nbCells; idCell++) {
    // Consider the bag of particles in that cell
    bag* b = &bagsCur[idCell];
    // Compute fields at corners of the cell
    vect_nbCorners field_at_corners = getFieldAtCorners(idCell, field);
    // For each particle in that cell
    bag_iter bag_it;
    for (particle* p = bag_iter_begin(&bag_it, b); p != NULL; p = bag_iter_next(&bag_it)) {
      double_nbCorners coeffs = cornerInterpolationCoeff(p->pos);
      vect fieldAtPos = matrix_vect_mul(coeffs, field_at_corners);
      vect accel = vect_mul(particleCharge / particleMass, fieldAtPos);
      p->speed = vect_add(p->speed, vect_mul(negHalfStepDuration, accel));
    }
  }
}

/// reads
///   gridX + gridY + gridZ -> cell int
///   cellX + cellY + cellZ -> cell double
///   areaX + areaY + areaZ -> cell double
///   nbCells -> cell int
///   particleCharge + particleMass + stepDuration -> cell double
///   field -> array vect
/// modifies
///   bagsCur -> [cell] array bag
///   bagsNext -> [cell] array bag
///   deposit -> [cell] array double
void step() {
  // For each cell from the grid
  for (int idCell = 0; idCell < nbCells; idCell++) {

    // Read the electric field that applies to the corners of the cell considered
    vect_nbCorners field_at_corners = getFieldAtCorners(idCell, field);
    /// field_at_corners -> cell vect_nbCorners

    // Consider the bag of particles in that cell
    bag* b = &bagsCur[idCell];
    /// b -> cell array bag + consume bagsCur 
    bag_iter bag_it;
    /// bag_it -> cell bag_iter
    for (particle* p = bag_iter_destructive_begin(&bag_it, b);
         p != NULL;
         p = bag_iter_next(&bag_it)) {
      /// p -> cell cell particle

      // Interpolate the field based on the position relative to the corners of the cell
      double_nbCorners coeffs = cornerInterpolationCoeff(p->pos);
      /// coeffs -> cell double_nbCorners
      vect fieldAtPos = matrix_vect_mul(coeffs, field_at_corners);
      /// fieldAtPos -> cell vect

      // Compute the acceleration: F = m*a and F = q*E  gives a = q/m*E
      vect accel = vect_mul(particleCharge / particleMass, fieldAtPos);
      /// accel -> cell vect

      // Compute the new speed and position for the particle.
      vect speed2 = vect_add(p->speed, vect_mul(stepDuration, accel));
      /// speed2 -> cell vect
      vect pos2 = vect_add(p->pos, vect_mul(stepDuration, speed2));
      /// pos2 -> cell vect
      pos2 = wrapArea(pos2);
      particle p2 = { pos2, speed2, CHECKER_ONLY(p->id) };
      /// p2 -> cell particle

      // Compute the location of the cell that now contains the particle
      const int idCell2 = idCellOfPos(pos2);
      /// idCell2 -> int

      // Push the updated particle into the bag associated with its target cell
      bag_push(&bagsNext[idCell2], p2); /// modifies bagsNext

      // Deposit the charge of the particle at the corners of the target cell
      double_nbCorners contribs = cornerInterpolationCoeff(pos2);
      /// contribs -> cell double_nbCorners
      accumulateChargeAtCorners(deposit, idCell2, contribs); /// modifies deposit
    }
    bag_init(b); /// modifies b
  }

  /// consume b to recover bagsCur
  // For the next time step, the contents of bagNext is moved into bagCur (which is empty)
  /// modifies barsCur and bagsNext
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_swap(&bagsCur[idCell], &bagsNext[idCell]);
  }

  // Update the new field based on the deposit array, then reset this array
  updateFieldUsingDeposit(); // TODO: stopped here
}

#ifdef CHECKER
void reportParticlesState() {
  // printf("NbParticles: %d\n", nbParticles);
  FILE* f = fopen("particles.res", "wb");
  fwrite(&nbParticles, sizeof(int), 1, f);
  fwrite(&areaX, sizeof(double), 1, f);
  fwrite(&areaY, sizeof(double), 1, f);
  fwrite(&areaZ, sizeof(double), 1, f);
  int count = 0;
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag* b = &bagsCur[idCell];
    bag_iter bag_it;
    for (particle* p = bag_iter_begin(&bag_it, b); p != NULL; p = bag_iter_next(&bag_it)) {
      count++;
      int id = p->id;
      double posX = p->pos.x;
      double posY = p->pos.y;
      double posZ = p->pos.z;
      double speedX = p->speed.x;
      double speedY = p->speed.y;
      double speedZ = p->speed.z;
      fwrite(&id, sizeof(int), 1, f);
      fwrite(&posX, sizeof(double), 1, f);
      fwrite(&posY, sizeof(double), 1, f);
      fwrite(&posZ, sizeof(double), 1, f);
      fwrite(&speedX, sizeof(double), 1, f);
      fwrite(&speedY, sizeof(double), 1, f);
      fwrite(&speedZ, sizeof(double), 1, f);
    }
  }
  if (count != nbParticles) {
    printf("ERROR: reportParticlesState: particles were lost, remaining %d\n", count);
  }
  fclose(f);
}
#endif

// --------- Main

  int main(int argc, char** argv) {

  loadParameters(argc, argv);

  computeConstants();

  allocateStructures();

  resetDeposit();

  createParticles();

#ifndef SKIPLEAPFROG
  stepLeapFrog();
#endif

#if defined(PRINTPERF) || defined(PRINTSTEPS)
  double timeStart = omp_get_wtime();
#endif
#ifdef PRINTSTEPS
  double nextReport = timeStart + 1.0;
#endif

// Foreach time step
  for (int idStep = 0; idStep < nbSteps; idStep++) {
#ifdef PRINTSTEPS
    // Report on progress during the execution
    if (omp_get_wtime() > nextReport) {
      nextReport += 1.0;
      printf("Step %d\n", idStep);
    }
#endif
    step();
  }

#ifdef PRINTPERF
  double timeTotal = (double) (omp_get_wtime() - timeStart);
  printf("Exectime: %.3f sec\n", timeTotal);
  printf("ParticlesMoved: %.1f billion particles\n", ((double) nbParticles * nbSteps) / 1000 / 1000 / 1000);
  printf("Throughput: %.1f million particles/sec\n", ((double) nbParticles * nbSteps) / timeTotal / 1000 / 1000);
#endif

#ifdef CHECKER
  reportParticlesState();
#endif

  deallocateStructures();

  free(deposit);
}

