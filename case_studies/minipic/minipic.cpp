#include "../../include/optitrust.h"



// minipic is like pic but:
// - without bags as chained lists of chunks,
//   instead using large arrays
// - without concurrent push operations in bags
// - with assumption of move by at most one cell
// - without the charge deposit computation,
//   instead assuming an electric field to be given at each step


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

int block = 2;
int halfBlock = (block / 2);

const int maxPartsPerCell = 1000000;


// =========================================================
// Interpolation formulae

// from double to int
int int_of_double(double a) {
  return (int) a - (a < 0.);
}

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

const int nbCorners = 8;

typedef struct { int v[nbCorners]; } int_nbCorners;

typedef struct { double v[nbCorners]; } double_nbCorners;

typedef struct { vect v[nbCorners]; } vect_nbCorners;

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


// =========================================================
// Coordinates of a cell

typedef struct {
  int iX;
  int iY;
  int iZ;
} coord;

coord coordOfCell(int idCell) {
  const int iZ = (idCell % gridZ);
  const int iXY = (idCell / gridZ);
  const int iY = (iXY % gridY);
  const int iX = (iXY / gridY);
  return {iX, iY, iZ};
}

int cellOfCoord(int i, int j, int k) {
  return MINDEX3(gridX, gridY, gridZ, i, j, k);
}

int idCellOfPos(vect pos) {
  int iX = int_of_double((pos.x / cellX));
  int iY = int_of_double((pos.y / cellY));
  int iZ = int_of_double((pos.z / cellZ));
  return cellOfCoord(iX, iY, iZ);
}

// =========================================================
// Electric field storage

// Electric field that applies at each time step
//   vect field[MINDEX3(idStep, idCorner)]

int wrap(int gridSize, int a) {
  return (((a % gridSize) + gridSize) % gridSize);
}


int_nbCorners indicesOfCorners(int idCell) {
  const coord coord = coordOfCell(idCell);
  const int x = coord.iX;
  const int y = coord.iY;
  const int z = coord.iZ;
  const int x2 = wrap(gridX, (x + 1));
  const int y2 = wrap(gridY, (y + 1));
  const int z2 = wrap(gridZ, (z + 1));
  return {cellOfCoord(x, y, z),   cellOfCoord(x, y, z2),
          cellOfCoord(x, y2, z),  cellOfCoord(x, y2, z2),
          cellOfCoord(x2, y, z),  cellOfCoord(x2, y, z2),
          cellOfCoord(x2, y2, z), cellOfCoord(x2, y2, z2)};
}

vect_nbCorners getFieldAtCorners(int nbSteps, int nbCells, int idStep, int idCell, vect* field) {
  const int_nbCorners indices = indicesOfCorners(idCell);
  vect_nbCorners res;
  for (int k = 0; k < nbCorners; k++) {
    res.v[k] = field[MINDEX2(nbSteps, nbCells, idStep, indices.v[k])];
  }
  return res;
}

// =========================================================
// Particle storage

// Bags for current step stores :
//    bags[MINDEX3(..., idStep%2, idCell, idPart)]
// where
//    0 <= idPart < bagsSize[MINDEX2(..., idStep%2, idCell)] < maxPartsPerCell
//
// Bags for next step, same, with (idStep+1)%2;
//   initially empty


// =========================================================
// Main loop

int simulate(double stepDuration, int nbSteps, int* bagsSize, particle* bags, vect* field) {

  // Foreach time step
  for (int idStep = 0; idStep < nbSteps; idStep++) {

    // For each cell from the grid
    for (int idCell = 0; idCell < nbCells; idCell++) {

      // Read the electric field that applies to the corners of the cell considered
      vect_nbCorners fieldAtCorners = getFieldAtCorners(nbSteps, nbCells, idStep, idCell, field);

      int nbParts = bagsSize[MINDEX2(2, nbCells, idStep%2, idCell)];
      for (int idPart = 0; idPart < nbParts; idPart++) {
        particle* p = &bags[MINDEX3(2, nbCells, maxPartsPerCell, idStep%2, idCell, idPart)];

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
        int idPartNext = bagsSize[MINDEX2(2, nbCells, (idStep+1)%2, idCellNext)]++;
        bags[MINDEX3(2, nbCells, maxPartsPerCell, (idStep+1)%2, idCellNext, idPartNext)] = p2;
      }
      bagsSize[MINDEX2(2, nbCells, idStep%2, idCell)] = 0;
    }

  }
}
