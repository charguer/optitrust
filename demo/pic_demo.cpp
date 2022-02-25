#include "optitrust.h"

// --------- Bags of particles

#include "particle_chunk.h"
#include "particle_chunk_alloc.h"

bag* CHOOSE (int nb, bag* b1, bag* b2) {return b1;}

// --------- Parameters

//  physical parameter of the simulation
const double areaX = 10.0;
const double areaY = 10.0;
const double areaZ = 10.0;

const double stepDuration = 0.2;
const double particleCharge = 10.0;
const double particleMass = 5.0;

// Grid description
const int gridX = 64;
const int gridY = 64;
const int gridZ = 64;
const int nbCells = gridX * gridY * gridZ;

// Derived grid parameters
const double cellX = areaX / gridX;
const double cellY = areaY / gridY;
const double cellZ = areaZ / gridZ;

// duration of the simulation
const int nbSteps = 100;

// --------- Grid coordinate functions

// from double to int
int int_of_double(double a) {
  return (int) a - (a < 0.);
}

int wrap(int gridSize, int a) {
  return (a % gridSize + gridSize) % gridSize;
}

// --------- Grid Representation

const int nbCorners = 8;

vect* fields = (vect*) malloc(nbCells * sizeof(vect));


int cellOfCoord(int i, int j, int k) {
  return MINDEX3(gridX,gridY,gridZ,i,j,k);
}

// idCellOfPos computes the id of the cell that contains a position.
int idCellOfPos(vect pos) {
  int iX = int_of_double(pos.x / cellX);
  int iY = int_of_double(pos.y / cellY);
  int iZ = int_of_double(pos.z / cellZ);
  return cellOfCoord(iX, iY, iZ);
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
  return { iX, iY, iZ };
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
  return {
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

void accumulateChargeAtCorners(double* nextCharge, int idCell, double_nbCorners charges) {
  const int_nbCorners indices = indicesOfCorners(idCell);
  for (int k = 0; k < nbCorners; k++) {
    nextCharge[indices.v[k]] += charges.v[k];
  }
}

// --------- Interpolation operations

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

double_nbCorners vect8_mul(const double a, const double_nbCorners data) {
  double_nbCorners res;
  for (int k = 0; k < nbCorners; k++) {
    res.v[k] = a * data.v[k];
  }
  return res;
}

// --------- LEFT to implement

void init(bag* bagsCur, bag* bagsNext, vect* field);

// updateFieldsUsingNextCharge in an operation that reads nextCharge,
// resets it to zero, and updates the values in the fields array.
void updateFieldUsingNextCharge(double* nextCharge, vect* field) { }

// --------- Module Simulation

int main() {

  // Particles in each cell, at the current and the next time step
  bag* bagsCur = (bag*) malloc(nbCells * sizeof(bag));
  bag* bagsNext = (bag*) malloc(nbCells * sizeof(bag));

  // nextCharge[idCell] corresponds to the cell in the front-top-left corner of that cell
  double* nextCharge = (double*) malloc(nbCells * sizeof(double));

  // Strength of the field that applies to each cell
  // fields[idCell] corresponds to the field at the top-right corner of the cell idCell;
  // The grid is treated with wrap-around
  vect* field = (vect*) malloc(nbCells * sizeof(vect));

  init(bagsCur, bagsNext, field);

  // Foreach time step
  for (int step = 0; step < nbSteps; step++) {

    // Update the new field based on the total charge accumulated in each cell
    updateFieldUsingNextCharge(nextCharge, field);

    // reset the array of next charges
    for (int idCell = 0; idCell < nbCells; idCell++) {
      nextCharge[idCell] = 0.;
    }

    // For each cell from the grid
    for (int idCell = 0; idCell < nbCells; idCell++) {

      // Read the electric field that applies to the corners of the cell considered
      vect_nbCorners field_at_corners = getFieldAtCorners(idCell,field);

      // Consider the bag of particles in that cell
      bag* b = &bagsCur[idCell];

      bag_iter bag_it;
      for (particle* p = bag_iter_begin(&bag_it, b); p != NULL; p = bag_iter_next(&bag_it, true)) {

        // Interpolate the field based on the position relative to the corners of the cell
        double_nbCorners coeffs = cornerInterpolationCoeff(p->pos);
        vect fieldAtPos = matrix_vect_mul(coeffs, field_at_corners);

        // Compute the acceleration: F = m*a and F = q*E  gives a = q/m*E
        vect accel = vect_mul(particleCharge / particleMass, fieldAtPos);

        // Compute the new speed and position for the particle.
        vect speed2 = vect_add(p->speed, vect_mul(stepDuration, accel));
        vect pos2 = vect_add(p->pos, vect_mul(stepDuration, speed2));
        particle p2 = { pos2, speed2 };

        // Compute the location of the cell that now contains the particle
        int idCell2 = idCellOfPos(pos2);

        // Push the updated particle into the bag associated with its target cell
        bag_push(&bagsNext[idCell2], p2);

        // Deposit the charge of the particle at the corners of the target cell
        double_nbCorners coeffs2 = cornerInterpolationCoeff(pos2);
        double_nbCorners deltaChargeOnCorners = vect8_mul(particleCharge, coeffs2);
        accumulateChargeAtCorners(nextCharge, idCell2, deltaChargeOnCorners);
      }
      bag_init_initial(b);
    }

    // For the next time step, the contents of bagNext is moved into bagCur (which is empty)
    for (int idCell = 0; idCell < nbCells; idCell++) {
      bag_swap(&bagsCur[idCell], &bagsNext[idCell]);
    }

  }
}
