#include <stdlib.h>

// --------- Bags of particles

// Import the chunked sequence data structure, specialized to particles
// In OptiTrust, we want to actually inline that code.

// implicitly includes particle.h
#include "particle_chunk.h"
#include "particle_chunk_alloc.h"
#include "optitrust.h"

bag* CHOOSE (int nb, bag* b1, bag* b2) {return b1;}

// --------- Parameters

// This code does not assume the cell size to be normalized,
// not the charge to be normalized; the normalization will
// be implemented in the transformations

//  physical parameter of the simulation
const double areaX = 10.0;
const double areaY = 10.0;
const double areaZ = 10.0;

const double stepDuration = 0.2;
const double particleCharge = 10.0;
const double particleMass = 5.0;

// Grid description
const int gridSize  = 64;
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

int wrapX(int gridSize, int a) {
  return (a % gridSize + gridSize) % gridSize;
}

/* Other possible implementations for wrap
  // assuming that a particle does not traverse the grid more than once in a timestep
   return (x % gridSize + gridSize) % gridSize
  // use of fmod possible
  // version without modulo but using if statements
    if (x < 0)
       return x + gridSize;
    if (x >= gridSize)
       return x - gridSize;
    return x;
*/

// --------- Grid Representation

const int nbCorners = 8;

vect* fields = (vect*) malloc(nbCells * sizeof(vect));


int cellOfCoord(int i, int j, int k) {
  return MINDEX3(gridSize,gridSize,gridSize,i,j,k);
}

// idCellOfPos computes the id of the cell that contains a position.
int idCellOfPos(vect pos) {
  int ix = int_of_double(pos.x / cellX);
  int iy = int_of_double(pos.y / cellY);
  int iz = int_of_double(pos.z / cellZ);
  return cellOfCoord(ix, iy, iz);
}

double relativePosX(double x) {
  int ix = int_of_double(x / cellX);
  return (x - ix * cellX) / cellX;
}
double relativePosY(double y) {
  int iy = int_of_double(y / cellY);
  return (y - iy * cellY) / cellY;
}
double relativePosZ(double z) {
  int iz = int_of_double(z / cellZ);
  return (z -  iz * cellZ) / cellZ;
}
/* DEPRECATED
// coord array of size 3
void compute_coordOfCell (int idCell, int* coord_arr) {
  int z = idCell % gridSize;
  int xy = idCell / gridSize;
  int y = xy % gridSize;
  int x = xy / gridSize;
  coord_arr[0] = x;
  coord_arr[1] = y;
  coord_arr[2] = z;
}*/
typedef struct {
  int ix;
  int iy;
  int iz;
} coord;

coord coordOfCell(int idCell) {
  int iz = idCell % gridZ;
  int ixy = idCell / gridZ;
  int iy = ixy % gridY;
  int ix = ixy / gridY;
  return { ix, iy, iz };
}

typedef struct {
  int values [nbCorners];
} int_nbCorners;

typedef struct {
  double values[nbCorners];
} double_nbCorners;

typedef struct {
  vect values[nbCorners];
} vect_nbCorners;

int_nbCorners indicesOfCorners (int idCell) {
  coord coord = coordOfCell (idCell);
  int x = coord.ix;
  int y = coord.iy;
  int z = coord.iz;
  int x2 = wrapX(gridSize, x+1);
  int y2 = wrapX(gridSize, y+1);
  int z2 = wrapX(gridSize, z+1);
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

vect_nbCorners getFieldAtCorners(int idCell) {
  int_nbCorners indices = indicesOfCorners(idCell);
  vect_nbCorners result;
  for (int k = 0; k < nbCorners; k++) {
    result.values[k] = fields[indices.values[k]];
  }
  return result;

}

// Total charge of the particles already placed in the cell for the next time step
// charge are also accumulated in the corners of the cells

void accumulateChargeAtCorners(double* nextCharge, int idCell, double_nbCorners charges) {
  int_nbCorners indices = indicesOfCorners(idCell);
  for(int k = 0; k < nbCorners; k++){
    nextCharge[indices.values[k]] += charges.values[k];
  }
}

// --------- Interpolation operations

// given the relative position inside a cell, with coordinates in the range [0,1],
// compute the coefficient for interpolation at each corner;
// the value for one corner is proportional to the volume between the particle
// and the opposite corner.


double_nbCorners cornerInterpolationCoeff(vect pos) {
  double rx = relativePosX(pos.x);
  double ry = relativePosY(pos.y);
  double rz = relativePosZ(pos.z);
  double cx = 1. - rx;
  double cy = 1. - ry;
  double cz = 1. - rz;
  return { {
    cx * cy * cz,
    cx * cy * rz,
    cx * ry * cz,
    cx * ry * rz,
    rx * cy * cz,
    rx * cy * rz,
    rx * ry * cz,
    rx * ry * rz,
  } };
}

vect vect_matrix_mul(const double_nbCorners coeffs, const vect_nbCorners matrix) {
  vect result = { 0., 0., 0. };
  for (int k = 0; k < nbCorners; k++) {
    result = vect_add(result, vect_mul(coeffs.values[k], matrix.values[k]));
  }
  return result;
}

double_nbCorners vect8_mul(const double a, const double_nbCorners v) {
  double_nbCorners result;
  for (int k = 0; k < nbCorners; k++) {
    result.values[k] = a * v.values[k];
  }
  return result;
}


// --------- LEFT to implement

void init(bag* bagsCur, bag* bagsNext, vect* field) {
  // example push of one particle in cell zero, just to see the effect of scaling/shifting
  // of speed and positions
  /*double posX = 1.0, posY = 1.0, posZ = 1.0; // arbitrary values
  double speedX = 1.0, speedY = 1.0, speedZ = 1.0; // arbitrary values
  const vect pos = { posX, posY, posZ };
  const vect speed = { speedX, speedY, speedZ };
  const particle p0 = { pos, speed };
  bag_push(&bagsCur[0], p0);
  */
}

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
      vect_nbCorners field_at_corners = getFieldAtCorners(idCell);

      // Consider the bag of particles in that cell
      bag* b = &bagsCur[idCell];

      // Perform a destructive iteration on that bag,
      // meaning that chunks are freed after traversal.
      chunk* c = b->front;
      while (true) { // loop on chunks
        int nb = c->size;
        // iterate over the items from the current chunk
        for (int i = 0; i < nb; i++) {
          /* TODO: re-printing the code below gives: "particle &p = cur_p;"
          particle* cur_p = &c->items[i];
          particle &p = *cur_p; */
          particle p = c->items[i];


          // Interpolate the field based on the position relative to the corners of the cell
          const double_nbCorners coeffs = cornerInterpolationCoeff(p.pos);
          const vect fieldAtPos = vect_matrix_mul(coeffs, field_at_corners);

          // Compute the acceleration: F = m*a and F = q*E  gives a = q/m*E
          const vect accel = vect_mul(particleCharge / particleMass, fieldAtPos);

          // Compute the new speed and position for the particle.
          const vect speed2 = vect_add(p.speed, vect_mul(stepDuration, accel));
          const vect pos2 = vect_add(p.pos, vect_mul(stepDuration, speed2));

          // Compute the location of the cell that now contains the particle
          const int idCell2 = idCellOfPos(pos2);

          // Push the updated particle into the bag associated with its target cell
          const particle p2 = { pos2, speed2 };
          bag_push(&bagsNext[idCell2], p2);

          // Deposit the charge of the particle at the corners of the target cell
          const double_nbCorners coeffs2 = cornerInterpolationCoeff(pos2);
          double_nbCorners deltaChargeOnCorners = vect8_mul(particleCharge, coeffs2);
          accumulateChargeAtCorners(nextCharge, idCell2, deltaChargeOnCorners);
        } // end of loop on chunk items

        // moving on to the next chunk LATER: ARTHUR: move this into an auxiliary function.
        //----
        chunk* cnext = c->next;
        if (cnext != NULL) {
          // move to the next chunk, free the current chunk
          chunk_free(c);
          c = cnext; // beware that "c = c->next" would be illegal here, because c was freed
        } else {
          // finished the last chunk, clear the current chunk, clear the bag
          c->size = 0;
          b->front = c;
          b->back = c; // this write is redundant, but let's do it for clarity
          c->next = NULL; // this write is redundant, but let's do it for clarity
          break; // exit the loop on chunks
        }
        //----
      }
    }

    // For the next time step, the contents of bagNext is moved into bagCur (which is empty)
    for (int idCell = 0; idCell < nbCells; idCell++) {
      bag_swap(&bagsCur[idCell], &bagsNext[idCell]);
    }

  }
}


// LATER: When ClangML supports it, we'll use overloaded + and * operators on class vect
// LATER: When ClangML supports it, we'll use higher-order iteration with a local function
// LATER: When ClangML supports it, we'll use boost arrays for fixed size arrays
