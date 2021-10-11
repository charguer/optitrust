#include <stdlib.h>

// --------- Bags of particles

// Import the chunked sequence data structure, specialized to particles
// In OptiTrust, we want to actually inline that code.

// implicitly includes particle.h
#include "particle_bag.cpp"


// --------- OptiTrust macros // temporary

bag* CHOOSE (int nb, bag* b1, bag* b2) {return b1;}


// --------- Parameters

// This code does not assume the cell size to be normalized,
// not the charge to be normalized; the normalization will
// be implemented in the transformations

// Time steps description
const int nbSteps = 100;
const double step_duration = 0.2;

// Grid description
const int gridSize = 64;
const int nbCells = gridSize * gridSize * gridSize;

// Maximum number of particles per cell
const int bagCapacity = 100;

const double charge = 10.0;

//  physical parameter of the simulation
const double cellSize = 0.001;

// size of the blocks used in loop tiling
const int blocksize = 2;




vect vect_add(vect v1, vect v2) {
  return { v1.x + v2.x, v1.y + v2.y, v1.z + v2.z };
}

vect vect_mul(double d, vect v) {
  return { d * v.x, d * v.y, d * v.z };
}


// --------- Grid coordinate functions

// from double to int
int int_of_double(double x) {
  return (int) x - (x < 0.);
}

// coordinate rounding
int index_of_double(double x) {
  return int_of_double(x / cellSize);
}

int wrap(int x) { // could be likewise on other dimensions
  // assuming that a particle does not traverse the grid more than once in a timestep
  return (x + gridSize) % gridSize;
  /*
  // version without modulo
  if (x < 0)
     return x + gridSize;
  if (x >= gridSize)
     return x - gridSize;
  return x;
  */
}

// --------- Grid Representation

const int nbCorners = 8;

// Strength of the field that applies to each cell
// fields[idCell] corresponds to the field at the top-right corner of the cell idCell;
// The grid is treated with wrap-around
vect fields[nbCells];

 // TODO: could use i,j,k for cell coordinates, to avoid confusion with x,y,z the absolute coordinates?

int cellOfCoord(int x, int y, int z) {
  return (x * gridSize + y)* gridSize + z;
}

// idCellOfPos computes the id of the cell that contains a position.
int idCellOfPos(vect pos) {
  int x = index_of_double(pos.x);
  int y = index_of_double(pos.y);
  int z = index_of_double(pos.z);
  return cellOfCoord(x, y, z);
}

double relativePosInCell(double x) { // likewise for all dimensions
  int i = index_of_double(x);
  return (x - (double) i) / gridSize;
}

// coord array of size 3
void compute_coordOfCell (int idCell, int* coord_arr) {
  int z = idCell % gridSize;
  int xy = idCell / gridSize;
  int y = xy % gridSize;
  int x = xy / gridSize;
  coord_arr[0] = x;
  coord_arr[1] = y;
  coord_arr[1] = z;
}

// indices array of size 8
// later compute
void compute_indicesOfCorners (int idCell, int* indices) {
  int coord[3];
  compute_coordOfCell(idCell, coord);
  int x = coord[0];
  int y = coord[1];
  int z = coord[2];
  int x2 = wrap(x+1);
  int y2 = wrap(y+1);
  int z2 = wrap(z+1);
  int indices[8];
  indices[0] = cellOfCoord(x,y,z);
  indices[1] = cellOfCoord(x,y,z2);
  indices[2] = cellOfCoord(x,y2,z);
  indices[3] = cellOfCoord(x,y2,z2);
  indices[4] = cellOfCoord(x2,y,z);
  indices[5] = cellOfCoord(x2,y,z2);
  indices[6] = cellOfCoord(x2,y2,z);
  indices[7] = cellOfCoord(x2,y2,z2);
}
vect[nbCorners] getFieldAtCorners(idCell) {


  int indices[nbCorners];
  compute_indicesOfCorners(idCell, &indices, nbCorners);
  vect result[nbCorners];
  for (int k = 0; k < nbCorners; k++) {
    result[k] = fields[indices[k]];
  }
  return result;
}

void computer_FieldAtCorners(idCell) {


  int indices[nbCorners];
  compute_indicesOfCorners(idCell, &indices, nbCorners);
  vect result[nbCorners];
  for (int k = 0; k < nbCorners; k++) {
    result[k] = fields[indices[k]];
  }
  return result;
}

// Total charge of the particles already placed in the cell for the next time step
// charge are also accumulated in the corners of the cells
double nextCharge[nbCells];

// charges an array of type [nbCorners]
void accumulateChargeAtCorners(idCell, double* charges) {
  int indices[nbCorners];
  compute_indicesOfCorners(idCell, &indices);
  for (int k = 0; k < nbCorners; k++) {
    nextCharge[indices[k]] += charges[k];
  }
}


// updateFieldsUsingNextCharge in an operation that reads nextCharge,
// resets it to zero, and updates the values in the fields array.
void updateFieldsUsingNextCharge();

// --------- Interpolation operations

// given the relative position inside a cell, with coordinates in the range [0,1],
// compute the coefficient for interpolation at each corner;
// the value for one corner is proportional to the volume between the particle
// and the opposite corner.
double[nbCorners] cornerInterpolationCoeff(vect pos) {
  double rx = relativePosInCell(pos.x);
  double ry = relativePosInCell(pos.y);
  double rz = relativePosInCell(pos.z);
  double cx = 1. - rx;
  double cy = 1. - ry;
  double cz = 1. - rz;
  return {
    cx * cy * cz,
    cx * cy * rz,
    cx * ry * cz,
    cx * ry * rz,
    rx * cy * cz,
    rx * cy * rz,
    rx * ry * cz,
    rx * ry * rz,
  };
}

vect vect_matrix_mul(const double coeffs[nbCorners], const vect matrix[nbCorners]) {
  vect result = { 0., 0., 0. };
  for (int k = 0; k < nbCorners; k++) {
    result = vect_add(result, vect_mul(coeffs[k], matrix[k]));
  }
  return result;
}

double[nbCorners] vect8_mul(const double a, const double v[nbCorners]) {
  vect result;
  for (int k = 0; k < nbCorners; k++) {
    result[k] = a * v[k];
  }
  return result;
}



// Particles in each cell, at the current and the next time step
bag bagsCur[nbCells];
bag bagsNext[nbCells];


// --------- Module Simulation

int main() {

  // Foreach time step
  for (int step = 0; step < nbSteps; step++) {

    // For each cell from the grid
    for (int idCell = 0; idCell < nbCells; idCell++) {

      // Read the electric field that applies to the corners of the cell considered
      vect field_at_corners[nbCorners] = getFieldAtCorners(idCell);

      // Consider the bag of particles in that cell
      bag* b = &bagsCur[idCell];

      // Perform a destructive iteration on that bag,
      // meaning that chunks are freed after traversal,
      // and the bag cleared at the end of the traversal.
      // Ideally, we would use a higher-order iteration function,
      // but since ClangML does not support this feature yet,
      // we temporarily encode it using an iterator.
      bag_iterator it = bag_destructive_iterator_create(b);
      for (particle* cur_p = it.begin(); cur_p < it.end(); it++) {
        particle &p = *cur_p;

        // interpolate the field based on the position relative to the corners of the cell
        double coeffs[nbCorners];
        compute_cornerInterpolationCoeff(p.pos, coeffs);
        vect field_at_pos = vect_matrix_mul(coeffs, field_at_corners);

        // Compute the new speed and position for the particle.
        // When ClangML supports it, we'll use overloaded + and * operators on class vect
        const vect speed2 = vect_add(p.speed, vect_mul(charge, field_at_pos));
        const vect pos2 = vect_add(p.pos, vect_mul(step_duration, speed2));

        // Deposit the charge of the particle at the corners of the target cell
        const int idCell2 = idCellOfPos(pos2);
        double_nbCorners coeffs2 = cornerInterpolationCoeff(p.pos);
        double_nbCorners charges = vect8_mul(charge, coeffs2);
        accumulateChargeAtCorners(idCell2, charges);

        // Push the updated particle into the bag associated with its target cell
        const particle p2 = { pos2, speed2 };
        bag_push(bagsNext[idCell2], p2);
      }
    }

    // Update the new field based on the total charge accumulated in each cell
    updateFieldsUsingNextCharge();

    // For the next time step, the contents of bagNext is moved into bagCur (which is empty)
    for (int idCell = 0; idCell < nbCells; idCell++) {
      bag_swap(bagsCur[idCell], bagsNext[idCell]);
    }

  }
}


