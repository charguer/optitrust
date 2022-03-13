#include <stdlib.h>

// --------- Parameters

// Time steps description
const int nbSteps = 100;
const double step_duration = 0.2;

// Grid description
const int gridSize = 64;
const int nbCells = gridSize * gridSize * gridSize;

// Maximum number of particles per cell
const int bagCapacity = 100;

const double charge = 1.0;

//  physical parameter of the simulation
const double cellSize = 0.001;

// size of the blocks used in loop tiling
const int blocksize = 2;

// from double to int
int int_of_double (double x) {
  return (int) x - (x < 0.);
}

// coordinate round up
int index_of_double (double x) {
  return (int) (x / cellSize); 
}

// translated at the end of our script into "omp atomic"
int fetch_and_add (int * p, int n); 



// --------- Vector

typedef struct {
  double x, y, z;
} vect;


vect vect_add(vect v1, vect v2) {
  return { v1.x + v2.x, v1.y + v2.y, v1.z + v2.z };
}

vect vect_mul(double d, vect v) {
  return { d * v.x, d * v.y, d * v.z };
}

// --------- Particle

typedef struct {
  vect pos;
  vect speed;
} particle;

// --------- Bags of particles

typedef struct {
  int nb; // 0 <= nb <= bagCapacity
  particle items[bagCapacity];
} bag;

void bag_push(bag& b, particle p) {
  // assert(b.nb < bagCapacity);
  b.items[b.nb] = p;
  b.nb++;
}

void bag_push_atomic (bag &b, particle p) {
  int fa = fetch_and_add (&b.nb, 1);
  b.items[fa] = p;
}

bag* bag_create() {
  return (bag*)malloc(nbCells * sizeof(bag));
}


void delete_bag (bag * b) {
  free(b);
}

void initParticles (bag* b);

void bag_clear(bag& b) {
  b.nb = 0;
}

void bag_transfer(bag& b1, bag& b2) { // transfer from b2 into b1
  for (int i = 0; i < b2.nb; i++) {
    bag_push(b1, b2.items[i]);
  }
  bag_clear(b2);
}
// chose function
bag* CHOOSE (int nb, bag* b1, bag* b2) {return b1;}

// --------- Grid Representation

// Particles in each cell, at the current and the next time step
bag bagsCur[nbCells];
bag bagsNext[nbCells];

// Strength of the field that applies to each cell
vect fields[nbCells];

// Total charge of the particles already placed in the cell for the next time step
double nextCharge[nbCells];

// updateFieldsUsingNextCharge in an operation that reads nextCharge,
// resets it to zero, and updates the values in the fields array.
void updateFieldsUsingNextCharge();

// idCellOfPos computes the id of the cell that contains a position.
int idCellOfPos(vect pos) {
  int x = index_of_double (pos.x);
  int y = index_of_double (pos.y);
  int z = index_of_double (pos.z);

  return (x * gridSize + y)* gridSize + z;
}

// --------- Module Simulation

int main() {

  // Foreach time step
  for (int step = 0; step < nbSteps; step++) {

    // For each cell from the grid
    for (int idCell = 0; idCell < nbCells; idCell++) {

      // Read the electric field that applies to the cell considered
      vect field = fields[idCell];

      // Foreach particle in the cell considered
      bag& b = bagsCur[idCell];
      int nb = b.nb;
      for (int idParticle = 0; idParticle < nb; idParticle++) {
        // Read the particle in memory
        particle &p = b.items[idParticle];

        // Compute the new speed and position for the particle
        vect speed2 = vect_add(p.speed, vect_mul(charge, field));
        vect pos2 = vect_add(p.pos, vect_mul(step_duration, speed2));

        // Deposit the unit charge of the particle in array "nextCharge"
        int idCell2 = idCellOfPos(pos2);
        nextCharge[idCell2] += 1.0;

        // Write the updated particle in the bag associated with its new cell
        particle p2 = { pos2, speed2 };
        bag_push(bagsNext[idCell2], p2);
      }

      // At the end of the time step, clear the contents of the bag
      bag_clear(bagsCur[idCell]);
    }

    // Update the new field based on the total charge accumulated in each cell
    updateFieldsUsingNextCharge();

    // For the next time step, the contents of bagNext is moved into bagCur
    for (int idCell = 0; idCell < nbCells; idCell++) {
      bag_transfer(bagsCur[idCell], bagsNext[idCell]);
    }

  }
}

