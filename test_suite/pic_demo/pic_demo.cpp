
// Particle description
const double charge = 1.0;

// Grid description
const int gridSize = 64;
const int nbCells = gridSize * gridSize * gridSize;
// Currently assuming a maximum number of particles per cell (LATER: linked lists of fixed-sized bags)
const int bagCapacity = 100;

// Simulation parameters
const double step_duration = 0.2;
const int nbSteps = 100;

// --------- Module Vector 

typedef struct {
  double x, y, z;
} vect;


vect vect_add(vect v1, vect v2) {
  vect v = { v1.x + v2.x,
             v1.y + v2.y,
             v1.z + v2.z };
  return v;
}

vect vect_mul(double d, vect v) {
  vect v = { d * v.x,
             d * v.y,
             d * v.z };
  return v;
}

// --------- Module Particle
typedef struct {
  vect pos;
  vect speed;
} particle;

// --------- Module Bags of particles

// For simplicity, we assume in this file that a given cell never
// contains more than bagCapacity particles. The real implementation,
// uses a linked list of fixed-capacity bags.

typedef struct {
  int nb; // 0 <= nb <= bagCapacity
  particle items[bagCapacity];
} bag;

void bag_push(bag& b, particle p) {
  // assert(b.nb < bagCapacity);
  b.items[b.nb] = p;
  b.nb++;
}

void bag_clear(bag& b) {
  b.nb = 0;
}

void bag_transfer(bag& b1, bag& b2) {
  // Move all items from b2 into b1
  // Note: in the real code, bags are linked lists,
  // so this operation only involves a pointer assignment,
  // not a deep copy of an array.
  for (int i = 0; i < b2.nb; i++) {
    bag_push(b1, b2.items[i]);
  }
  bag_clear(b2);
}

// --------- Module Grid Representation

// LATER: in the real code, the charge is associated not to each cell,
// but to each corner of a cell in the grid.

// Particles in each cell, at the current and the next time step
bag bagsCur[nbCells];
bag bagsNext[nbCells];

// Strength of the field that applies to each cell
vect fields[nbCells];

// Total charge of the particles already placed in the cell for the next time step
double nextCharge[nbCells];

// --------- Auxiliary functions

// updateFieldsUsingNextCharge in an operation that reads nextCharge,
// resets it to zero, and updates the values in the fields array.
void updateFieldsUsingNextCharge();

// idCellOfPos computes the id of the cell that contains a position.
int idCellOfPos(vect pos);

// --------- Module Simulation

int main() {

  // Foreach time step
  for (int step = 0; step < nbSteps; step++) {

    // For each cell from the grid
    for (int idCell = 0; idCell < nbCells; idCell++) {

      // Read the electric field that applies to the cell considered
      vect field = fields[idCell];

      // Foreach particle in that cell
      bag& b = bagsCur[idCell];
      int nb = b.nb;
      for (int idParticle = 0; idParticle < nb; idParticle++) {
        // get the particle
        particle& p = b.items[idParticle];

        // compute speed and position
        vect speed2 = vect_add(p.speed, vect_mul(charge, field));
        vect pos2 = vect_add(p.pos, vect_mul(step_duration, speed2));

        // deposit particle charge
        int idCell2 = idCellOfPos(pos2);
        nextCharge[idCell2] += charge;

        // write particle into target cell
        particle p2 = { speed2, pos2 };
        bag_push(bagsNext[idCell2], p2);
      }

      // empty the source cell
      bag_clear(bagsCur[idCell]);
    }

    // update charge field
    updateFieldsUsingNextCharge();

    // assign next to current
    for (int idCell = 0; idCell < nbCells; idCell++) {
      bag_transfer(bagsCur[idCell], bagsNext[idCell]);
    }

  }
}



/*

for (int idCell...)

<= flattening of the nest loops into one loop
=> tiling

  for( int x = 0; x < gridX; x++) {
      for (int y = 0; y < gridY; y++) {
        for (int z = 0; z < gridZ; z++) {}
           int idCell = x * gridY * gridZ + y * gridY + z;
        }
      }
    }
  */