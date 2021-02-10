#include <iostream>
//constants 
const int gridSize = 64;
const int nbCells = gridSize * gridSize * gridSize;
const int bagCapacity = 100;

const double charge = 1.0;
const double step_duration = 0.2;

int nbSteps = 100;

//vectors

typedef struct{
  double x, y, z;
}vect;

vect vect_add(vect v1, vect v2) {
  vect v = { v1.x + v2.x,
             v1.y + v2.y,
             v1.z + v2.z };
  return v;
}

vect vect_mul(double d, vect v) {
  vect w = { d * v.x,
             d * v.y,
             d * v.z };
  return w;
}



typedef struct{
  vect pos;
  vect speed;

} particle;


typedef struct {
  int nb;
  particle items[bagCapacity];
} bag;


void bag_push(bag* b, particle p) {
  (*b).items[(*b).nb] = p;
  (*b).nb++;
}
/*
void bag_clear(bag* b) {
  (*b).nb = 0;
}
*/
void bag_transfer(bag* b1, bag* b2) {
  //Move all the items from b2 to b1
  // Note: int the real code, bqgs are linked lists
  // so this operation only involves a pointer assignment
  //not a deep copy of an arraw.
  for(int i = 0; i < (*b2).nb; i++){
    //bag_push(b1,(*b2).items[i]);
  }
  //bag_clear(b2);
}

// --Data structures

// in the real code, the charge is associated not to each cell, 
// but to each corner of a cell in the grid


vect fields[nbCells];
double nextCharge[nbCells];
bag bagsCur[nbCells];
bag bagsNext[nbCells];



int idCellOfPos(vect pos){return 0;}



int main () {
  
  // for each time step
  for(int step = 0; step < nbSteps; step++){
    // for each cell from the grid
    for(int idCell = 0; idCell < nbCells; idCell++){
      // read the electric field for that cell 
      vect field = fields[idCell];

      // foreach particle in that cell
      bag* b = bagsCur;
      int nb = b[nbCells].nb;
      for(int idParticle = 0; idParticle < nb; idParticle++){
        particle p = (*b).items[idParticle];
        vect speed2={0.,0.,0.};
        // compute speed and poistion
        speed2 = vect_add(p.speed, vect_mul(charge, field));
        vect pos2 = vect_add(p.pos, vect_mul(step_duration,speed2));

        //deposit particle charge
        int idCell2 = idCellOfPos(pos2);
        nextCharge[idCell2] += charge;

        //write particle into the target cell
        particle p2 ={speed2, pos2};
        //bag_push(&bagsNext[idCell2], p2);
      }

    }
  }
  return 0;
}