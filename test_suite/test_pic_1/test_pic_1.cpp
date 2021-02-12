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
/*
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


*/
typedef struct{
  vect pos;
  vect speed;

} particle;


typedef struct {
  int nb;
  particle items[bagCapacity];
} bag;
/*
void bag_push(bag* b, particle p) {
  (*b).items[(*b).nb] = p;
  (*b).nb++;
}

void bag_clear(bag* b) {
  (*b).nb = 0;
}

void bag_transfer(bag* b1, bag* b2) {
  //Move all the items from b2 to b1
  // Note: int the real code, bqgs are linked lists
  // so this operation only involves a pointer assignment
  //not a deep copy of an arraw.
  for(int i = 0; i < (*b2).nb; i++){
    bag_push(b1,(*b2).items[i]);
  }
  bag_clear(b2);
}

// --Data structures

// in the real code, the charge is associated not to each cell, 
// but to each corner of a cell in the grid


vect fields[nbCells];
double nextCharge[nbCells];
bag bagsCur[nbCells];
bag bagsNext[nbCells];



int idCellOfPos(vect pos){return 0;}


*/
/*
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
        
        //update particle
        
        vect v2 = vect_mul(charge, field);

        // compute speed and poistion
        vect speed2;
        speed2.x = p.speed.x + charge * field.x;
        speed2.y = p.speed.y + charge * field.y;
        speed2.z = p.speed.z + charge * field.z;



       
        

        vect pos2 = vect_add(p.pos, vect_mul(step_duration,speed2));

        //deposit particle charge
        int idCell2 = idCellOfPos(pos2);
        nextCharge[idCell2] += charge;

        //write particle into the target cell
        //particle p2 ={{speed2.x, speed2.y, speed2.z},{pos2.x, pos2.y,pos2.y}};
        
        bag* b2 = bagsNext[idCell2];
        int k = (*b2).nb;
        (*b2).items[k].pos.x = pos2.x;
        (*b2).items[k].pos.y = pos2.y;
        (*b2).items[k].pos.z = pos2.z;
        (*b2).items[k].speed.x = speed2.x;
        (*b2).items[k].speed.y = speed2.y;
        (*b2).items[k].speed.z = speed2.z;
        (*b2).nb++;
    

      }
      //empty the source cell
      bag_clear(&bagsCur[idCell]);
    }
    /*
    //update charge field
    //updateFieldsUsingNextCharge();
    for(int idCell = 0; idCell < nbCells; idCell++){
      bag_transfer(&bagsCur[idCell],&bagsNext[idCell]);
    }
  }
  return 0;
  
}

*/