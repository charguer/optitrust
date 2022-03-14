#include "../../include/optitrust.h"


typedef struct {
  double x, y, z;
} vect;

typedef struct {
  vect pos;
  vect speed;
} particle;


typedef struct chunk {
  struct chunk* next; // null if last in the chain
  int size;
  particle items[10];
} chunk;

typedef struct {
  chunk* front;
  chunk* back;
} bag;

typedef struct bag_iter {
  chunk* iter_chunk;
  int size;
  int index;
} bag_iter;


particle* bag_iter_begin(bag_iter* it, bag* b);
particle* bag_iter_next_common(bag_iter* it, bool destructive);
void bag_push(bag* b, particle p);
void bag_init(bag* b);
void bag_swap(bag* b1, bag* b2);
void bag_merge(bag* b1, bag* b2);
void bag_free(bag* b);

int main() {
  int const nbCells = 100;
  int const N0 = 5;
  int const N1 = 10;
  int const N2 = 11;
  int const N3 = 12;

// Allocate bagsNext and bagsCur with empty bags in every cell
  
  bag* bagCur = (bag*) MMALLOC1(nbCells, sizeof(bag));
  bag_iter bag_it;
  
  bag* bagNext = (bag*) MMALLOC1(nbCells, sizeof(bag));
  for (int idCell = 0; idCell < nbCells; idCell++){
    bag_init(&bagNext[MINDEX1(nbCells,idCell)], 0, idCell);
  }
  
  for (int idCell = 0; idCell < nbCells; idCell++){
    for (particle* p = bag_iter_begin(&bag_it, NULL); p != NULL; p = bag_iter_next_common(&bag_it, true)) {
      bag_push(&bagNext[MINDEX1(nbCells, idCell)],*p);
    }
  }
    
  for (int idCell = 0; idCell < nbCells; idCell++){
    bag_swap(&bagNext[MINDEX1(nbCells,idCell)], &bagCur[MINDEX1(nbCells,idCell)]);
  }
  MFREE(bagCur);
  MFREE(bagNext);
  
  return 0;

}
