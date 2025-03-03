#include <optitrust.h>

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


const int nbCells = 100;
const int N0 = 5;
const int N1 = 10;
const int N2 = 11;
const int N3 = 12;


particle* bag_iter_begin(bag_iter* it, bag* b);
particle* bag_iter_next_common(bag_iter* it, bool destructive);
void bag_push(bag* b, particle p);
void bag_init(bag* b);
void bag_swap(bag* b1, bag* b2);
void bag_merge(bag* b1, bag* b2);
void bag_free(bag* b);

bag* bagNext1;

void allocate (){
  bagNext1 = MALLOC1(bag, nbCells);
}


int main() {

// Allocate bagsNext and bagsCur with empty bags in every cell

  bag* bagCur = MALLOC1(bag, nbCells);
  bag_iter bag_it;

  bag* bagNext = MALLOC1(bag, nbCells);
  for (int idCell = 0; idCell < nbCells; idCell++){
    bag_init(&bagNext[MINDEX1(nbCells,idCell)]);
  }

  for (int idCell = 0; idCell < nbCells; idCell++){
    for (particle* p = bag_iter_begin(&bag_it, NULL); p != NULL; p = bag_iter_next_common(&bag_it, true)) {
      bag_push(&bagNext[MINDEX1(nbCells, idCell)],*p);
    }
  }

  for (int idCell = 0; idCell < nbCells; idCell++){
    bag_swap(&bagNext[MINDEX1(nbCells,idCell)], &bagCur[MINDEX1(nbCells,idCell)]);
  }

  for (int idCell = 0; idCell < nbCells; idCell++){
    for (particle* p = bag_iter_begin(&bag_it, NULL); p != NULL; p = bag_iter_next_common(&bag_it, true)) {
      bag_push(&bagNext1[MINDEX1(nbCells, idCell)],*p);
    }
  }

  for (int idCell = 0; idCell < nbCells; idCell++){
    bag_swap(&bagNext1[MINDEX1(nbCells,idCell)], &bagCur[MINDEX1(nbCells,idCell)]);
  }

  free(bagCur);
  free(bagNext);
  free(bagNext1);

  return 0;

}
