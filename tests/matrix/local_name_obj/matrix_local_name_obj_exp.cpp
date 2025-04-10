#include <optitrust.h>

typedef struct {
  double x;
  double y;
  double z;
} vect;

typedef struct {
  vect pos;
  vect speed;
} particle;

typedef struct chunk {
  chunk* next;
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

particle* bag_iter_begin(bag_iter*, bag*);

particle* bag_iter_next_common(bag_iter*, bool);

void bag_push(bag*, particle);

void bag_init(bag*, int, int);

void bag_swap(bag*, bag*);

void bag_merge(bag*, bag*);

void bag_free(bag*);

int main() {
  const int nbCells = 100;
  const int N0 = 5;
  const int N1 = 10;
  const int N2 = 11;
  const int N3 = 12;
  bag* const bagCur = (bag*)malloc(MSIZE1(nbCells) * sizeof(bag));
  bag_iter bag_it;
  bag* const bagNext = (bag*)malloc(MSIZE1(nbCells) * sizeof(bag));
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_init(&bagNext[MINDEX1(nbCells, idCell)], 0, idCell);
  }
  bag* const bagNexts = (bag*)malloc(MSIZE1(nbCells) * sizeof(bag));
  for (int i1 = 0; i1 < nbCells; i1++) {
    bag_init(&bagNexts[MINDEX1(nbCells, i1)]);
  }
  for (int idCell = 0; idCell < nbCells; idCell++) {
    for (particle* p = bag_iter_begin(&bag_it, NULL); p != NULL;
         p = bag_iter_next_common(&bag_it, true)) {
      bag_push(&bagNexts[MINDEX1(nbCells, idCell)], *p);
    }
  }
  for (int i1 = 0; i1 < nbCells; i1++) {
    bag_merge(&bagNext[MINDEX1(nbCells, i1)], &bagNexts[MINDEX1(nbCells, i1)]);
  }
  for (int i1 = 0; i1 < nbCells; i1++) {
    bag_free(&bagNexts[MINDEX1(nbCells, i1)]);
  }
  free(bagNexts);
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_swap(&bagNext[MINDEX1(nbCells, idCell)],
             &bagCur[MINDEX1(nbCells, idCell)]);
  }
  free(bagCur);
  free(bagNext);
  return 0;
}
