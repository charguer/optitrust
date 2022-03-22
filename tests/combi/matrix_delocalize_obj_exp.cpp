#include "../../include/optitrust.h"

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
  struct chunk *next;
  int size;
  particle items[10];
} chunk;

typedef struct {
  chunk *front;
  chunk *back;
} bag;

typedef struct bag_iter {
  chunk *iter_chunk;
  int size;
  int index;
} bag_iter;

particle *bag_iter_begin(bag_iter *it, bag *b);

particle *bag_iter_next_common(bag_iter *it, bool destructive);

void bag_push(bag *b, particle p);

void bag_init(bag *b);

void bag_swap(bag *b1, bag *b2);

void bag_merge(bag *b1, bag *b2);

void bag_free(bag *b);

int main() {
  const int nbCells = 100;
  const int N0 = 5;
  const int N1 = 10;
  const int N2 = 11;
  const int N3 = 12;
  bag *bagCur = (bag *)MMALLOC1(nbCells, sizeof(bag));
  bag_iter bag_it;
  bag *bagNext = (bag *)MMALLOC1(nbCells, sizeof(bag));
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_init(&bagNext[MINDEX1(nbCells, idCell)]);
  }
  bag *bagNexts = (bag *)MMALLOC2(nbCells, N0, sizeof(bag));
  for (int idCell = 0; idCell < nbCells; idCell++) {
    for (int bagKind = 0; bagKind < N0; bagKind++) {
      bag_init(&bagNexts[MINDEX2(nbCells, N0, idCell, bagKind)]);
    }
  }
  for (int idCell = 0; idCell < nbCells; idCell++) {
    for (particle *p = bag_iter_begin(&bag_it, NULL); p != NULL;
         p = bag_iter_next_common(&bag_it, true)) {
      bag_push(&bagNexts[MINDEX2(nbCells, N0, idCell, ANY(N0))], *p);
    }
  }
  for (int idCell = 0; idCell < nbCells; idCell++) {
    for (int bagKind = 0; bagKind < N0; bagKind++) {
      bag_merge(&bagNext[MINDEX1(nbCells, idCell)],
                &bagNexts[MINDEX2(nbCells, N0, idCell, bagKind)]);
    }
  }
  for (int idCell = 0; idCell < nbCells; idCell++) {
    for (int bagKind = 0; bagKind < N0; bagKind++) {
      bag_free(&bagNexts[MINDEX2(nbCells, N0, idCell, bagKind)]);
    }
  }
  MFREE(bagNexts);
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_swap(&bagNext[MINDEX1(nbCells, idCell)],
             &bagCur[MINDEX1(nbCells, idCell)]);
  }
  MFREE(bagCur);
  MFREE(bagNext);
  return 0;
}
