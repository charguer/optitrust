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
  struct chunk* next;
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

bag* bagNexts1;

void allocate() {
  bagNext1 = (bag*)MALLOC1(nbCells, sizeof(bag));
  /*no-brace*/ {
    /*no-brace*/ { bagNexts1 = (bag*)MALLOC2(nbCells, N0, sizeof(bag)); }
    for (int idCell = 0; idCell < nbCells; idCell++) {
      for (int bagKind = 0; bagKind < N0; bagKind++) {
        bag_init(&bagNexts1[MINDEX2(nbCells, N0, idCell, bagKind)]);
      }
    }
  }
}

int main() {
  bag* bagCur = (bag*)MALLOC1(nbCells, sizeof(bag));
  bag_iter bag_it;
  bag* bagNext = (bag*)MALLOC1(nbCells, sizeof(bag));
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_init(&bagNext[MINDEX1(nbCells, idCell)]);
  }
  /*no-brace*/ {
    bag* bagNexts = (bag*)MALLOC2(nbCells, N0, sizeof(bag));
    for (int idCell = 0; idCell < nbCells; idCell++) {
      for (int bagKind = 0; bagKind < N0; bagKind++) {
        bag_init(&bagNexts[MINDEX2(nbCells, N0, idCell, bagKind)]);
      }
    }
    for (int idCell = 0; idCell < nbCells; idCell++) {
      for (particle* p = bag_iter_begin(&bag_it, NULL); p != NULL;
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
    MFREE1(nbCells, bagNexts);
  }
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_swap(&bagNext[MINDEX1(nbCells, idCell)],
             &bagCur[MINDEX1(nbCells, idCell)]);
  }
  /*no-brace*/ {
    /*no-brace*/ {
      for (int idCell = 0; idCell < nbCells; idCell++) {
        for (particle* p = bag_iter_begin(&bag_it, NULL); p != NULL;
             p = bag_iter_next_common(&bag_it, true)) {
          bag_push(&bagNexts1[MINDEX2(nbCells, N0, idCell, ANY(N0))], *p);
        }
      }
      for (int idCell = 0; idCell < nbCells; idCell++) {
        for (int bagKind = 0; bagKind < N0; bagKind++) {
          bag_merge(&bagNext1[MINDEX1(nbCells, idCell)],
                    &bagNexts1[MINDEX2(nbCells, N0, idCell, bagKind)]);
        }
      }
    }
    /*no-brace*/ {
      for (int idCell = 0; idCell < nbCells; idCell++) {
        for (int bagKind = 0; bagKind < N0; bagKind++) {
          bag_free(&bagNexts1[MINDEX2(nbCells, N0, idCell, bagKind)]);
        }
      }
      MFREE1(nbCells, bagNexts1);
    }
  }
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_swap(&bagNext1[MINDEX1(nbCells, idCell)],
             &bagCur[MINDEX1(nbCells, idCell)]);
  }
  MFREE(bagCur);
  MFREE(bagNext);
  MFREE(bagNext1);
  return 0;
}
