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

void bag_init(bag*);

void bag_swap(bag*, bag*);

void bag_merge(bag*, bag*);

void bag_free(bag*);

int main() {
  const int nbCells = 100;
  const int N0 = 5;
  const int N1 = 10;
  const int N2 = 11;
  const int N3 = 12;
  bag* bagCur = (bag*)malloc(MSIZE1(nbCells) * sizeof(bag));
  bag_iter bag_it;
  bag* bagNext = (bag*)malloc(MSIZE1(nbCells) * sizeof(bag));
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_init(&bagNext[MINDEX1(nbCells, idCell)]);
  }
mark : {
  bag* bagNexts = (bag*)malloc(MSIZE2(N0, nbCells) * sizeof(bag));
  for (int i1 = 0; i1 < nbCells; i1++) {
    for (int i0 = 0; i0 < N0; i0++) {
      bag_init(&bagNexts[MINDEX2(N0, nbCells, i0, i1)]);
    }
  }
  for (int idCell = 0; idCell < nbCells; idCell++) {
    for (particle* p = bag_iter_begin(&bag_it, NULL); p != NULL;
         p = bag_iter_next_common(&bag_it, true)) {
      bag_push(&bagNexts[MINDEX2(N0, nbCells, ANY(N0), idCell)], *p);
    }
  }
  for (int i1 = 0; i1 < nbCells; i1++) {
    for (int i0 = 0; i0 < N0; i0++) {
      bag_merge(&bagNext[MINDEX1(nbCells, i1)],
                &bagNexts[MINDEX2(N0, nbCells, i0, i1)]);
    }
  }
  for (int i1 = 0; i1 < nbCells; i1++) {
    for (int i0 = 0; i0 < N0; i0++) {
      bag_free(&bagNexts[MINDEX2(N0, nbCells, i0, i1)]);
    }
  }
  free(bagNexts);
}
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_swap(&bagNext[MINDEX1(nbCells, idCell)],
             &bagCur[MINDEX1(nbCells, idCell)]);
  }
  free(bagCur);
  free(bagNext);
  return 0;
}
