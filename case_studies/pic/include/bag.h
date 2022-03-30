#ifndef BAG_H
#define BAG_H

#include <stdio.h>
#include "particle.h"

//==========================================================================

#ifndef CHUNK_SIZE
  const int CHUNK_SIZE = 128; // we might prefer to raise an error if CHUNK_SIZE is undef (?)
#endif


/*
 * A chunk is a fixed-capacity array of particles, with a pointer to the next chunk.
 */
typedef struct chunk {
  struct chunk* next; // null if last in the chain
  int size;
  particle items[CHUNK_SIZE];
} chunk;

/*
 * A bag is a pair of pointers on the first and the last chunk (possibly the same).
 */
typedef struct {
  chunk* front;
  chunk* back;
} bag;

// First-order iterator
typedef struct bag_iter {
  bool destructive;
  chunk* iter_chunk;
  int size;
  int index;
} bag_iter;


void bag_init(bag* b);
void bag_append_noinit(bag* b, bag* other);
void bag_append(bag* b, bag* other);
void bag_nullify(bag* b);
int bag_size(bag* b);
void bag_add_front_chunk(bag* b);
void bag_push_concurrent(bag* b, particle p);
void bag_push_serial(bag* b, particle p);
void bag_push(bag* b, particle p);
void bag_swap(bag* b1, bag* b2);
void bag_push(bag* b, particle p);
void bag_init(bag* b);
void bag_free(bag* b);

chunk* chunk_next(chunk* c, bool destructive);

void bag_iter_load_chunk(bag_iter* it, chunk* c);
void bag_iter_init(bag_iter* it, bag* b, bool destructive);
particle* bag_iter_get(bag_iter* it);
chunk* bag_iter_get_chunk(bag_iter* it);
particle* bag_iter_begin_common(bag_iter* it, bag* b, bool destructive);
particle* bag_iter_begin(bag_iter* it, bag* b);
particle* bag_iter_destructive_begin(bag_iter* it, bag* b);

particle* bag_iter_next_common(bag_iter* it);
particle* bag_iter_next(bag_iter* it);
particle* bag_iter_next_destructive(bag_iter* it);
void bag_iter_ho_basic(bag* b, void body(particle*), bool destructive);
void bag_iter_ho_chunk(bag* b, void body(particle*), bool destructive);

#endif
