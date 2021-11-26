
#include <stdio.h>
#include "particle.h"

//==========================================================================
// Representation of chunks

/*
 * In this example, CHUNK_SIZE = 20.
 *
 * ? in the array means allocated space for the array, not filled with elements.
 *
 *           Chunk bag
 *       { front   back  };
 *       |———————|———————|
 *       |       |       |
 *       |   X   |   X   |
 *       |   |   |   |   |
 *       |———+———|———+———|
 *           |       |
 *           |       +—————————————————————————————————————+
 *           |                                             |
 *           |     +——————+     +— ...     ——+     +————+  |
 *           |     |      |     |            |     |    |  |
 *           v     |      v     |            v     |    v  v
 *       |———————| |  |———————| |        |———————| |  |———————|
 *       |       | |  |       | |        |       | |  |       |
 *  next |   X———+—+  |   X———+—+        |   X———+—+  |   X———+——>NIL
 *       |       |    |       |          |       |    |       |
 *       |———————|    |———————|    ...   |———————|    |———————|
 *       |       |    |       |          |       |    |       |
 *  size |  16   |    |  11   |          |  13   |    |  17   |
 *       |       |    |       |          |       |    |       |
 *       |———————|    |———————|    ...   |———————|    |———————|
 *     0 |       |    |       |          |       |    |       |
 *     1 |       |    |       |          |       |    |       |
 *     2 |       |    |       |          |       |    |       |
 *     3 |       |    |       |          |       |    |       |
 *     4 |       |    |       |          |       |    |       |
 *     5 |       |    |       |          |       |    |       |
 *     6 |       |    |       |          |       |    |       |
 *     7 |       |    |       |          |       |    |       |
 *     8 |       |    |       |          |       |    |       |
 *     9 |       |    |       |          |       |    |       |
 *    10 |       |    |       |          |       |    |       |
 *    11 |       |    |   ?   |          |       |    |       |
 *    12 |       |    |   ?   |          |       |    |       |
 *    13 |       |    |   ?   |          |   ?   |    |       |
 *    14 |       |    |   ?   |          |   ?   |    |       |
 *    15 |       |    |   ?   |          |   ?   |    |       |
 *    16 |   ?   |    |   ?   |          |   ?   |    |       |
 *    17 |   ?   |    |   ?   |          |   ?   |    |   ?   |
 *    18 |   ?   |    |   ?   |          |   ?   |    |   ?   |
 *    19 |   ?   |    |   ?   |          |   ?   |    |   ?   |
 *       |———————|    |———————|    ...   |———————|    |———————|
 *
 */


const int CHUNK_SIZE = 128;


//==========================================================================
// Representation

/*
 * A chunk is a fixed-capacity array of particles, with a pointer to the next chunk.
 */
typedef struct chunk {
  chunk* next; // null if last in the chain
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


//==========================================================================
// Auxiliary tool

/* [internal function]
 * Guarantee that the entire value of *p is read atomically.
 * No part of *p can change during the read operation.
 *
 * Taken from openmp-examples-4.5.0.pdf, Example atomic.2.c
 */
chunk* atomic_read(chunk** p) {
  chunk* value;
  #pragma omp atomic read
  value = *p;
  return value;
}



//==========================================================================
// Naive chunk allocation operations

chunk* chunk_alloc() {
  return (chunk*) malloc(sizeof(chunk));
}

void chunk_free(chunk* c) {
  free(c);
}


//==========================================================================
// External chunk operations

/*
 * Initialize a bag (already allocated) with only one empty chunk,
 * taken from the freelists (they hold enough spare chunks).
 * WARNING: 1. Must have called compute_cumulative_free_list_sizes first.
 *          2. Must call update_free_list_sizes when all the bags are initialized.
 *
 * Note : When SPARE_LOC_OPTIMIZED is set, you only go through the cumulative sum
 * to find the chunk when you reach the end of a freelist.
 *        When it's not set, you recompute it every time you need a chunk.
 *
 * In this example, CHUNK_SIZE = 20.
 * The array is filled with ? because it's not filled yet.
 *
 *           Chunk bag
 *       { front   back  };
 *       |———————|———————|
 *       |       |       |
 *       |   X   |   X   |
 *       |   |   |   |   |
 *       |———+———|———+———|
 *           |       |
 *           |  +————+
 *           |  |
 *           v  v
 *        |———————|
 *        |       |
 *   next |   X———+——>NIL
 *        |       |
 *        |———————|
 *        |       |
 *   size |   0   |
 *        |       |
 *        |———————|
 *      0 |   ?   |
 *      1 |   ?   |
 *      2 |   ?   |
 *      3 |   ?   |
 *      4 |   ?   |
 *      5 |   ?   |
 *      6 |   ?   |
 *      7 |   ?   |
 *      8 |   ?   |
 *      9 |   ?   |
 *     10 |   ?   |
 *     11 |   ?   |
 *     12 |   ?   |
 *     13 |   ?   |
 *     14 |   ?   |
 *     15 |   ?   |
 *     16 |   ?   |
 *     17 |   ?   |
 *     18 |   ?   |
 *     19 |   ?   |
 *        |———————|
 *
 * @param[in, out] b the bag to initialize.
 */
void bag_init(bag* b, int id_bag, int id_cell) {
  chunk* c = chunk_alloc();
  c->size = 0;
  c->next = NULL;
  b->front = c;
  b->back  = c;
}

/*
 * Merge other into b; other is re-initialized if not void,
 * and stays void if it was already void.
 *
 * @param[in, out] b
 * @param[in, out] other
 */
void bag_append(bag* b, bag* other, int id_bag, int id_cell) {
  if (other->front) {
    b->back->next = other->front;
    b->back       = other->back;
    bag_init(other, id_bag, id_cell);
  }
}

/*
 * Nullify a bag (already allocated).
 *
 *           Chunk bag
 *       { front   back  };
 *       |———————|———————|
 *       |       |       |
 *       |   X   |   X   |
 *       |   |   |   |   |
 *       |———+———|———+———|
 *           |       |
 *           v       v
 *              null
 *
 * @param[in, out] b the bag to nullify.
 */
void bag_nullify(bag* b) {
  b->front = NULL;
  b->back  = NULL;
}

/*
 * Compute the number of elements stored into all
 * chunks of a chunk bag.
 *
 * @param[in] b the bag.
 * @return    the number of elements stored into b.
 */
int bag_size(bag* b) {
  chunk* c = b->front;
  int size = 0;
  while (c) {
    size += c->size;
    c = c->next;
  }
  return size;
}

/*
 * [private function]
 * Allocate a new chunk and put it at the front of a chunk bag.
 * We have to set the field "size" to 0, because:
 *   1. When a chunk goes back to a freelist, this field is not reset.
 *   2. When a chunk is newly allocated, this field is not set neither.
 * It is simpler to set to 0 in this lone function, rather than
 * both in the allocation and in the free functions.
 *
 * @param[in, out] b the bag in which to put the new chunk.
 */
void bag_add_front_chunk(bag* b) {
  chunk* c = chunk_alloc();
  // Warning - TODO: the instruction c->size=0 might be viewd switched with b->front=c by other threads, which would lead to non-valid code.
  // (e.g. on PowerPC, the present code is valid on Intel).
  // Solution: adding a memory fence (putting write c->size=0 when freeing a chunk, and not when adding it is not enough).
  c->size = 0;
  c->next = b->front;
  #pragma omp atomic write
  b->front = c;
}

/*
 * Add a particle into a chunk bag. Add it into the first chunk,
 * then tests if this chunk is full. In that case, allocate a new
 * chunk after adding the particle.
 * This function is thread-safe (uses atomics).
 *
 * Adapted from openmp-examples-4.5.0.pdf, Example atomic.3.c
 * No flush is needed here, because the only variable that needs
 * to be really shared is c->size.
 *
 * @param[in, out] b
 * @param[in]      p
 */
void bag_push_concurrent(bag* b, particle p) {
  chunk* c;
  int index;
  while (true) { // Until success.
    c = b->front;

    #pragma omp atomic capture
    index = c->size++;

    if (index < CHUNK_SIZE) {
      // The chunk is not full, we can write the particle.
      c->items[index] = p;
      if (index == CHUNK_SIZE - 1) {
        // The chunk is now full, we extend the bag.
        // Inside bag_add_front_chunk, the update of the b->front
        // pointer is made atomic so that other threads see the update.
        bag_add_front_chunk(b);
      }
      return;
    } else {
      // The chunk is full, another thread has just pushed a particle
      // at the end, and is now extending the bag.
      // First we have to cancel our additional "c->size++". This can be done
      // either with an "atomic c->size--", or just without atomics this way:
      c->size = CHUNK_SIZE;
      while (atomic_read(&b->front) == c) {
        // Then we wait until the other thread extends the bag.
        // The atomic_read forces the thread to read the value in the
        // main memory, and not in its temporary view.
      }
    }
  }
}

/*
 * Add a particle into a chunk bag. Add it into the first chunk,
 * then tests if this chunk is full. In that case, allocate a new
 * chunk after adding the particle.
 * This function is not thread-safe. Multiple threads should not
 * access the same bag.
 *
 * @param[in, out] b
 * @param[in]      p
 */
void bag_push_serial(bag* b, particle p) {
  chunk* c = b->front;
  int index = c->size++;
  c->items[index] = p;
  if (index == CHUNK_SIZE - 1) {
    // chunk is full, we extend the bag
    bag_add_front_chunk(b);
  }
}

void bag_push(bag* b, particle p) {
  return bag_push_serial(b, p);
}

/*
 * Swap the contents of two bags
 */
void bag_swap(bag* b1, bag* b2) {
  bag temp =*b1;
  *b1 = *b2;
  *b2 = temp;
}

//==========================================================================
// Iteration

// First-order iterator
typedef struct bag_iter {
  chunk* iter_chunk;
  int size;
  int index;
} bag_iter;

void bag_iter_load_chunk(bag_iter* it, chunk* c) {
  it->iter_chunk = c;
  // if (c == NULL) { return; }   // technically optional as we assume all bags to have at least one chunk
  it->size = c->size;
  it->index = 0;
}

void bag_iter_init(bag_iter* it, bag* b) {
  bag_iter_load_chunk(it, b->front);
}

bag_iter bag_iter_begin(bag* b) {
  bag_iter it;
  bag_iter_init(&it, b);
  return it;
}

particle* bag_iter_get(bag_iter* it) {
  return &(it->iter_chunk->items[it->index]);
}

chunk* bag_iter_get_chunk(bag_iter* it) {
  return it->iter_chunk;
}

// Return the next chunk in the chain, possibly Null;
// Deallocate the argument [c] if [destructive] is true
chunk* chunk_next(chunk* c, bool destructive) {
  chunk* cnext = c->next;
  if (destructive) {
    chunk_free(c);
  }
  return cnext;
}

// Return the next particle, or NULL if at the end
// (we cannot return one-past-the-end pointer because chunks are deallocated)
particle* bag_iter_next(bag_iter* it, bool destructive) {
  it->index++;
  if (it->index == it->size) {
    chunk* c = it->iter_chunk;
    chunk* cnext = chunk_next(c, destructive);
    if (cnext == NULL) {
      return NULL;
    }
    bag_iter_load_chunk(it, cnext);
  }
  return bag_iter_get(it);
}

/*
TODO: to parse this code, need to fix the function Ast.trm_for_of_trm_for_c
where "is_simple_loop" is not correctly computed (see, e.g.,  the branch
  | _ -> true   in  is_simple_loop_component). More generally, one should
  use different tests for each of the components, not a unified one.

// example of a basic iteration over a bag
void bag_ho_iter_basic(bag* b, void body(particle*)) {
  bag_iter it = bag_iter_begin(b);
  for (particle* p = bag_iter_get(&it); p != NULL; p = bag_iter_next(&it, true)) {
    body(p);
  }
}pas

// example of an iteration over a bag with the loop over the chunk items revealed
void bag_ho_iter_chunk(bag* b, void body(particle*)) {
  for (chunk* c = b->front; c != NULL; c = chunk_next(c, true)) {
    int nb = c->size;
    for (int i = 0; i < nb; i++) {
      particle* p = &c->items[i];
      body(p);
    }
  }
}

*/



//==========================================================================
// Initial, sequential manipulation of bags

// TODO: inline this functions in the main initialization code

/*
 * The following functions do the same thing as their counterparts
 * without "_initial" in their names, except here they are used only in
 * the initialization of the particles, and not in the rest of the simulation.
 * All the free chunks are in only one list. At the end of the initialization,
 * they have to be split into one free list per thread.
 */

void bag_push_initial(bag* b, particle p) {
  bag_push_serial(b, p); // threadid=THREAD_INITIAL
}

void bag_init_initial(bag* b) {
  bag_init(b, -1, -1); // threadid=THREAD_INITIAL
  // TODO: maybe put the thread_id argument before id_bag and id_cell (both are dummy values here)
}


