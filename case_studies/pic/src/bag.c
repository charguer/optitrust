#ifndef PARTICLE_CHUNK_H
#define PARTICLE_CHUNK_H

#include "bag.h"
#include "bag_atomics.h"

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


//==========================================================================
// Naive chunk allocation operations

#define VEC_ALIGN 64

chunk* chunk_alloc() {
  // non-aligned would fail: return (chunk*) malloc(sizeof(chunk));
  chunk* c;
  // we need chunks to be aligned
  if (posix_memalign((void**)&c, VEC_ALIGN, sizeof(chunk))) {
      fprintf(stderr, "chunk_alloc: posix_memalign.\n");
      exit(EXIT_FAILURE);
  }
  return c;
}

void chunk_free(chunk* c) {
  free(c);
}

//==========================================================================
// Bag chunk allocation operations, naive versions

#ifndef CUSTOM_BAG_CHUNK_ALLOC

chunk* bag_chunk_alloc() {
  return chunk_alloc();
}

void bag_chunk_free(chunk* c) {
  chunk_free(c);
}

#endif

//==========================================================================
// Bag chunk allocation operations, custom allocator for pic_demo.c

#ifdef CUSTOM_BAG_CHUNK_ALLOC

// ----- functions to get nbThreads and idThread

#include <omp.h>

void get_num_threads() {
  int nbThreads;
  #pragma omp parallel
  {
    #pragma omp single
    nbThreads = omp_get_num_threads();
  }
  return nbThreads;
}

void get_id_thread() {
  int idThread;
  #pragma omp parallel
  {
    #pragma omp single
    idThread = omp_get_thread_num();
  }
  return idThread;
}

// ----- free lists data structure

typedef struct {
  int nb;
  int padding[7]; // alternative is to put alignas
  chunk* items; // of size freelist_capacity = 2*nbCells
} chunks_freelist;

chunks_freelist* chunk_freelists; // of size nbThreads
int freelist_capacity; // = 2*nbCells

void init_freelists(int nbCells) {
  const int nbThreads = get_num_threads();
  freelist_capacity = 2 * nbCells;
  chunk_freelists = (chunk_free_list*) malloc(nbThreads * sizeof(chunks_freelist));
  for (int idThread = 0; idThread < nbThreads; idThread++) {
    chunk_freelist* cf = &chunk_freelists[idThread];
    cf->nb = 0;
    cf->items = malloc(freelist_capacity * sizeof(chunk*));
  }
}

void release_freelists() {
  const int nbThreads = get_num_threads();
  for (int idThread = 0; idThread < nbThreads; idThread++) {
    chunk_freelist* cf = &chunk_freelists[idThread];
    for (int i = 0; i < cf->nb; i++) {
      chunk_free(cf->items[i]);
      free(cf->items);
    }
  }
  free(chunk_freelists);
}

// ----- custom implementation of chunk alloc and free

chunk* freelist_try_pop(int idThread) {
  chunk_freelist* cf = &chunk_freelists[idThread];
  if (cf->nb > 0) {
    cf->nb--;
    return cf->items[cf->nb];
  } else {
    return NULL;
  }
}

// initialize a collection of bags, using all freelists, from a single-threaded context
void bag_init_array(bag** bags, int nb) {
  // LATER: this could be done in parallel (requires prefix sums and tiling to balance the load)
  const int nbThreads = get_num_threads();
  int idThread = 0;
  chunk_freelist* cf = &chunk_freelists[idThread];
  for (int i = 0; i < nb; i++) {
    chunk* c = freelist_try_pop(idThread);
    if (c == NULL) {
      idThread++;
      if (idThread == nbThreads) { // complete using alloc
        for (; i < nb; i++) {
          bag_init_using(bags[i], chunk_alloc());
        }
        return;
      }
      i--; // restart the iteration with another thread
      continue;
    } else {
      bag_init_using(bags[i], c);
    }
  }
}

chunk* bag_chunk_alloc() {
  const int idThread = get_id_thread();
  chunk_freelist* cf = &chunk_freelists[idThread];
  if (cf->nb > 0) {
    // pop from nonempty free list
    cf->nb--;
    return cf->items[cf->nb];
  } else {
    return chunk_alloc();
  }
}

void bag_chunk_free(chunk* c) {
  const int idThread = get_id_thread();
  chunk_freelist* cf = &chunk_freelists[idThread];
  if (cf->nb < freelist_capacity) {
    // push in nonfull freelist
    cf->items[cf->nb] = c;
    cf->nb++;
  } else {
    chunk_free(c);
  }
}

#endif

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

void bag_init_using(bag* b, chunk* c) {
  c->size = 0;
  c->next = NULL;
  b->front = c;
  b->back  = c;
}

void bag_init(bag* b) {
  chunk* c = bag_chunk_alloc();
  bag_init_using(b, c);
}

/*
 * Merge other into b; other is re-initialized if not void,
 * and stays void if it was already void.
 *
 * @param[in, out] b
 * @param[in, out] other
 */
void bag_append(bag* b, bag* other) {
  if (other->front) {
    b->back->next = other->front;
    b->back       = other->back;
    bag_init(other);
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
void bag_add_front_chunk_serial(bag* b) {
  chunk* c = bag_chunk_alloc();
  // Warning - TODO: the instruction c->size=0 might be viewd switched with b->front=c by other threads, which would lead to non-valid code.
  // (e.g. on PowerPC, the present code is valid on Intel).
  // Solution: adding a memory fence (putting write c->size=0 when freeing a chunk, and not when adding it is not enough).
  c->size = 0;
  c->next = b->front;
  b->front = c;
}

void bag_add_front_chunk_concurrent(bag* b) {
  chunk* c = bag_chunk_alloc();
  // Warning - TODO: the instruction c->size=0 might be viewd switched with b->front=c by other threads, which would lead to non-valid code.
  // (e.g. on PowerPC, the present code is valid on Intel).
  // Solution: adding a memory fence (putting write c->size=0 when freeing a chunk, and not when adding it is not enough).
  c->size = 0;
  c->next = b->front;
  atomic_write_chunk(&b->front, c);
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

    index = atomic_increment(&c->size);

    if (index < CHUNK_SIZE) {
      // The chunk is not full, we can write the particle.
      c->items[index] = p;
      if (index == CHUNK_SIZE - 1) {
        // The chunk is now full, we extend the bag.
        // Inside bag_add_front_chunk, the update of the b->front
        // pointer is made atomic so that other threads see the update.
        bag_add_front_chunk_concurrent(b);
      }
      return;
    } else {
      // The chunk is full, another thread has just pushed a particle
      // at the end, and is now extending the bag.
      // First we have to cancel our additional "c->size++". This can be done
      // either with an "atomic c->size--", or just without atomics this way:
      c->size = CHUNK_SIZE;
      while (atomic_read_chunk(&b->front) == c) {
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
  int index = c->size;
  c->size++;
  // TRACE("Push bag %p, chunk size %d\n", b, c->size);
  c->items[index] = p;
  if (index == CHUNK_SIZE - 1) {
    // chunk is full, we extend the bag
    bag_add_front_chunk_serial(b);
    // TRACE("Extend bag %p\n", b);
  }
}

void bag_push(bag* b, particle p) {
  bag_push_serial(b, p);
}

/*
 * Swap the contents of two bags
 */
void bag_swap(bag* b1, bag* b2) {
  bag temp =*b1;
  *b1 = *b2;
  *b2 = temp;
}

// Return the next chunk in the chain, possibly NULL;
// Deallocate the argument [c] if [destructive] is true
chunk* chunk_next(chunk* c, bool destructive) {
  chunk* cnext = c->next;
  if (destructive) {
    bag_chunk_free(c);
  }
  return cnext;
}

// Free all the chunks in a bag
void bag_free(bag* b) {
  chunk* c = b->front;
  while (c != NULL) {
    c = chunk_next(c, true);
  }
}

//==========================================================================
// Iteration

void bag_iter_load_chunk(bag_iter* it, chunk* c) {
  it->iter_chunk = c;
  // if (c == NULL) { return; }   // technically optional as we assume all bags to have at least one chunk
  it->size = c->size;
  it->index = 0;
}

void bag_iter_skip_empty_chunks(bag_iter* it) {
  // Note: the first few chunks could be empty even if there are full chunks behind
  chunk* c0 = it->iter_chunk;
  chunk* c = c0;
  while (c != NULL && c->size == 0) {
    c = chunk_next(c, it->destructive);
  }
  if (c != c0) { // if there were some empty chunks to skip over
    if (c == NULL) {
      // iteration completed
      it->iter_chunk = NULL;
      it->size = 0;
      it->index = 0;
    } else {
      bag_iter_load_chunk(it, c);
    }
  }
  // here we have the guarantee that  if c->iter_chunk != NULL, then c->size > 0
}

void bag_iter_init(bag_iter* it, bag* b, bool destructive) {
  it->destructive = destructive;
  bag_iter_load_chunk(it, b->front);
  // note: this functiond  es not skip empty chunks
}

particle* bag_iter_get(bag_iter* it) {
  //TRACE("bag iter get at index %d of size %d in chunksize %d\n", it->index, it->size, it->iter_chunk->size);
  if (it->size == 0) {
    bag_iter_skip_empty_chunks(it); // works even if it->iter_chunk is already null
    if (it->iter_chunk == NULL || it->size == 0) { // iteration completed (the two conditions might be redundant)
      return NULL;
    }
  }
  // here we should have it->index < it->size
  return &(it->iter_chunk->items[it->index]);
}

chunk* bag_iter_get_chunk(bag_iter* it) {
  return it->iter_chunk;
}

particle* bag_iter_begin_common(bag_iter* it, bag* b, bool destructive) {
  bag_iter_init(it, b, destructive);
  return bag_iter_get(it);
}

particle* bag_iter_begin(bag_iter* it, bag* b) {
  return bag_iter_begin_common(it, b, false);
}

particle* bag_iter_destructive_begin(bag_iter* it, bag* b) {
  return bag_iter_begin_common(it, b, true);
}

// Return the next particle, or NULL if at the end
// (we cannot return one-past-the-end pointer because chunks are deallocated)
particle* bag_iter_next(bag_iter* it) {
  //TRACE("bag iter next from index %d in size %d\n", it->index, it->size);
  it->index++;
  if (it->index == it->size) {
    chunk* c = it->iter_chunk;
    chunk* cnext = chunk_next(c, it->destructive);
    if (cnext == NULL) {
      //TRACE("bag iter next reached the end\n");
      return NULL;
    }
    //TRACE("bag iter next jumps to chunk of size %d\n", cnext->size);
    bag_iter_load_chunk(it, cnext);
    //TRACE("bag iter next reset index to %d\n", it->index);
  }
  return bag_iter_get(it);
}


// example of a basic iteration over a bag
void bag_iter_ho_basic(bag* b, void body(particle*), bool destructive) {
  bag_iter it;
  for (particle* p = bag_iter_begin_common(&it, b, destructive); p != NULL; p = bag_iter_next(&it)) {
    body(p);
  }
}

// example of an iteration over a bag with the loop over the chunk items reveal_fielded
void bag_iter_ho_chunk(bag* b, void body(particle*), bool destructive) {
  for (chunk* c = b->front; c != NULL; c = chunk_next(c, destructive)) {
    const int nb = c->size;
    for (int i = 0; i < nb; i++) {
      particle* p = &c->items[i];
      body(p);
    }
  }
}

/* TEMPORARY LATER: remove
      // Perform a destructive iteration on that bag,
      // meaning that chunks are freed after traversal.
      chunk* c = b->front;
      while (true) { // loop on chunks
        int nb = c->size;
        // iterate over the items from the current chunk
        for (int i = 0; i < nb; i++) {
          ...
        }
        //----
        chunk* cnext = c->next;
        if (cnext != NULL) {
          // move to the next chunk, free the current chunk
          chunk_free(c);
          c = cnext; // beware that "c = c->next" would be illegal here, because c was freed
        } else {
          // finished the last chunk, clear the current chunk, clear the bag
          c->size = 0;
          b->front = c;
          b->back = c; // this write is redundant, but let's do it for clarity
          c->next = NULL; // this write is redundant, but let's do it for clarity
          break; // exit the loop on chunks
        }
*/


#endif