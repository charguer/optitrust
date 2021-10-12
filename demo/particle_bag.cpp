
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


// --------- Particle
// --------- Vector




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
typedef struct bag {
  chunk* front;
  chunk* back;
} bag;

// TODO: move above in this file into particle_chunk.h


//==========================================================================
// Manual memory management for chunks

// TODO: move this section into particle_chunk_alloc.h

// You need one free list per processor
unsigned int FREELIST_SIZE;
// free_index[thread_id][0] will store the number of free chunks available
// for thread number thread_id. Other subscripts are not used, but this is a
// matrix and not an array to avoid false sharing (each row is aligned
// on the cache line size, usually 64 bytes on modern Intel architectures).
#define FREE_INDEX(i) free_index[i][0]
int** free_index;
#ifdef PIC_VERT_TESTING
// Could also be optimised for false sharing, but it's only for testing so...
int* nb_malloc;
int* nb_free;
#endif

chunk*** free_chunks;    // Freelists
chunk** all_free_chunks; // Stores the free chunks during the initialization phase, before they are put into freelists
int number_of_spare_chunks_per_parity; // Number of chunks needed to initialize each bag in the append phase.
int** spare_chunks_ids;                // Bijection between [0 ; nb_bags_per_cell[ x [0 ; num_cells_3d[ and [0 ; number_of_spare_chunks_per_parity[
int num_threads;

// Cumulative sum of the freelist sizes.
// It is useless to allocate this array with only one cell per cache line,
// as all the threads will access all the cells, and not, like other arrays
// of this kind, only thread index thread_id will access array[thread_id].
int* cumulative_free_indexes;
int bag_last_spare_chunk_to_be_used;
#ifdef SPARE_LOC_OPTIMIZED
enum SPARE_CHUNK_LOCATION {
    SPARE_CHUNK_LOCATION_ID_THREAD,
    SPARE_CHUNK_LOCATION_OFFSET,
    SPARE_CHUNK_LOCATION_MAX_OFFSET,
    SPARE_CHUNK_LOCATION_NB_INFO // Always has to be last if you update this enum !
};
int** spare_chunk_location;
#endif

/*
 * Take a chunk from the freelist of thread_id. If the freelist is empty,
 * allocate a new one. This function should only be called during the particle
 * loop. During the append phase, the needed chunks should be found
 * in all the freelists, depending of the cumulative freelist sizes.
 *
 * @param[in] thread_id the index of the thread asking for a new chunk.
 * @return    a chunk almost ready to be filled with particles (set size to 0 before).
 */
chunk* manual_chunk_alloc(int thread_id) {
  if (FREE_INDEX(thread_id) > 0) {
    return free_chunks[thread_id][--FREE_INDEX(thread_id)];
  } else {
#ifdef PIC_VERT_TESTING
    nb_malloc[thread_id]++;
#endif
    return (chunk*) malloc(sizeof(chunk));
  }
}

/*
 * Put a chunk back into the freelist of thread_id. This function is only
 * called during the particle loop, on a chunk that has just been iterated on.
 * If that list is full, free the chunk.
 *
 * @param[in] c         the chunk to be put back in the freelist.
 * @param[in] thread_id the index of the thread asking for this release.
 */
void manual_chunk_free(chunk* c, int thread_id) {
  if (FREE_INDEX(thread_id) < FREELIST_SIZE) {
    free_chunks[thread_id][FREE_INDEX(thread_id)++] = c;
  } else {
#ifdef PIC_VERT_TESTING
    nb_free[thread_id]++;
#endif
    free(c);
  }
}

#ifdef SPARE_LOC_OPTIMIZED
/*
 * Locate where in the freelists is the chunk that should be put on the
 * cell id_cell (in [0 ; ncx * ncy * ncz[), in the bag id_bag (in [0 ; 9[)
 * for the next iteration, by the thread thread_id.
 *
 * The location is stored into spare_chunk_location[thread_id].
 * The first number (in SPARE_CHUNK_LOCATION_ID_THREAD) tells in the freelist
 * of which thread this chunk should be found.
 * The second number (in SPARE_CHUNK_LOCATION_OFFSET) tells at which position
 * in that freelist this chunk should be found.
 * The last number (SPARE_CHUNK_LOCATION_MAX_OFFSET) tells at which index in
 * this freelist you should re-call this function, because you are reaching
 * the end of the freelist. As long as you do not reach this number, you can
 * locate the next chunk in the same freelist, by just increasing the second
 * number (in SPARE_CHUNK_LOCATION_OFFSET).
 *
 * @param[in] id_bag    the index of the bag (in [0 ; 9[) in which to put a chunk.
 * @param[in] id_cell   the index of the cell (in [0 ; ncx * ncy * ncz[) in which
 *                      to put a chunk.
 * @param[in] thread_id the index of the thread that asks for a chunk.
 */
void locate_spare_chunk(int id_bag, int id_cell, int thread_id) {
  int id_chunk = spare_chunks_ids[id_bag][id_cell];
  int k;
  for (k = num_threads - 1; k >= 0; k--)
    if (cumulative_free_indexes[k] <= id_chunk)
      break;
  int index = id_chunk - cumulative_free_indexes[k];
  if (index < 0 || index >= FREE_INDEX(k)) {
    printf("Not enough free chunks in thread %d !\n", k);
    printf("Maybe did you forgot to call compute_cumulative_free_list_sizes and/or update_free_list_sizes ?\n");
    exit(EXIT_FAILURE);
  }
  spare_chunk_location[thread_id][SPARE_CHUNK_LOCATION_ID_THREAD ] = k;
  spare_chunk_location[thread_id][SPARE_CHUNK_LOCATION_OFFSET    ] = (k == bag_last_spare_chunk_to_be_used)
    ? index + FREE_INDEX(k) - (number_of_spare_chunks_per_parity - cumulative_free_indexes[k])
    : index;
  spare_chunk_location[thread_id][SPARE_CHUNK_LOCATION_MAX_OFFSET] = FREE_INDEX(k);
}
#endif

// In the initial phase, instantiate the argument thread_id with the value THREAD_INITIAL
// to assume that all free chunks are stored in the free list of processor THREAD_ZERO.
const int THREAD_INITIAL = -1;
const int THREAD_ZERO = 0;

chunk* manual_obtain_chunk_initial() {
  // We are never too careful (-:
  if (FREE_INDEX(THREAD_ZERO) < 1) {
    fprintf(stderr, "Not enough chunks in all_free_chunks. Check its allocation.\n");
    exit(EXIT_FAILURE);
  }
  return all_free_chunks[--FREE_INDEX(THREAD_ZERO)];
}

/*
 * Obtain a chunk from the free list; used by bag_init (i.e. outside of the particle loop)
 * ARTHUR: document this function
 */
chunk* manual_obtain_chunk(int id_bag, int id_cell, int thread_id) {
  if (thread_id == THREAD_INITIAL) {
    return manual_obtain_chunk_initial();
  }
#ifdef SPARE_LOC_OPTIMIZED
  if (spare_chunk_location[thread_id][SPARE_CHUNK_LOCATION_OFFSET] >= spare_chunk_location[thread_id][SPARE_CHUNK_LOCATION_MAX_OFFSET])
    locate_spare_chunk(id_bag, id_cell, thread_id);
  chunk* c = free_chunks[spare_chunk_location[thread_id][SPARE_CHUNK_LOCATION_ID_THREAD]]
                        [spare_chunk_location[thread_id][SPARE_CHUNK_LOCATION_OFFSET]++];
#else
  int id_chunk = spare_chunks_ids[id_bag][id_cell];
  int k;
  for (k = num_threads - 1; k >= 0; k--)
    if (cumulative_free_indexes[k] <= id_chunk)
      break;
  int offset = id_chunk - cumulative_free_indexes[k];
  if (offset < 0 || offset >= FREE_INDEX(k)) {
    printf("Not enough free chunks in thread %d !\n", k);
    printf("Maybe did you forgot to call compute_cumulative_free_list_sizes and/or update_free_list_sizes ?\n");
    exit(EXIT_FAILURE);
  }
  chunk* c = free_chunks[k][FREE_INDEX(k) - 1 - offset];
#endif
  return c;
}

/*
 * Compute the cumulative sum of the freelist sizes. This cumulative sum will
 * be used to locate needed chunks in the append phase. It should therefore
 * be called before the append phase.
 *
 * WARNING : This function is not thread-safe, and should be called only by
 * the master thread.
 *
 * Reminder : cumulative_free_indexes[0] = 0
 *            cumulative_free_indexes[i] = sum(j = 0; j < i) FREE_INDEX(j).
 */
void compute_cumulative_free_list_sizes() {
  int k;
  cumulative_free_indexes[0] = 0;
  for (k = 1; k < num_threads; k++)
    cumulative_free_indexes[k] = cumulative_free_indexes[k - 1] + FREE_INDEX(k - 1);
  int nb_free_chunks = cumulative_free_indexes[num_threads - 1] + FREE_INDEX(num_threads - 1);
  // We are never too careful :)
  if (nb_free_chunks < number_of_spare_chunks_per_parity) {
    int nb_chunks_to_allocate = number_of_spare_chunks_per_parity - nb_free_chunks;
    printf("Not enough free chunks in the free lists ! We must malloc %d chunks.\n", nb_chunks_to_allocate);
    // Allocates new free chunks, all in the freelist of thread 0.
    // Remember that FREELIST_SIZE = 2 * number_of_spare_chunks_per_parity, so
    // there is enough indexes in that list to put all the new chunks here.
    int nb_allocated_chunks = 0;
    while (nb_allocated_chunks < nb_chunks_to_allocate) {
      free_chunks[THREAD_ZERO][FREE_INDEX(THREAD_ZERO)++] = (chunk*)malloc(sizeof(chunk));
      nb_allocated_chunks++;
#ifdef PIC_VERT_TESTING
      nb_malloc[THREAD_ZERO]++;
#endif
    }
    compute_cumulative_free_list_sizes();
  }
#ifdef SPARE_LOC_OPTIMIZED
  for (k = 0; k < num_threads; k++) {
    spare_chunk_location[k][SPARE_CHUNK_LOCATION_ID_THREAD ] = 0;
    spare_chunk_location[k][SPARE_CHUNK_LOCATION_OFFSET    ] = 0;
    spare_chunk_location[k][SPARE_CHUNK_LOCATION_MAX_OFFSET] = 0;
  }
#endif
  for (k = num_threads - 1; k >= 0; k--)
    if (cumulative_free_indexes[k] <= number_of_spare_chunks_per_parity) {
      bag_last_spare_chunk_to_be_used = k;
      return;
  }
}

/*
 * Update the freelist sizes after the append phase, according to the number
 * of chunks that were taken in each freelist. It should therefore
 * be called after the append phase.
 *
 * WARNING : This function is not thread-safe, and should be called only by
 * the master thread.
 */
void update_free_list_sizes() {
  for (int i = 0; i < bag_last_spare_chunk_to_be_used; i++)
    FREE_INDEX(i) = 0;
  FREE_INDEX(bag_last_spare_chunk_to_be_used) -= number_of_spare_chunks_per_parity - cumulative_free_indexes[bag_last_spare_chunk_to_be_used];
}



//==========================================================================
// Naive chunk allocation operations

chunk* chunk_alloc() {
  return (chunk*) malloc(sizeof(chunk));
}

chunk* obtain_chunk() {
  return chunk_alloc();
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
  chunk* c = obtain_chunk(id_bag, id_cell);
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
void add_front_chunk(bag* b) {
  chunk* c = chunk_alloc();
  // Warning - TODO: the instruction c->size=0 might be viewd switched with b->front=c by other threads, which would lead to non-valid code.
  // (e.g. on PowerPC, the present code is valid on Intel).
  // Solution: adding a memory fence (putting write c->size=0 when freeing a chunk, and not when adding it is not enough).
  c->size = 0;
  c->next = b->front;
  #pragma omp atomic write
  b->front = c;
}


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
        // Inside add_front_chunk, the update of the b->front
        // pointer is made atomic so that other threads see the update.
        add_front_chunk(b);
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
    add_front_chunk(b);
  }
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

// Higher-order iteration, destructive version

void bag_destructive_iter(bag* b, void f(particle*)) {
  for (chunk* c = b->front; c != NULL; c = c->next) {
    int nb = c->size;
    for (int i = 0; i < nb; i++) {
      particle* cur_p = &c->items[i];
      f(cur_p);
    }
  }
}

// First-order iterator
typedef struct {
  chunk* chunk;
  int size;
  int index;
} bag_iter;

void bag_iter_load_chunk(bag_iter* it, chunk* c) {
  it->chunk = c;
  if (c != NULL) {
    it->size = c->size;
    it->index = 0;
  }
}

particle* bag_iter_current(bag_iter* it) {
  return &it->chunk[it->index]
}

void bag_iter_init(bag_iter* it, bag* b) {
  bag_iter_load_chunk(it, b->front);
}

bool bag_iter_finished(bag_iter* it) {
  return it->chunk == NULL;
}

particle* bag_iter_next_destructive(bag_iter* it) {
  int i = it->index;
  it->index++;
  if (it->index == it->size) {
    chunk* c = it->chunk;
    bag_iter_load_chunk(it, c->next);
    chunk_free(c); // because destructive iteration
    if (bag_iter_finished(it)) {
      return NULL;
    }
  }
  return bag_iter_current(it);
}


//==========================================================================
// Initial, sequential manipulation of bags

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

// ARTHUR: document better why we have alloc_chunk and obtain_chunk
// ARTHUR: simplify init_all_chunks and  init_freelists
// ARTHUR: document what is number_of_spare_chunks_per_parity
// ARTHUR: cumulative_nb_chunks_per_cell -> would be nicer to have one extra cell