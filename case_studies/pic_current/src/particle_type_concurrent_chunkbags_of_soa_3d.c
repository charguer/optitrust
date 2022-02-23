#include <stdio.h>                                        // function  fprintf (output strings on a stream)
                                                          // constant  stderr (standard error output stream)
#include <stdlib.h>                                       // functions exit (error handling)
                                                          // constant  EXIT_FAILURE (error handling)
                                                          // type      size_t
#include <stdbool.h>                                      // type      bool
                                                          // constant  true
#include <omp.h>                                          // function  omp_get_num_threads
#include "initial_distributions.h"                        // types     speeds_generator_3d, distribution_function_3d, max_distribution_function
                                                          // variables speed_generators_3d, distribution_funs_3d, distribution_maxs_3d
#include "math_functions.h"                               // functions ceiling, is_even
#include "matrix_functions.h"                             // functions allocate_int_matrix, allocate_aligned_int_matrix
#include "meshes.h"                                       // type      cartesian_mesh_3d
#include "space_filling_curves.h"                         // macros    COMPUTE_I_CELL_3D, I_CELL_PARAM1_3D, I_CELL_PARAM2_3D
#include "particle_type_concurrent_chunkbags_of_soa_3d.h" // types     chunk, bag
                                                          // variable  free_index, FREELIST_SIZE
#include "random.h"                                       // function  pic_vert_next_random_double

/*****************************************************************************
 *                              Chunk bags                                   *
 *****************************************************************************/

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
chunk* chunk_alloc(int thread_id) {
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
void chunk_free(chunk* c, int thread_id) {
  if (FREE_INDEX(thread_id) < FREELIST_SIZE) {
    free_chunks[thread_id][FREE_INDEX(thread_id)++] = c;
  } else {
#ifdef PIC_VERT_TESTING
    nb_free[thread_id]++;
#endif
    free(c);
  }
}

/*
 * Allocate a new chunk and put it at the front of a chunk bag.
 * We have to set the field "size" to 0, because:
 *   1. When a chunk goes back to a freelist, this field is not reset.
 *   2. When a chunk is newly allocated, this field is not set neither.
 * It is simpler to set to 0 in this lone function, rather than
 * both in the allocation and in the free functions.
 *
 * @param[in, out] b the bag in which to put the new chunk.
 */
void add_front_chunk(bag* b, int thread_id) {
  chunk* c = chunk_alloc(thread_id);
  // Warning - TODO: the instruction c->size=0 might be viewd switched with b->front=c by other threads, which would lead to non-valid code.
  // (e.g. on PowerPC, the present code is valid on Intel).
  // Solution: adding a memory fence (putting write c->size=0 when freeing a chunk, and not when adding it is not enough).
  c->size = 0;
  c->next = b->front;
  #pragma omp atomic write
  b->front = c;
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
      free_chunks[0][FREE_INDEX(0)++] = malloc(sizeof(chunk));
      nb_allocated_chunks++;
#ifdef PIC_VERT_TESTING
      nb_malloc[0]++;
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
void bag_init(bag* b, int id_bag, int id_cell, int thread_id) {
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
  c->size = 0;
  c->next = (void*)0;
  b->front = c;
  b->back  = c;
}

/*
 * Merge other into b; other is re-initialized if not null,
 * and stays null if it was already null.
 *
 * @param[in, out] b
 * @param[in, out] other
 */
void bag_append(bag* b, bag* other, int id_bag, int id_cell, int thread_id) {
  if (other->front) {
    b->back->next = other->front;
    b->back       = other->back;
    bag_init(other, id_bag, id_cell, thread_id);
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
  b->front = (void*)0;
  b->back  = (void*)0;
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
 * @param[in]      dx, dy, dz, vx, vy, vz
 */
void bag_push_concurrent(bag* b, float dx, float dy, float dz, double vx, double vy, double vz, CHECKER_ONLY_COMMA(int id) int thread_id) {
  chunk* c;
  int index;
  while (true) { // Until success.
    c = b->front;

    #pragma omp atomic capture
    index = c->size++;

    if (index < CHUNK_SIZE) {
      // The chunk is not full, we can write the particle.
      c->dx[index] = dx;
      c->dy[index] = dy;
      c->dz[index] = dz;
      c->vx[index] = vx;
      c->vy[index] = vy;
      c->vz[index] = vz;
      CHECKER_ONLY(c->id[index] = id;)
      if (index == CHUNK_SIZE - 1) {
        // The chunk is now full, we have to extend the bag.
        // Inside add_front_chunk, the update of the b->front
        // pointer is made atomic so that other threads see the update.
        add_front_chunk(b, thread_id);
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
 * @param[in]      dx, dy, dz, vx, vy, vz
 */
void bag_push_serial(bag* b, float dx, float dy, float dz, double vx, double vy, double vz, CHECKER_ONLY_COMMA(int id) int thread_id) {
  chunk* c = b->front;
  int index = c->size++;
  c->dx[index] = dx;
  c->dy[index] = dy;
  c->dz[index] = dz;
  c->vx[index] = vx;
  c->vy[index] = vy;
  c->vz[index] = vz;
  CHECKER_ONLY(c->id[index] = id;)
  if (index == CHUNK_SIZE - 1) {
    // chunk is now full, we have to extend the bag
    add_front_chunk(b, thread_id);
  }
}

/*
 * The following three functions do the same thing as their counterparts
 * without "_initial" in their names, except here they are used only in
 * the initialization of the particles, and not in the rest of the simulation.
 * All the free chunks are in only one list. At the end of the initialization,
 * they have to be split into one free list per thread.
 */
void add_front_chunk_initial(bag* b) {
  // We are never too careful (-:
  if (FREE_INDEX(0) < 1) {
    fprintf(stderr, "Not enough chunks in all_free_chunks. Check its allocation.\n");
    exit(EXIT_FAILURE);
  }
  chunk* c = all_free_chunks[--FREE_INDEX(0)];
  c->size = 0;
  c->next = b->front;
  b->front = c;
}
void bag_init_initial(bag* b) {
  // Initialize b->front to null before because add_front_chunk_initial
  // uses b->front to set the front chunk's next field.
  b->front = (void*)0;
  add_front_chunk_initial(b);
  b->back = b->front;
}
void bag_push_initial(bag* b, CHECKER_ONLY_COMMA(int id) float dx, float dy, float dz, double vx, double vy, double vz) {
  chunk* c = b->front;
  int index = c->size++;
  c->dx[index] = dx;
  c->dy[index] = dy;
  c->dz[index] = dz;
  c->vx[index] = vx;
  c->vy[index] = vy;
  c->vz[index] = vz;
  CHECKER_ONLY(c->id[index] = id;)
  if (index == CHUNK_SIZE - 1)
    add_front_chunk_initial(b);
}

/*
 * In case of non-perfect tiling, computes the border_size for the contour tiles.
 * A tiling is perfect when in each direction, the number of cells is an even multiple of tile_size.
 * Otherwise, the tiles with same parity from each side in a direction might have overlapping borders.
 * e.g. (tile indexes are given in each tile of size 5 x 5, the full domain is size 32 x 16)
 *
 *         y-axis
 *            ^
 *            |
 *         16 |———————+———————+———————+———————+———————+———————+——+
 *            | (0,3) | (1,3) | (2,3) | (3,3) | (4,3) | (5,3) |63|
 *         15 |———————+———————+———————+———————+———————+———————+——+
 *            |       |       |       |       |       |       |6 |
 *            | (0,2) | (1,2) | (2,2) | (3,2) | (4,2) | (5,2) |, |
 *            |       |       |       |       |       |       |2 |
 *         10 |———————+———————+———————+———————+———————+———————+——+
 *            |       |       |       |       |       |       |6 |
 *            | (0,1) | (1,1) | (2,1) | (3,1) | (4,1) | (5,1) |, |
 *            |       |       |       |       |       |       |1 |
 *          5 |———————+———————+———————+———————+———————+———————+——+
 *            |       |       |       |       |       |       |6 |
 *            | (0,0) | (1,0) | (2,0) | (3,0) | (4,0) | (5,0) |, |
 *            |       |       |       |       |       |       |0 |
 *          0 +———————+———————+———————+———————+———————+———————+——+———> x-axis
 *            0       5       10     15      20      25      30  32
 *                    <———————>
 *                    tile_size
 *
 * As can be seen on the picture, the tiling is not perfect, neither in x nor in the y direction.
 *
 * If the border_size is 0, there is no problem.
 *
 * Starting from border_size 1, there is a problem in the x direction: for each Y, the tile
 * (0,Y) and (6,Y) have both even indexes, so they store private data in the same bag, thus
 * conflicts can happen between the right border of (6,Y) and the inside of tile (0,Y) or
 * between the left border of (0,Y) and the inside of tile (6,Y).
 *
 * Starting from border_size 1, there is a problem in the y direction: for each X, the tile
 * (X,0) and (X,2) have both even indexes, so they store private data in the same bag, thus
 * conflicts can happen between the upper border of (X,2) and the lower border of tile (X,0)
 * (they are each exactly equal to the tile (X,3) for border_size 1, and are bigger thus will
 * also overlap with the inside of the tiles for border_size 2 - bigger border_size is not
 * allowed because it causes conflicts with normal tiles when border_size > tile_size / 2
 * for the same reasons).
 *
 * Notice that it is not always the last tile that causes problem (there is no problem with
 * tiles (X,3) on the y direction), it depends on parity - because that is how we manage
 * private bags.
 *
 * Thus, in such cases, the cell index at the beginning of the tile than can overlap with the
 * tile that starts at cell index 0 is stored, so that the corresponding borders are set to 0
 * to avoid overlapping and thus race conditions. Here, we will store
 *     start_tile_without_borders_x = 30
 *     start_tile_without_borders_y = 10
 */
bool is_perfect_tiling_x;
bool is_perfect_tiling_y;
bool is_perfect_tiling_z;
int start_tile_without_borders_x;
int start_tile_without_borders_y;
int start_tile_without_borders_z;

/*
 * Sets the x borders of the tile that starts at cell index ix_min.
 * The default scenario is normal_border_size, but if the tiling is not perfect,
 * we must take care of the contour tiles.
 *
 * @param[in]  ix_min the cell index at the beginning of the current tile.
 * @param[in]  normal_border_size the border size for usual tiles.
 * @param[out] lower_x_border the lower border size for the current tile.
 * @param[out] upper_x_border the upper border size for the current tile.
 */
void set_borders_x(int ix_min, int normal_border_size, int* lower_x_border, int* upper_x_border) {
  *lower_x_border = is_perfect_tiling_x
    ? normal_border_size
    : (ix_min == 0) ? 0 : normal_border_size;
  *upper_x_border = is_perfect_tiling_x
    ? normal_border_size
    : (ix_min == start_tile_without_borders_x) ? 0 : normal_border_size;
}

/*
 * Sets the y borders of the tile that starts at cell index iy_min.
 * The default scenario is normal_border_size, but if the tiling is not perfect,
 * we must take care of the contour tiles.
 *
 * @param[in]  iy_min the cell index at the beginning of the current tile.
 * @param[in]  normal_border_size the border size for usual tiles.
 * @param[out] lower_y_border the lower border size for the current tile.
 * @param[out] upper_y_border the upper border size for the current tile.
 */
void set_borders_y(int iy_min, int normal_border_size, int* lower_y_border, int* upper_y_border) {
  *lower_y_border = is_perfect_tiling_y
    ? normal_border_size
    : (iy_min == 0) ? 0 : normal_border_size;
  *upper_y_border = is_perfect_tiling_y
    ? normal_border_size
    : (iy_min == start_tile_without_borders_y) ? 0 : normal_border_size;
}

/*
 * Sets the z borders of the tile that starts at cell index iz_min.
 * The default scenario is normal_border_size, but if the tiling is not perfect,
 * we must take care of the contour tiles.
 *
 * @param[in]  iz_min the cell index at the beginning of the current tile.
 * @param[in]  normal_border_size the border size for usual tiles.
 * @param[out] lower_z_border the lower border size for the current tile.
 * @param[out] upper_z_border the upper border size for the current tile.
 */
void set_borders_z(int iz_min, int normal_border_size, int* lower_z_border, int* upper_z_border) {
  *lower_z_border = is_perfect_tiling_z
    ? normal_border_size
    : (iz_min == 0) ? 0 : normal_border_size;
  *upper_z_border = is_perfect_tiling_z
    ? normal_border_size
    : (iz_min == start_tile_without_borders_z) ? 0 : normal_border_size;
}

/*
 * Does the full allocation of all the chunks needed for the simulation in one pass.
 * Threads will iterate particles tile by tile. For each tile, the thread that iterates
 * over it needs to have private bags for the tile + borders (to avoid numerous atomic
 * operations, as particles will frequently move outside the tile).
 * Also initializes particlesNext with empty chunks where needed (and null bags otherwise).
 *
 * But we do not need to have as many private bags per cell as there are threads. We
 * can have only 8 private bags (in 3d) per cell, as long as we ensure that in each
 * direction, 2 * border_size <= tile_size. Then we can look at, in each direction,
 * the parity of the index of the tile to know in which private bag to work.
 *
 * To know how many private bags we will really use (we won't use 8 per cell each time),
 * there are two equivalent ways of doing the computation. In the following, we
 * denote by c = tile_size and b = border_size.
 *   1. Each tile will need (c + 2 * b)**3 private bags. Thus, in total,
 *      we need a number of private bags equal to :
 *      (c + 2 * b)**3 / (c**3) * nb_cells.
 *   2. Each cell will have a number of private bags depending of its position:
 *      corners         ->  8 *                  b**3 cells, each 8 private bags
 *      (rest of) edges -> 12 * (c - 2 * b)    * b**2 cells, each 4 private bags
 *      (rest of) faces ->  6 * (c - 2 * b)**2 * b    cells, each 2 private bags
 *      interior        ->  1 * (c - 2 * b)**3        cells, each 1 private bag
 *
 * WARNING : We make the assumption that:
 *   1. in the case of a non-colored algorithm, the id of the shared bag is 8
 * (= nb_bags_per_cell - 1), and that for each tile  beginning at
 * (ix_min, iy_min, iz_min), the id of the corresponding private bag is computed by:
 * ((ix_min % (2 * tile_size)) != 0) + 2 * ((iy_min % (2 * tile_size)) != 0) + 4 * ((iz_min % (2 * tile_size)) != 0)
 *   2. in the case of a colored algorithm, the id of the shared bag is 1,
 * and the id of the private bag is 0.
 * In both cases, if this is not the case, you have to modify it.
 *
 * Remark : we could solve this with a macro, but this would mean that we would have to pass
 * yet another parameter in command line to know whether we are in color or non-color algorithm,
 * whereas the core of the code would not be easy to read if written with preprocessor instructions.
 *
 * @param[in] nb_bags_per_cell  the number of bags per cells (2 when using coloring scheme, 9 otherwise).
 * @param[in] num_particles     the total number of particles in this MPI process.
 * @param[in] mesh              the physical mesh we're working on.
 * @param[in] tile_size         the size of an edge of a cubic tile.
 * @param[in] border_size       the size of the borders allowed in a private bag (= tile_size / 2
 *                              when using coloring scheme, <= tile_size / 2 otherwise).
 * @param[in,out] particlesNext
 */
void init_all_chunks(int nb_bags_per_cell, unsigned int num_particle, cartesian_mesh_3d mesh,
    int tile_size, int border_size, bag*** particlesNext) {
  const int ncx = mesh.num_cell_x;
  const int ncy = mesh.num_cell_y;
  const int ncz = mesh.num_cell_z;
  const int id_shared_bag = nb_bags_per_cell - 1;
  const int num_cells_3d = ncx * ncy * ncz;
  const int icell_param1 = I_CELL_PARAM1_3D(ncx, ncy, ncz);
  const int icell_param2 = I_CELL_PARAM2_3D(ncx, ncy, ncz);
  const int ncxminusone = ncx - 1;
  const int ncyminusone = ncy - 1;
  const int nczminusone = ncz - 1;
  int id_private_bag, i, j;
  int ix_min, iy_min, iz_min, ix_max, iy_max, iz_max, ix, iy, iz, i_cell;
  int nb_chunks_per_cell[num_cells_3d];
  int cumulative_nb_chunks_per_cell[num_cells_3d];
  #pragma omp parallel
  num_threads = omp_get_num_threads();

  // Testing that the tiling is appropriate.
  if (nb_bags_per_cell == 2) {
    if (border_size != (tile_size / 2)) {
      fprintf(stderr, "Within the coloring scheme, the borders (%d) should be equal to half the tile size (%d).\n", border_size, tile_size);
      exit(EXIT_FAILURE);
    }
  } else {
    if (border_size > (tile_size / 2)) {
      fprintf(stderr, "The borders (%d) should be at most equal to half the tile size (%d).\n", border_size, tile_size);
      exit(EXIT_FAILURE);
    }
  }

  // Handling of non-perfect tiling. See explanations a few lines before.
  int nb_full_tiles_x, nb_full_tiles_y, nb_full_tiles_z;
  int remaining_cells_x, remaining_cells_y, remaining_cells_z;
  remaining_cells_x = ncx % tile_size;
  remaining_cells_y = ncy % tile_size;
  remaining_cells_z = ncz % tile_size;
  nb_full_tiles_x = ncx / tile_size;
  nb_full_tiles_y = ncy / tile_size;
  nb_full_tiles_z = ncz / tile_size;
  is_perfect_tiling_x = (remaining_cells_x == 0) && is_even(nb_full_tiles_x);
  is_perfect_tiling_y = (remaining_cells_y == 0) && is_even(nb_full_tiles_y);
  is_perfect_tiling_z = (remaining_cells_z == 0) && is_even(nb_full_tiles_z);
  start_tile_without_borders_x = is_even(nb_full_tiles_x)
      ? (nb_full_tiles_x    ) * tile_size  // even number of full tiles: the tile that can overlap is            the last one (x-axis on the picture)
      : (nb_full_tiles_x - 1) * tile_size; //  odd number of full tiles: the tile that can overlap is one before the last one (y-axis on the picture)
  start_tile_without_borders_y = is_even(nb_full_tiles_y)
      ? (nb_full_tiles_y    ) * tile_size  // even number of full tiles: the tile that can overlap is            the last one (x-axis on the picture)
      : (nb_full_tiles_y - 1) * tile_size; //  odd number of full tiles: the tile that can overlap is one before the last one (y-axis on the picture)
  start_tile_without_borders_z = is_even(nb_full_tiles_z)
      ? (nb_full_tiles_z    ) * tile_size  // even number of full tiles: the tile that can overlap is            the last one (x-axis on the picture)
      : (nb_full_tiles_z - 1) * tile_size; //  odd number of full tiles: the tile that can overlap is one before the last one (y-axis on the picture)

  // Computation of the number of spare chunks to put in each cell, and the total number of spare chunks.
  // Also tells which bags will hold spare chunks, by temporarily putting a 0 inside.
  spare_chunks_ids = allocate_int_matrix(nb_bags_per_cell, num_cells_3d);
  for (j = 0; j < nb_bags_per_cell; j++)
    for (i = 0; i < num_cells_3d; i++)
      spare_chunks_ids[j][i] = -1;
  // *** shared part
  for (i = 0; i < num_cells_3d; i++) {
    nb_chunks_per_cell[i] = 1;
    spare_chunks_ids[id_shared_bag][i] = 0;
  }
  // *** private part
  if (nb_bags_per_cell == 2) {
    id_private_bag = 0;
    for (i = 0; i < num_cells_3d; i++) {
      nb_chunks_per_cell[i]++;
      spare_chunks_ids[id_private_bag][i] = 0;
    }
  } else {
    int lower_x_border, lower_y_border, lower_z_border;
    int upper_x_border, upper_y_border, upper_z_border;
    for (ix_min = 0; ix_min < ncx; ix_min += tile_size) {
    set_borders_x(ix_min, border_size, &lower_x_border, &upper_x_border);
    for (iy_min = 0; iy_min < ncy; iy_min += tile_size) {
    set_borders_y(iy_min, border_size, &lower_y_border, &upper_y_border);
    for (iz_min = 0; iz_min < ncz; iz_min += tile_size) {
    set_borders_z(iz_min, border_size, &lower_z_border, &upper_z_border);
      ix_max = min(ix_min + tile_size - 1, ncxminusone);
      iy_max = min(iy_min + tile_size - 1, ncyminusone);
      iz_max = min(iz_min + tile_size - 1, nczminusone);
      id_private_bag = ((ix_min % (2 * tile_size)) != 0) + 2 * ((iy_min % (2 * tile_size)) != 0) + 4 * ((iz_min % (2 * tile_size)) != 0);
      // Nested loops on the cells of the tile +/- borders.
      for (ix = ix_min - lower_x_border; ix <= ix_max + upper_x_border; ix++) {
      for (iy = iy_min - lower_y_border; iy <= iy_max + upper_y_border; iy++) {
      for (iz = iz_min - lower_z_border; iz <= iz_max + upper_z_border; iz++) {
        i_cell = COMPUTE_I_CELL_3D(icell_param1, icell_param2, ix & ncxminusone, iy & ncyminusone, iz & nczminusone);
        nb_chunks_per_cell[i_cell]++;
        spare_chunks_ids[id_private_bag][i_cell] = 0;
      }}}
    }}}
  }

  // We are never too careful (-:
  for (i = 0; i < num_cells_3d; i++)
    if (nb_chunks_per_cell[i] > nb_bags_per_cell)
      fprintf(stderr, "Something went wrong with the computation of the bags ; edit init_all_chunks in %s.\n", __FILE__);

  // Computation of the cumulative sum.
  cumulative_nb_chunks_per_cell[0] = 0;
  for (i = 1; i < num_cells_3d; i++)
    cumulative_nb_chunks_per_cell[i] = cumulative_nb_chunks_per_cell[i - 1] + nb_chunks_per_cell[i - 1];

  // Total number of spare chunks per parity.
  number_of_spare_chunks_per_parity = cumulative_nb_chunks_per_cell[num_cells_3d - 1] + nb_chunks_per_cell[num_cells_3d - 1];

  // Computation of bijection between [0 ; nb_bags_per_cell[ x [0 ; num_cells_3d[ and [0 ; number_of_spare_chunks_per_parity[
  // For this bijection, we must have that, for each two distinct chunks A and B:
  //   (idCell(A) < idCell(B))                                                       => (idChunk(A) < idChunk(B))
  //   ((idCell(A) = idCell(B)) && (idBag(A) == id_shared_bag))                      => (idChunk(A) < idChunk(B))
  //   ((idCell(A) = idCell(B)) && (idBag(B) == id_shared_bag))                      => (idChunk(A) > idChunk(B))
  //   ((idCell(A) = idCell(B)) && (A and B != shared bag) && (idBag(A) > idBag(B))) => (idChunk(A) > idChunk(B))
  // It means that bags are ordered by cell (increasing order), and inside each cell,
  // they are ordered by bag id (increasing order, except the shared bag which is first).
  // This is an assumption made to gain performance at the end of the particle loop,
  // when assigning free chunks in particlesNext.
  for (i = 0; i < num_cells_3d; i++) {
    spare_chunks_ids[id_shared_bag][i] = cumulative_nb_chunks_per_cell[i]++;
    for (j = 0; j < id_shared_bag; j++)
      if (spare_chunks_ids[j][i] != -1)
        spare_chunks_ids[j][i] = cumulative_nb_chunks_per_cell[i]++;
  }

  // Worse-case scenario: on one given iteration, the particles are stored in full chunks,
  // thus there is one empty chunk per bucket.
  // Then we must add one spare chunk per bucket for the next iteration.
  int total_nb_chunks = ceiling(num_particle, CHUNK_SIZE) + 2 * number_of_spare_chunks_per_parity;
  all_free_chunks = malloc(total_nb_chunks * sizeof(chunk*));
  if (!all_free_chunks) {
    fprintf(stderr, "init_all_chunks : malloc error for all_free_chunks (size %d).\n", total_nb_chunks);
    exit(EXIT_FAILURE);
  }
  for (i = 0; i < total_nb_chunks; i++) {
    // For an unknown reason, the chunks have to be allocated aligned (despite the __attribute__((aligned(VEC_ALIGN)))
    // in the type declaration), else it causes a segfault because of unaligned memory if compiled with gcc.
    if (posix_memalign((void**)&(all_free_chunks[i]), VEC_ALIGN, sizeof(chunk))) {
        fprintf(stderr, "init_all_chunks: posix_memalign failed to initialize all_free_chunks[%d].\n", i);
        exit(EXIT_FAILURE);
    }
  }

  // Temporarily stores the number of free chunks inside the free_index of thread 0.
  free_index = allocate_aligned_int_matrix(num_threads, 1);
  FREE_INDEX(0) = total_nb_chunks;

  // Filling of particlesNext.
  for (i = 0; i < num_cells_3d; i++)
    for (j = 0; j < nb_bags_per_cell; j++)
      if (spare_chunks_ids[j][i] != -1)
        bag_init_initial(&((*particlesNext)[j][i]));
      else
        bag_nullify(&((*particlesNext)[j][i]));

#ifdef SPARE_LOC_OPTIMIZED
  // To reduce the computation needed during the append phase.
  spare_chunk_location = allocate_aligned_int_matrix(num_threads, SPARE_CHUNK_LOCATION_NB_INFO);
#endif
  cumulative_free_indexes = allocate_int_array(num_threads);
#ifdef PIC_VERT_TESTING
  nb_malloc = allocate_int_array(num_threads);
  nb_free   = allocate_int_array(num_threads);
  for (i = 0; i < num_threads; i++) {
    nb_malloc[i] = 0;
    nb_free[i]   = 0;
  }
#endif
}

/*
 * Take the free chunks remaining in all_free_chunks, and put them into
 * the free lists, in a round-robin fashion.
 *
 * Rermark : We could as well put them all inside only one freelist, this would
 * not make any difference.
 */
void init_freelists() {
  int i;
  // We temporarily stored the number of free chunks inside the free_index of thread 0.
  int nb_free_chunks = FREE_INDEX(0);
  // Worse-case scenario: on one given iteration, one thread receives all the chunks
  // to be freed, while it already had all the spare chunks needed for next iteration.
  FREELIST_SIZE = 2 * number_of_spare_chunks_per_parity;
  free_chunks = malloc(num_threads * sizeof(chunk**));
  for (i = 0; i < num_threads; i++) {
    // The freelists could be aligned to the cache size, but this doesn't matter:
    // their size ensure that there won't be any false sharing.
    free_chunks[i] = malloc(FREELIST_SIZE * sizeof(chunk*));
    FREE_INDEX(i) = 0;
  }
  // Distributes the free chunks to all the freelists, in a round-robin fashion.
  int proc_id = 0;
  for (i = 0; i < nb_free_chunks; i++) {
    free_chunks[proc_id][FREE_INDEX(proc_id)++] = all_free_chunks[i];
    proc_id = (proc_id + 1) % num_threads;
  }
  // We don't need all_free_chunks anymore, the rest of the code uses only freelists.
  free(all_free_chunks);
}

/*
 * Initializes arrays of num_particle particles from a file.
 *
 * @param[in]  mpi_world_size the number of MPI processes.
 * @param[in]  num_particle the size of the array (number of particles in it).
 * @param[in]  mesh, the mesh on which we're working.
 * @param[out] weight.
 * @param[out] particles[mesh.ncx * mesh.ncy * mesh.ncz] a newly allocated array of chunkbags of particles read from file.
 */
void read_particle_array_3d(int mpi_world_size, unsigned int num_particle, cartesian_mesh_3d mesh,
        float* weight, bag** particles) {
    size_t i;
    char filename[30];
    int throw_that_number_x, throw_that_number_y, throw_that_number_z;
    unsigned int throw_that_number_also;
    const int ncx = mesh.num_cell_x;
    const int ncy = mesh.num_cell_y;
    const int ncz = mesh.num_cell_z;
    const int num_cells_3d = ncx * ncy * ncz;
    int i_cell;
    float dx, dy, dz;
    double vx, vy, vz;

    sprintf(filename, "initial_particles_%dkk.dat", num_particle / 1000000);
    FILE* file_read_particles = fopen(filename, "r");
    if (!file_read_particles) { // Error in file opening
        fprintf(stderr, "%s doesn't exist.\n", filename);
        exit(EXIT_FAILURE);
    }
    if (fscanf(file_read_particles, "%d %d %d %u", &throw_that_number_x, &throw_that_number_y, &throw_that_number_z,
            &throw_that_number_also) < 3) {
        fprintf(stderr, "%s should begin with ncx ncy ncz num_particle.\n", filename);
        exit(EXIT_FAILURE);
    }
    if (throw_that_number_x != ncx) {
        fprintf(stderr, "I expected ncx = %d but found %d in the input file.\n", ncx, throw_that_number_x);
        exit(EXIT_FAILURE);
    }
    if (throw_that_number_y != ncy) {
        fprintf(stderr, "I expected ncy = %d but found %d in the input file.\n", ncy, throw_that_number_y);
        exit(EXIT_FAILURE);
    }
    if (throw_that_number_z != ncz) {
        fprintf(stderr, "I expected ncz = %d but found %d in the input file.\n", ncz, throw_that_number_z);
        exit(EXIT_FAILURE);
    }
    if (throw_that_number_also != num_particle) {
        fprintf(stderr, "I expected num_particle = %d but found %d in the input file.\n", num_particle, throw_that_number_also);
        exit(EXIT_FAILURE);
    }
    *weight = (float)(mesh.x_max - mesh.x_min) * (float)(mesh.y_max - mesh.y_min) * (float)(mesh.z_max - mesh.z_min) / ((float)mpi_world_size * (float)num_particle);

    // Initializes the bags with empty chunks.
    for (i = 0; i < num_cells_3d; i++)
        bag_init_initial(&((*particles)[i]));

    // Read particles and push them into the bags.
    for (i = 0; i < num_particle; i++) {
        if (fscanf(file_read_particles, "%d %f %f %f %lf %lf %lf", &i_cell,
                &dx, &dy, &dz, &vx, &vy, &vz) < 7) {
            fprintf(stderr, "I expected %d particles but there are less in the input file.\n", num_particle);
            exit(EXIT_FAILURE);
        }
        bag_push_initial(&((*particles)[i_cell]), CHECKER_ONLY_COMMA(i) dx, dy, dz, vx, vy, vz);
    }
    fclose(file_read_particles);

    // Initializes the different freelists with the remaining free chunks.
    init_freelists();
}

/*
 * Return an array of num_particle random particles following given distributions
 * for positions and speeds.
 *
 * @param[in]  mpi_world_size the number of MPI processes.
 * @param[in]  num_particle the size of the array (number of particles in it).
 * @param[in]  mesh, the mesh on which we're working.
 * @param[in]  sim_distrib the physical test case (tells the distribution that the particules should follow).
 * @param[in]  spatial_params.
 * @param[in]  thermal_speed.
 * @param[out] weight.
 * @param[out] particles[mesh.ncx * mesh.ncy * mesh.ncz] a newly allocated array of chunkbags of randomized particles.
 */
void create_particle_array_3d(int mpi_world_size, unsigned int num_particle, cartesian_mesh_3d mesh,
        unsigned char sim_distrib, double* spatial_params, double* speed_params, float* weight,
        bag** particles) {
    size_t i, j;
    int i_cell;
    double x, y, z, vx, vy, vz;
    double control_point, evaluated_function;
    speeds_generator_3d speeds_generator = speed_generators_3d[sim_distrib];
    distribution_function_3d distrib_function = distribution_funs_3d[sim_distrib];
    max_distribution_function max_distrib_function = distribution_maxs_3d[sim_distrib];

    const double x_range = mesh.x_max - mesh.x_min;
    const double y_range = mesh.y_max - mesh.y_min;
    const double z_range = mesh.z_max - mesh.z_min;
    const int ncx = mesh.num_cell_x;
    const int ncy = mesh.num_cell_y;
    const int ncz = mesh.num_cell_z;
    const int icell_param1 = I_CELL_PARAM1_3D(ncx, ncy, ncz);
    const int icell_param2 = I_CELL_PARAM2_3D(ncx, ncy, ncz);
    const int num_cells_3d = ncx * ncy * ncz;

    *weight = (float)x_range * (float)y_range * (float)z_range / ((float)mpi_world_size * (float)num_particle);

    // Initializes the bags with empty chunks.
    for (i = 0; i < num_cells_3d; i++)
        bag_init_initial(&((*particles)[i]));

    // Create particles and push them into the bags.
    for (j = 0; j < num_particle; j++) {
        do {
#ifdef DEBUG_CHECKER
            double rx = pic_vert_next_random_double();
            x = x_range * rx + mesh.x_min;
            // printf("id = %d, rand = %lf, x = %lf\n", (int) j, rx, x);
#else
            x = x_range * pic_vert_next_random_double() + mesh.x_min;
#endif
            y = y_range * pic_vert_next_random_double() + mesh.y_min;
            z = z_range * pic_vert_next_random_double() + mesh.z_min;
            control_point = (*max_distrib_function)(spatial_params) * pic_vert_next_random_double();
            evaluated_function = (*distrib_function)(spatial_params, x, y, z);
        } while (control_point > evaluated_function);
#ifdef DEBUG_CHECKER
        printf("created = %d, %lf %lf %lf ", (int) j, x, y, z);
#endif
        x = (x - mesh.x_min) / mesh.delta_x;
        y = (y - mesh.y_min) / mesh.delta_y;
        z = (z - mesh.z_min) / mesh.delta_z;
        (*speeds_generator)(speed_params, &vx, &vy, &vz);
#ifdef DEBUG_CHECKER
        printf("%lf %lf %lf \n", vx, vy, vz);
#endif
        i_cell = COMPUTE_I_CELL_3D(icell_param1, icell_param2, (int)x, (int)y, (int)z);
        bag_push_initial(&((*particles)[i_cell]), CHECKER_ONLY_COMMA(j) (float)(x - (int)x), (float)(y - (int)y), (float)(z - (int)z), vx, vy, vz);
    }

    // Initializes the different freelists with the remaining free chunks.
    init_freelists();

#ifdef PIC_VERT_TEST_INITIAL_DISTRIBUTION
    // Test the initial distribution.
    size_t k;
    FILE* file_diag_particles = fopen("test_particles_3d.dat", "w");
    unsigned int nb_particle_in_cell;
    switch(sim_distrib) {
        case LANDAU_1D_PROJ3D:
            for (i = 0; i < ncx; i++) {
                nb_particle_in_cell = 0;
                for (j = 0; j < ncy; j++)
                    for (k = 0; k < ncz; k++)
                        nb_particle_in_cell += bag_size(&((*particles)[COMPUTE_I_CELL_3D(icell_param1, icell_param2, i, j, k)]));
                fprintf(file_diag_particles, "%ld %d\n", i, nb_particle_in_cell);
            }
            break;
        case LANDAU_2D_PROJ3D:
            for (i = 0; i < ncx; i++)
                for (j = 0; j < ncy; j++) {
                    nb_particle_in_cell = 0;
                    for (k = 0; k < ncz; k++)
                        nb_particle_in_cell += bag_size(&((*particles)[COMPUTE_I_CELL_3D(icell_param1, icell_param2, i, j, k)]));
                    fprintf(file_diag_particles, "%ld %ld %d\n", i, j, nb_particle_in_cell);
                }
            break;
        default:
            for (i = 0; i < ncx; i++)
                for (j = 0; j < ncy; j++)
                    for (k = 0; k < ncz; k++)
                        fprintf(file_diag_particles, "%ld %ld %ld %d\n", i, j, k, bag_size(&((*particles)[COMPUTE_I_CELL_3D(icell_param1, icell_param2, i, j, k)])));
    }
    fclose(file_diag_particles);
#endif
}

