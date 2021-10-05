#ifndef PIC_VERT_PARTICLE_TYPE_CHUNKBAGS_OF_AOS_2D
#define PIC_VERT_PARTICLE_TYPE_CHUNKBAGS_OF_AOS_2D

#include "meshes.h"     // type      cartesian_mesh_2d
#include "parameters.h" // constants NB_PARTICLE, CHUNK_SIZE, NB_PROC

/*****************************************************************************
 *                       Particle data structures                            *
 *****************************************************************************/

/*
 *         y-axis
 *            ^
 *            |
 *      y_max |———————|———————|   ...               ...   |———————|
 *            |       |  2 *  |                           |  ncy  |
 *            |ncx - 1|  ncx  |                           |* ncx  |
 *            |       |  - 1  |                           |  - 1  |
 *            |———————|———————|   ...               ...   —————————
 *            .               .                                   .
 *            .               .                                   .
 *            .               .                                   .
 *                                    dx*delta_x
 *            |                         <——>
 *            |           ...           |———————|                 .
 *            |                         |       |                 .
 *          y +-------------------------|--*    | ^               .
 *            |                         |  .    | | dy*delta_y
 *            |           ...           |———————| v
 *            |                            .     
 *            .               .            .              .       .
 *            .               .            .              .       .
 *            .               .            .              .       .
 *            |———————|———————|            .              |———————| ^
 *            |       |       |            .              |(ncy-1)| |
 *            |   1   |ncx + 1|            .              |* ncx  | | delta_y
 *            |       |       |            .              |  + 1  | |
 *            |———————|———————|   ...      .        ...   ————————— v
 *            |       |       |            .              |(ncy-1)|
 *            |   0   |  ncx  |            .              |* ncx  |
 *            |       |       |            .              |       |
 *      y_min +———————|———————|   ...   ———+—————   ...   —————————————> x-axis
 *          x_min     <———————>            x                    x_max
 *                     delta_x
 *
 * In the physical world, the particle has x in [x_min ; x_max [
 *                                         y in [y_min ; y_max [
 * 
 * This is mapped to a grid of size ncx * ncy. Thus, we have :
 *     delta_x = (x_max - x_min) / ncx;
 *     delta_y = (y_max - y_min) / ncy;
 * 
 * If we call :
 *     x_mapped = (x - x_min) / delta_x which is in [0 ; ncx [
 *     y_mapped = (y - y_min) / delta_y which is in [0 ; ncy [
 *     index_x = floor(x_mapped), the number of times we have to move
 *         by delta_x from the start of the mesh to the particle on the x-axis
 *     index_y = floor(y_mapped), the number of times we have to move
 *         by delta_y from the start of the mesh to the particle on the y-axis
 * Then i_cell, the cell index (given inside the cells above), is computed by :
 *     i_cell = index_x * ncy + index_y;
 */
typedef struct particle_2d particle_2d;
struct particle_2d {
     float dx;  // x_mapped - index_x, a number in [0 ; 1[       (see the drawing above)
     float dy;  // y_mapped - index_y, a number in [0 ; 1[       (see the drawing above)
     double vx; // speed of the particle on the x-axis
     double vy; // speed of the particle on the y-axis
};

/*****************************************************************************
 *                              Chunk bags                                   *
 *****************************************************************************/

/*
 * In this example, CHUNK_SIZE = 20.
 *
 * ? in the array means allocated space for the array, not filled with elements.
 *
 * ? in the size means that the size is not yet computed for the last chunk.
 *
 *                    Chunk bag
 *       { front   back    b_head  b_end };
 *       |———————|———————|———————|———————|
 *       |       |       |       |       |
 *       |   X   |   X   |   X   |   X   |
 *       |   |   |   |   |   |   |   |   |
 *       |———+———|———+———|———+———|———+———|
 *           |       |       |       |
 *           |       |       |       +——————————————————————————————————————+
 *           |       |       |                                              |
 *           |       |       +—————————————————————————————————————————+    |
 *           |       |                                                 |    |
 *           |       +—————————————————————————————————————+           |    |
 *           |                                             |           |    |
 *           |     +——————+     +— ...     ——+     +————+  |           |    |
 *           |     |      |     |            |     |    |  |           |    |
 *           v     |      v     |            v     |    v  v           |    |
 *       |———————| |  |———————| |        |———————| |  |———————|        |    |
 *       |       | |  |       | |        |       | |  |       |        |    |
 *  next |   X———+—+  |   X———+—+        |   X———+—+  |   X———+——>NIL  |    |
 *       |       |    |       |          |       |    |       |        |    |
 *       |———————|    |———————|    ...   |———————|    |———————|        |    |
 *       |       |    |       |          |       |    |       |        |    |
 *  size |  16   |    |  11   |          |  13   |    |   ?   |        |    |
 *       |       |    |       |          |       |    |       |        |    |
 *       |———————|    |———————|    ...   |———————|    |———————|        |    |
 *     0 |       |    |       |          |       |    |       |        |    |
 *     1 |       |    |       |          |       |    |       |        |    |
 *     2 |       |    |       |          |       |    |       |        |    |
 *     3 |       |    |       |          |       |    |       |        |    |
 *     4 |       |    |       |          |       |    |       |        |    |
 *     5 |       |    |       |          |       |    |       |        |    |
 *     6 |       |    |       |          |       |    |       |        |    |
 *     7 |       |    |       |          |       |    |       |        |    |
 *     8 |       |    |       |          |       |    |       |        |    |
 *     9 |       |    |       |          |       |    |       |        |    |
 *    10 |       |    |       |          |       |    |       |        |    |
 *    11 |       |    |   ?   |          |       |    |       |        |    |
 *    12 |       |    |   ?   |          |       |    |       |        |    |
 *    13 |       |    |   ?   |          |   ?   |    |       |        |    |
 *    14 |       |    |   ?   |          |   ?   |    |       |        |    |
 *    15 |       |    |   ?   |          |   ?   |    |       |        |    |
 *    16 |   ?   |    |   ?   |          |   ?   |    |       |        |    |
 *    17 |   ?   |    |   ?   |          |   ?   |    |   ?   |<———————+    |
 *    18 |   ?   |    |   ?   |          |   ?   |    |   ?   |             |
 *    19 |   ?   |    |   ?   |          |   ?   |    |   ?   |             |
 *       |———————|    |———————|    ...   |———————|    |———————|<————————————+
 *
 */

#define FREELIST_SIZE (NB_PARTICLE / CHUNK_SIZE / NB_PROC)

typedef particle_2d particle;

typedef struct chunk {
  struct chunk* next;
  int size;                  // warning: not updated when the chunk is last of a bag
  particle array[CHUNK_SIZE];
} chunk;

typedef struct bag { 
  chunk* front;
  chunk* back;
  particle* back_end;  // cell one past the end in back->array
  particle* back_head; // first free cell in back->array
  // back->size  is deduced as CHUNK_SIZE - (back_end - back_head);
} bag;

// You need one free list per processor
chunk* free_chunks[NB_PROC][FREELIST_SIZE];
// free_index[thread_id][0] will store the number of free chunks available
// for thread number thread_id. Other subscripts are not used, but this is a
// matrix and not an array to avoid false sharing (each row is aligned
// on the cache line size, usually 64 bytes on modern Intel architectures).
#define FREE_INDEX(i) free_index[i][0]
int** free_index;

/*
 * Fills free_chunks with as many chunks as possible and sets free_index accordingly.
 */
void init_freelists();

chunk* chunk_alloc(int thread_id);

void chunk_free(chunk* c, int thread_id);

/*
 * Allocate a new chunk and put it at the back
 * of a chunk bag.
 *
 * @param[in, out] b the bag in which to put the new chunk.
 * @return         the newly allocated chunk.
 */
chunk* new_back_chunk(bag* b, int thread_id);

/*
 * Initialize a bag (already allocated) with only one empty chunk.
 *
 * In this example, CHUNK_SIZE = 20.
 * The array is filled with ? because it's not filled yet.
 * The size of the array is of course 0, but it will not be maintained,
 * so we don't compute it.
 *
 *                    Chunk bag
 *       { front   back    b_head  b_end };
 *       |———————|———————|———————|———————|
 *       |       |       |       |       |
 *       |   X   |   X   |   X   |   X   |
 *       |   |   |   |   |   |   |   |   |
 *       |———+———|———+———|———+———|———+———|
 *           |       |       |       |
 *           |  +————+       |       |
 *           |  |            |       |
 *           v  v            |       |
 *        |———————|          |       |
 *        |       |          |       |
 *   next |   X———+——>NIL    |       |
 *        |       |          |       |
 *        |———————|          |       |
 *        |       |          |       |
 *   size |   ?   |          |       |
 *        |       |          |       |
 *        |———————|          |       |
 *      0 |   ?   |<—————————+       |
 *      1 |   ?   |                  |
 *      2 |   ?   |                  |
 *      3 |   ?   |                  |
 *      4 |   ?   |                  |
 *      5 |   ?   |                  |
 *      6 |   ?   |                  |
 *      7 |   ?   |                  |
 *      8 |   ?   |                  |
 *      9 |   ?   |                  |
 *     10 |   ?   |                  |
 *     11 |   ?   |                  |
 *     12 |   ?   |                  |
 *     13 |   ?   |                  |
 *     14 |   ?   |                  |
 *     15 |   ?   |                  |
 *     16 |   ?   |                  |
 *     17 |   ?   |                  |
 *     18 |   ?   |                  |
 *     19 |   ?   |                  |
 *        |———————|<—————————————————+
 *
 * @param[in, out] b the bag to initialize.
 */
void bag_init(bag* b, int thread_id);

/*
 * Compute the number of elements stored into the last
 * chunk of a chunk bag.
 * Uses pointer arithmetic.
 *
 * @param[in] b the bag.
 * @return    the number of elements stored into b->back.
 */
int bag_back_size(bag* b);

/*
 * Compute the number of elements stored into all
 * chunks of a chunk bag.
 *
 * @param[in] b the bag.
 * @return    the number of elements stored into b.
 */
int bag_size(bag* b);

/*
 * Compute the number of elements stored into the last
 * chunk of a chunk bag, and store this number inside
 * b->back->size.
 *
 * @param[in] b the bag.
 */
void bag_store_size_of_back(bag* b);

/* 
 * Merge other into b; other becomes empty.
 *
 * @param[in, out] b
 * @param[in, out] other
 */
void bag_append(bag* b, bag* other, int thread_id);

/* 
 * Add a particle into a chunk bag. Add it into the last chunk,
 * unless this chunk is full. In that case, allocate a new chunk
 * before adding it into that new chunk.
 *
 * @param[in, out] b
 * @param[in]      p
 */
void bag_push(bag* b, particle p, int thread_id);

/*
 * Initializes arrays of num_particle particles from a file.
 *
 * @param[in]  mpi_world_size the number of MPI processes.
 * @param[in]  num_particle the size of the array (number of particles in it).
 * @param[in]  mesh, the mesh on which we're working.
 * @param[out] weight.
 * @param[out] particles[num_particle] a newly allocated array of chunkbags of particles read from file.
 */
void read_particle_array_2d(int mpi_world_size, unsigned int num_particle, cartesian_mesh_2d mesh,
        float* weight, bag** particles);

/*
 * Return an array of num_particle random particles following given distributions
 * for positions and speeds.
 * drand48() returns doubles uniformly distributed in the interval [0. ; 1.[.
 *
 * @param[in]  mpi_world_size the number of MPI processes.
 * @param[in]  num_particle the size of the array (number of particles in it).
 * @param[in]  mesh, the mesh on which we're working.
 * @param[in]  sim_distrib the physical test case (tells the distribution that the particules should follow).
 * @param[in]  spatial_params.
 * @param[in]  thermal_speed.
 * @param[out] weight.
 * @param[out] particles[num_particle] a newly allocated array of chunkbags of randomized particles.
 */
void create_particle_array_2d(int mpi_world_size, unsigned int num_particle, cartesian_mesh_2d mesh,
        unsigned char sim_distrib, double* spatial_params, double* speed_params, float* weight,
        bag** particles);

#endif // ifndef PIC_VERT_PARTICLE_TYPE_CHUNKBAGS_OF_AOS_2D

