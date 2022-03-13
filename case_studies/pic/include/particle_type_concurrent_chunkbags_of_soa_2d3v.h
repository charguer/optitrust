#ifndef PIC_VERT_PARTICLE_TYPE_CONCURRENT_CHUNKBAGS_OF_SOA_2D3V
#define PIC_VERT_PARTICLE_TYPE_CONCURRENT_CHUNKBAGS_OF_SOA_2D3V

#include "meshes.h"     // type     cartesian_mesh_2d
#include "parameters.h" // constant CHUNK_SIZE

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

typedef struct chunk {
  struct chunk* next;
  int size;                  // always updated
  float  dx[CHUNK_SIZE] __attribute__((aligned(VEC_ALIGN))); // x_mapped - index_x, a number in [0 ; 1[       (see the drawing above)
  float  dy[CHUNK_SIZE] __attribute__((aligned(VEC_ALIGN))); // y_mapped - index_y, a number in [0 ; 1[       (see the drawing above)
  double vx[CHUNK_SIZE] __attribute__((aligned(VEC_ALIGN))); // speed of the particle on the x-axis
  double vy[CHUNK_SIZE] __attribute__((aligned(VEC_ALIGN))); // speed of the particle on the y-axis
  double vz[CHUNK_SIZE] __attribute__((aligned(VEC_ALIGN))); // speed of the particle on the z-axis
} chunk;

typedef struct bag {
  chunk* front;
  chunk* back;
} bag;

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

/*
 * Take a chunk from the freelist of thread_id. If the freelist is empty,
 * allocate a new one. This function should only be called during the particle
 * loop. During the append phase, the needed chunks should be found
 * in all the freelists, depending of the cumulative freelist sizes.
 *
 * @param[in] thread_id the index of the thread asking for a new chunk.
 * @return    a chunk almost ready to be filled with particles (set size to 0 before).
 */
chunk* chunk_alloc(int thread_id);

/*
 * Put a chunk back into the freelist of thread_id. This function is only
 * called during the particle loop, on a chunk that has just been iterated on.
 * If that list is full, free the chunk.
 *
 * @param[in] c         the chunk to be put back in the freelist.
 * @param[in] thread_id the index of the thread asking for this release.
 */
void chunk_free(chunk* c, int thread_id);

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
void add_front_chunk(bag* b, int thread_id);

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
void compute_cumulative_free_list_sizes();

/*
 * Update the freelist sizes after the append phase, according to the number
 * of chunks that were taken in each freelist. It should therefore
 * be called after the append phase.
 *
 * WARNING : This function is not thread-safe, and should be called only by
 * the master thread.
 */
void update_free_list_sizes();

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
void bag_init(bag* b, int id_bag, int id_cell, int thread_id);

/*
 * Merge other into b; other is re-initialized if not null,
 * and stays null if it was already null.
 *
 * @param[in, out] b
 * @param[in, out] other
 */
void bag_append(bag* b, bag* other, int id_bag, int id_cell, int thread_id);

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
void bag_nullify(bag* b);

/*
 * Compute the number of elements stored into all
 * chunks of a chunk bag.
 *
 * @param[in] b the bag.
 * @return    the number of elements stored into b.
 */
int bag_size(bag* b);

/*
 * Guarantee that the entire value of *p is read atomically.
 * No part of *p can change during the read operation.
 *
 * Taken from openmp-examples-4.5.0.pdf, Example atomic.2.c
 */
chunk* atomic_read(chunk** p);

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
 * @param[in]      dx, dy, vx, vy, vz
 */
void bag_push_concurrent(bag* b, float dx, float dy, double vx, double vy, double vz, int thread_id);

/*
 * Add a particle into a chunk bag. Add it into the first chunk,
 * then tests if this chunk is full. In that case, allocate a new
 * chunk after adding the particle.
 * This function is not thread-safe. Multiple threads should not
 * access the same bag.
 *
 * @param[in, out] b
 * @param[in]      dx, dy, vx, vy, vz
 */
void bag_push_serial(bag* b, float dx, float dy, double vx, double vy, double vz, int thread_id);

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
void set_borders_x(int ix_min, int normal_border_size, int* lower_x_border, int* upper_x_border);

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
void set_borders_y(int iy_min, int normal_border_size, int* lower_y_border, int* upper_y_border);

/*
 * Does the full allocation of all the chunks needed for the simulation in one pass.
 * Threads will iterate particles tile by tile. For each tile, the thread that iterates
 * over it needs to have private bags for the tile + borders (to avoid numerous atomic
 * operations, as particles will frequently move outside the tile).
 * Also initializes particlesNext with empty chunks where needed (and null bags otherwise).
 *
 * But we do not need to have as many private bags per cell as there are threads. We
 * can have only 4 private bags (in 2d) per cell, as long as we ensure that in each
 * direction, 2 * border_size <= tile_size. Then we can look at, in each direction,
 * the parity of the index of the tile to know in which private bag to work.
 *
 * To know how many private bags we will really use (we won't use 4 per cell each time),
 * there are two equivalent ways of doing the computation. In the following, we
 * denote by c = tile_size and b = border_size.
 *   1. Each tile will need (c + 2 * b)**2 private bags. Thus, in total,
 *      we need a number of private bags equal to :
 *      (c + 2 * b)**2 / (c**2) * nb_cells.
 *   2. Each cell will have a number of private bags depending of its position:
 *      corners         -> 4 *                  b**2 cells, each 4 private bags
 *      (rest of) edges -> 4 * (c - 2 * b)    * b    cells, each 2 private bags
 *      interior        -> 1 * (c - 2 * b)**2        cells, each 1 private bag
 *
 * WARNING : We make the assumption that:
 *   1. in the case of a non-colored algorithm, the id of the shared bag is 4
 * (= nb_bags_per_cell - 1), and that for each tile  beginning at
 * (ix_min, iy_min), the id of the corresponding private bag is computed by:
 * ((ix_min % (2 * tile_size)) != 0) + 2 * ((iy_min % (2 * tile_size)) != 0)
 *   2. in the case of a colored algorithm, the id of the shared bag is 1,
 * and the id of the private bag is 0.
 * In both cases, if this is not the case, you have to modify it.
 *
 * Remark : we could solve this with a macro, but this would mean that we would have to pass
 * yet another parameter in command line to know whether we are in color or non-color algorithm,
 * whereas the core of the code would not be easy to read if written with preprocessor instructions.
 *
 * @param[in] nb_bags_per_cell  the number of bags per cells (2 when using coloring scheme, 5 otherwise).
 * @param[in] num_particle      the total number of particles in this MPI process.
 * @param[in] mesh              the physical mesh we're working on.
 * @param[in] tile_size         the size of an edge of a cubic tile.
 * @param[in] border_size       the size of the borders allowed in a private bag (= tile_size / 2
 *                              when using coloring scheme, <= tile_size / 2 otherwise).
 * @param[in,out] particlesNext
 */
void init_all_chunks(int nb_bags_per_cell, long int num_particle, cartesian_mesh_2d mesh,
    int tile_size, int border_size, bag*** particlesNext);

/*
 * Write the num_particle particles in a file, in ASCII.
 *
 * @param[in] num_particle the number of particles.
 * @param[in] mesh, the mesh on which we're working.
 * @param[in] particles[mesh.ncx * mesh.ncy] a newly allocated array of chunkbags of particles read from file.
 */
void write_particle_array_2d3v(int mpi_rank, long int num_particle, cartesian_mesh_2d mesh, bag* particles);

/*
 * Initializes arrays of num_particle particles from a file, in ASCII.
 *
 * @param[in]  mpi_world_size the number of MPI processes.
 * @param[in]  num_particle the size of the array (number of particles in it).
 * @param[in]  mesh, the mesh on which we're working.
 * @param[out] weight.
 * @param[out] particles[mesh.ncx * mesh.ncy] a newly allocated array of chunkbags of particles read from file.
 */
void read_particle_array_2d3v(int mpi_rank, int mpi_world_size, long int num_particle, cartesian_mesh_2d mesh,
        float* weight, bag** particles);

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
 * @param[out] particles[mesh.ncx * mesh.ncy] a newly allocated array of chunkbags of randomized particles.
 */
void create_particle_array_2d3v(int mpi_world_size, long int num_particle, cartesian_mesh_2d mesh,
        unsigned char sim_distrib, double* spatial_params, double* speed_params, float* weight,
        bag** particles);

#endif // ifndef PIC_VERT_PARTICLE_TYPE_CONCURRENT_CHUNKBAGS_OF_SOA_2D3V

