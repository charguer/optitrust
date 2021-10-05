#ifndef PIC_VERT_PARTICLE_TYPE_CONCURRENT_CHUNKBAGS_OF_AOS_2D
#define PIC_VERT_PARTICLE_TYPE_CONCURRENT_CHUNKBAGS_OF_AOS_2D

#include "domain_decomposition.h" // type      mpi_parameters
#include "meshes.h"               // type      cartesian_mesh_2d
#include "parameters.h"           // constants VEC_ALIGN, CHUNK_SIZE
#include "variadic.h"             // macros    VARIADIC, NUMARG32

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
 *            Chunk bag
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

typedef particle_2d particle;

typedef struct chunk {
  struct chunk* next;
  int size;                  // always updated
  particle array[CHUNK_SIZE];
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

// When calling with 0 argument, the macro will set mpi_world_size to 1.
// WARNING: uses ISEMPTY, so unlike other uses of VARIADIC, 0 is for the "not empty" case (here : 1 argument)
// and 1 is for the "empty" case (0 argument).

// When calling with 1 argument, the macro will set mpi_world_size to 1.
#define init_freelists_1(a   ) a, 0
#define init_freelists_2(a, b) a, b
#define init_freelists(...) VARIADIC(init_freelists, NUMARG32(__VA_ARGS__), __VA_ARGS__)

/*
 * Fills free_chunks with as many chunks as possible and sets free_index accordingly.
 *
 * @param mpi_world_size the number of mpi processes when using domain decomposition. Optional, defaults to 1.
 */
void init_freelists(long int num_particle, int mpi_world_size);

chunk* chunk_alloc(int thread_id);

void chunk_free(chunk* c, int thread_id);

/*
 * Allocate a new chunk and put it at the front
 * of a chunk bag.
 *
 * @param[in, out] b the bag in which to put the new chunk.
 */
void add_front_chunk(bag* b, int thread_id);

/*
 * Initialize a bag (already allocated) with only one empty chunk.
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
void bag_init(bag* b, int thread_id);

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
 * Merge other into b; other is re-initialized.
 *
 * @param[in, out] b
 * @param[in, out] other
 */
void bag_append(bag* b, bag* other, int thread_id);

/* 
 * Merge other into b; other is nullified.
 *
 * @param[in, out] b
 * @param[in, out] other
 */
void bag_append_nullify(bag* b, bag* other);

/* 
 * Merge other into b; other is re-initialized if not null,
 * and stays null if it was already null.
 *
 * @param[in, out] b
 * @param[in, out] other
 */
void bag_append_null_remains(bag* b, bag* other, int thread_id);

/* 
 * Add a particle into a chunk bag. Add it into the first chunk,
 * then tests if this chunk is full. In that case, allocate a new
 * chunk after adding the particle.
 * This function is thread-safe (uses atomics).
 *
 * @param[in, out] b
 * @param[in]      p
 */
void bag_push_concurrent(bag* b, particle p, int thread_id);

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
void bag_push_serial(bag* b, particle p, int thread_id);

/*
 * Initializes arrays of num_particle particles from a file.
 *
 * @param[in]  mpi_world_size the number of MPI processes.
 * @param[in]  num_particle the number of particles.
 * @param[in]  mesh, the mesh on which we're working.
 * @param[out] weight.
 * @param[out] particles[mesh.ncx * mesh.ncy] a newly allocated array of chunkbags of particles read from file.
 */
void read_particle_array_2d(int mpi_world_size, long int num_particle, cartesian_mesh_2d mesh,
        float* weight, bag** particles);

/*
 * Return an array of num_particle random particles following given distributions
 * for positions and speeds.
 *
 * @param[in]  mpi_world_size the number of MPI processes.
 * @param[in]  num_particle the number of particles.
 * @param[in]  mesh, the mesh on which we're working.
 * @param[in]  sim_distrib the physical test case (tells the distribution that the particules should follow).
 * @param[in]  spatial_params.
 * @param[in]  thermal_speed.
 * @param[out] weight.
 * @param[out] particles[mesh.ncx * mesh.ncy] a newly allocated array of chunkbags of randomized particles.
 */
void create_particle_array_2d(int mpi_world_size, long int num_particle, cartesian_mesh_2d mesh,
        unsigned char sim_distrib, double* spatial_params, double* speed_params, float* weight,
        bag** particles);

/*
 * Initializes arrays of num_particle particles from a file.
 *
 * @param[in]  par_variables the MPI parameters.
 * @param[in]  num_particle the number of particles.
 * @param[out] weight.
 * @param[out] particles[mesh.ncx * mesh.ncy] a newly allocated array of chunkbags of particles read from file.
 */
void read_dd_particle_array_2d(mpi_parameters par_variables, long int num_particle,
        float* weight, bag** particles);

/*
 * Split a file with all the particles of the form initial_particles_%dkk.dat in 4 files
 * of the form initial_particles_%dkk_rank%d.dat
 *
 * @param[in]  par_variables the MPI parameters.
 * @param[in]  num_particle the number of particles.
 */
void read_and_split_particles_2d(mpi_parameters par_variables, long int num_particle);

/*
 * Return an array of num_particle random particles following a given distributions
 * for positions and speeds.
 *
 * @param[in]      par_variables the MPI parameters.
 * @param[in]      sim_distrib the physical test case (tells the distribution that the particules should follow).
 * @param[in]      spatial_params.
 * @param[in]      thermal_speed.
 * @param[in, out] num_particle the number of particles.
 * @param[out]     weight.
 * @param[out]     particles[mesh.ncx * mesh.ncy] a newly allocated array of chunkbags of randomized particles.
 */
void create_dd_particle_array_2d(mpi_parameters par_variables,
        unsigned char sim_distrib, double* spatial_params, double* speed_params,
        long int* num_particle, float* weight, bag** particles);

/*
 * Write the num_particle particles in a file, in ASCII.
 *
 * @param[in] par_variables the MPI parameters.
 * @param[in] num_particle the number of particles.
 * @param[in] particles[mesh.ncx * mesh.ncy] a newly allocated array of chunkbags of randomized particles.
 */
void write_ascii_dd_particle_array_2d(mpi_parameters par_variables,
        long int num_particle, bag* particles);

#endif // ifndef PIC_VERT_PARTICLE_TYPE_CONCURRENT_CHUNKBAGS_OF_AOS_2D

