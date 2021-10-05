#ifndef PIC_VERT_PARTICLE_TYPE_SOA_3D
#define PIC_VERT_PARTICLE_TYPE_SOA_3D

#include "meshes.h"   // type cartesian_mesh_3d
#include "variadic.h" // macros VARIADIC, NUMARG11, NUMARG14, NUMARG17

// When calling with 11 arguments, the macro will set the icXXX to NULL.
#define read_particle_arrays_3d_11(a, b, c, d, e,    g, h,    j, k,    m, n) a, b, c, d, e, NULL, g, h, NULL, j, k, NULL, m, n
#define read_particle_arrays_3d_14(a, b, c, d, e, f, g, h, i, j, k, l, m, n) a, b, c, d, e, f,    g, h, i,    j, k, l,    m, n
#define read_particle_arrays_3d(...) VARIADIC(read_particle_arrays_3d, NUMARG14(__VA_ARGS__), __VA_ARGS__)

/*
 * Initializes arrays of num_particle particles from a file.
 *
 * @param[in]  mpi_world_size the number of MPI processes.
 * @param[in]  num_particle the size of the arrays (number of particles in it).
 * @param[in]  mesh, the mesh on which we're working.
 * @param[out] weight.
 * @param[out] icx, dx, vx, ivy, dy, vy, icz, dz, vz[num_particle] newly allocated arrays of particles read from file.
 */
void read_particle_arrays_3d(int mpi_world_size, unsigned int num_particle, cartesian_mesh_3d mesh,
        float* weight, int** i_cell,
        short int** icx, float** dx, double** vx,
        short int** icy, float** dy, double** vy,
        short int** icz, float** dz, double** vz);

// When calling with 14 arguments, the macro will set the icXXX to NULL.
#define create_particle_arrays_3d_14(a, b, c, d, e, f, g, h,    j, k,    m, n,    p, q) a, b, c, d, e, f, g, h, NULL, j, k, NULL, m, n, NULL, p, q
#define create_particle_arrays_3d_17(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) a, b, c, d, e, f, g, h, i,    j, k, l,    m, n, o,    p, q
#define create_particle_arrays_3d(...) VARIADIC(create_particle_arrays_3d, NUMARG17(__VA_ARGS__), __VA_ARGS__)

/*
 * Initializes arrays of num_particle random particles following a given distribution
 * for positions, and following the gaussian distribution for speeds.
 * drand48() returns doubles uniformly distributed in the interval [0. ; 1.[.
 *
 * @param[in]  mpi_world_size the number of MPI processes.
 * @param[in]  num_particle the size of the arrays (number of particles in it).
 * @param[in]  mesh, the mesh on which we're working.
 * @param[in]  sim_distrib the physical test case (tells the distribution that the particules should follow).
 * @param[in]  spatial_params.
 * @param[in]  thermal_speed.
 * @param[out] weight.
 * @param[out] icx, dx, vx, ivy, dy, vy, icz, dz, vz[num_particle] newly allocated arrays of randomized particles.
 */
void create_particle_arrays_3d(int mpi_world_size, unsigned int num_particle, cartesian_mesh_3d mesh,
        unsigned char sim_distrib, double* spatial_params, double* speed_params, float* weight,
        int** i_cell,
        short int** icx, float** dx, double** vx,
        short int** icy, float** dy, double** vy,
        short int** icz, float** dz, double** vz);



/*****************************************************************************
 *                           Sorting functions                               *
 *                     (by increasing values of i_cell)                      *
 *****************************************************************************/

typedef struct particle_sorter_oop_3d particle_sorter_oop_3d;
struct particle_sorter_oop_3d {
    // number of particles in the array to sort
    unsigned int num_particle;
    // number of cells in the mesh (number of different values for i_cell)
    unsigned int num_cell;
    // [num_threads][num_cell] array of integers, telling
    // how many particles each thread sees for each value of i_cell.
    int** num_particle_per_cell;
    // [num_threads][num_cell] array. index_next_particle[t][i] tells where the
    // thread t has to store the next particle that has i_cell = i. Whenever
    // a thread puts particle p in the correct position,
    // index_next_particle[t][p.i_cell] is incremented.
    int** index_next_particle;
    // [num_particle] additional arrays used to sort.
    float* dx_tmp;
    double* vx_tmp;
    float* dy_tmp;
    double* vy_tmp;
    float* dz_tmp;
    double* vz_tmp;
#ifdef PIC_VERT_SFC_WITH_ADDITIONAL_ARRAYS
    short int* icx_tmp;
    short int* icy_tmp;
    short int* icz_tmp;
#endif
};

/*
 * @param[in] num_particle, number of particles in the array to sort.
 * @param[in] num_cell, number of cells in the mesh (number of different values for i_cell).
 * @return    a particle sorter.
 */
particle_sorter_oop_3d new_particle_sorter_oop_3d(unsigned int num_particle, unsigned int num_cell);

// When calling with 8 arguments, the macro will set the icXXX to NULL.
#define sort_particles_oop_3d_8( a, b,    d, e,    g, h,    j, k) a, b, NULL, d, e, NULL, g, h, NULL, j, k
#define sort_particles_oop_3d_11(a, b, c, d, e, f, g, h, i, j, k) a, b, c,    d, e, f,    g, h, i,    j, k
#define sort_particles_oop_3d(...) VARIADIC(sort_particles_oop_3d, NUMARG11(__VA_ARGS__), __VA_ARGS__)

/*
 * Sort the array of particles (with temporary arrays).
 *
 * @param[in, out] sorter a pointer on the particle_sorter_oop for this array (has to be initialized
 *                 before the call).
 * @param[in, out] i_cell, icx, dx, vx, icy, dy, vy, icz, dz, vz pointers on the [num_particle] arrays of particles to sort.
 */
void sort_particles_oop_3d(particle_sorter_oop_3d* sorter, int** i_cell,
        short int** icx, float** dx, double** vx,
        short int** icy, float** dy, double** vy,
        short int** icz, float** dz, double** vz);

void free_particle_sorter_oop_3d(particle_sorter_oop_3d sorter, unsigned int num_cell);

#endif // ifndef PIC_VERT_PARTICLE_TYPE_SOA_3D

