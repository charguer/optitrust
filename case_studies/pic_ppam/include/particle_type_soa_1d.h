#ifndef PIC_VERT_PARTICLE_TYPE_SOA_1D
#define PIC_VERT_PARTICLE_TYPE_SOA_1D

#include "meshes.h" // type cartesian_mesh_1d

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
 * @param[out] i_cell, dx, vx[num_particle] newly allocated arrays of randomized particles.
 */
void create_particle_arrays_1d(int mpi_world_size, unsigned int num_particle, cartesian_mesh_1d mesh,
        unsigned char sim_distrib, double* spatial_params, double* speed_params, float* weight,
        int** i_cell, float** dx, double** vx);



/*****************************************************************************
 *                           Sorting functions                               *
 *                     (by increasing values of i_cell)                      *
 *****************************************************************************/

typedef struct particle_sorter_oop_1d particle_sorter_oop_1d;
struct particle_sorter_oop_1d {
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
};

/*
 * @param[in] num_particle, number of particles in the array to sort.
 * @param[in] num_cell, number of cells in the mesh (number of different values for i_cell).
 * @return    a particle sorter.
 */
particle_sorter_oop_1d new_particle_sorter_oop_1d(unsigned int num_particle, unsigned int num_cell);

/*
 * Sort the array of particles (with temporary arrays).
 *
 * @param[in, out] sorter a pointer on the particle_sorter_oop for this array (has to be initialized
 *                 before the call).
 * @param[in, out] i_cell, dx, vx pointers on the [num_particle] arrays of particles to sort.
 */
void sort_particles_oop_1d(particle_sorter_oop_1d* sorter, int** i_cell,
        float** dx, double** vx);

void free_particle_sorter_oop_1d(particle_sorter_oop_1d sorter, unsigned int num_cell);

#endif // ifndef PIC_VERT_PARTICLE_TYPE_SOA_1D

