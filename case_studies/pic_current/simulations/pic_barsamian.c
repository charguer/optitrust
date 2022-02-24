/**
 * sim3d_aocosoa.c: PIC simulations in 3d.
 *
 * Compilation :
 *     Pic-Vert/scripts/3d_performance
 *
 * Contact:
 *   Yann Barsamian <ybarsamian@unistra.fr>
 */

//#define PAPI_LIB_INSTALLED

// #define PRINTPARAMS 1
// #define PRINTPERF 1


#ifdef CHECKER
#define CHECKER_ONLY(X) X
#define STR(a) STRINTERNAL(a)
#define STRINTERNAL(a) #a
#define CHECKER_FILENAME STR(CHECKER)
#else
#define CHECKER_ONLY(X)
#endif


#include <omp.h>                                          // functions omp_get_wtime, omp_get_num_threads, omp_get_thread_num
#include <math.h>                                         // functions cos, log, fmin
#include <mpi.h>                                          // constants MPI_COMM_WORLD, MPI_THREAD_FUNNELED
                                                          // functions MPI_Init, MPI_Finalize, MPI_Comm_size, MPI_Comm_rank
#include <stdio.h>                                        // function  printf, fprintf (output strings on a stream)
                                                          // constant  stderr (standard error output stream)
#include <stdlib.h>                                       // functions malloc, free ((de)allocate memory)
                                                          //           exit (error handling)
                                                          // constant  EXIT_FAILURE (error handling)
                                                          // type      size_t
#include <string.h>                                       // function  strcmp
#ifdef PAPI_LIB_INSTALLED
#    include <papi.h>                                     // constants PAPI_OK, PAPI_L1_DCM, PAPI_L2_DCM, PAPI_L3_TCM...
                                                          // functions PAPI_read_counters
                                                          // type      long_long
#    include "papi_handlers.h"                            // functions start_diag_papi, stop_diag_papi
#endif
#include "compiler_test.h"                                // constant  PIC_VERT_OPENMP_4_0
#include "diagnostics.h"                                  // function  integral_of_squared_field_3d, get_damping_values
#include "fields.h"                                       // type      field_3d
                                                          // functions create_field_3d, free_field_3d, accumulate_field_3d
#include "initial_distributions.h"                        // constants LANDAU_1D_PROJ3D, LANDAU_2D_PROJ3D, LANDAU_3D_SUM_OF_COS,
                                                          //           LANDAU_3D_PROD_OF_COS, LANDAU_3D_PROD_OF_ONE_PLUS_COS
#include "math_functions.h"                               // function  sqr
#include "matrix_functions.h"                             // functions allocate_matrix, deallocate_matrix, allocate_3d_array, deallocate_3d_array,
                                                          //           allocate_aligned_int_array_array, deallocate_aligned_int_array_array
                                                          // type      aligned_int_array
#include "meshes.h"                                       // type      cartesian_mesh_3d
                                                          // function  create_mesh_3d
#include "output.h"                                       // functions print_time_chunkbags, diag_energy_and_speed_chunkbags
#include "parameters.h"                                   // constants PI, EPSILON, VEC_ALIGN, DBL_DECIMAL_DIG, FLT_DECIMAL_DIG, NB_PARTICLE
#include "parameter_reader.h"                             // type      simulation_parameters
                                                          // constants STRING_NOT_SET, INT_NOT_SET, DOUBLE_NOT_SET
                                                          // function  read_parameters_from_file
#include "particle_type_concurrent_chunkbags_of_soa_3d.h" // types     chunk, bag
                                                          // functions create_particle_array_3d, init_all_chunks, bag_push_concurrent, bag_push_serial,
                                                          //           bag_init, bag_append
#include "poisson_solvers.h"                              // type      poisson_3d_solver
                                                          // function  new_poisson_3d_fft_solver, compute_E_from_rho_3d_fft, free_poisson_3d
#include "random.h"                                       // macros    pic_vert_seed_double_RNG, pic_vert_free_RNG
#include "rho.h"                                          // constant  NB_CORNERS_3D
                                                          // functions mpi_reduce_rho_3d, reset_charge_3d_accumulator, convert_charge_to_rho_3d_per_per
#include "space_filling_curves.h"                         // macro     COMPUTE_I_CELL_3D

/*****************************************************************************
 *                             Simulation 3d                                 *
 *****************************************************************************/

// All the simulations in this file follow the 'chunk bags of structure of arrays'
// data layout. The structure doesn't contains the weight, because it is known
// to be a constant.

#define INIT_READ   0
#define INIT_WRITE  1
#define INIT_NOFILE 2

// Id of the private chunkbag.
#define ID_PRIVATE_BAG   0
// Id of the shared chunkbag.
#define ID_SHARED_BAG    1
// Total number of chunkbags.
#define NB_BAGS_PER_CELL 2

// OpenMP tiling.
// Each thread has private chunkbags for the cells in the tile
// it's working on + borders = TILE_SIZE / 2.
// We have shared chunkbags in the case where particles move further than
// half-tile away.
// We use a coloring scheme to avoid races.
#ifndef OMP_TILE_SIZE
#   define OMP_TILE_SIZE 4
#endif
#define OMP_TILE_BORDERS (OMP_TILE_SIZE / 2)

#ifndef THERMAL_SPEED
#    define THERMAL_SPEED 1.
#endif

#ifndef DRIFT_VELOCITY
#    define DRIFT_VELOCITY 4.
#endif

#ifndef PROPORTION_FAST_PARTICLES
#    define PROPORTION_FAST_PARTICLES 0.01
#endif

#ifndef INITIAL_DISTRIBUTION
#    define INITIAL_DISTRIBUTION LANDAU_3D_PROD_OF_ONE_PLUS_COS
#endif

int main(int argc, char** argv) {
    // Timing
    double time_start, time_simu;
    double time_mark1, time_mark2, time_mark3, time_mark4, time_mark5;
    double time_particle_loop, time_append, time_mpi_allreduce, time_poisson;

#ifdef PAPI_LIB_INSTALLED
    // Performance counters
    int papi_num_events = 3;
    int Events[papi_num_events];
    Events[0] = PAPI_L1_DCM;
    Events[1] = PAPI_L2_DCM;
    Events[2] = PAPI_L3_TCM;
    long_long values[papi_num_events];
    FILE* file_diag_papi;
#endif

    // Automatic values for the parameters.
    unsigned char sim_distrib = INITIAL_DISTRIBUTION; // Physical test case (LANDAU_1D_PROJ3D, LANDAU_2D_PROJ3D, LANDAU_3D_SUM_OF_COS,
                                                      // LANDAU_3D_PROD_OF_COS, LANDAU_3D_PROD_OF_ONE_PLUS_COS or DRIFT_VELOCITIES_3D).
    int ncx               = NCX;                      // Number of grid points, x-axis
    int ncy               = NCY;                      // Number of grid points, y-axis
    int ncz               = NCZ;                      // Number of grid points, z-axis
    long int nb_particles = NB_PARTICLE;              // Number of particles
    int num_iteration     = NB_ITER;                  // Number of time steps
    double delta_t        = DELTA_T;                  // Time step
    double thermal_speed  = THERMAL_SPEED;            // Thermal speed
    // DRIFT_VELOCITIES_3D only
    double drift_velocity            = DRIFT_VELOCITY;            // Center of the second maxwellian
    double proportion_fast_particles = PROPORTION_FAST_PARTICLES; // Proportions of the particles in the second maxwellian
    // LANDAU_3D_PROD_OF_ONE_PLUS_COS only
    double L = 22.;        // Length of the physical space
    // LANDAU_XXX only
    double alpha   = 0.05; // Landau perturbation amplitude
    double kmode_x = 0.5;  // Landau perturbation mode, x-axis
    double kmode_y = 0.5;  // Landau perturbation mode, y-axis
    double kmode_z = 0.5;  // Landau perturbation mode, z-axis

    // Read parameters from file.
    if (argc >= 2) {
#ifdef PRINTPARAMS
        printf("Loading parameters from file %s\n", argv[1]);
#endif
        simulation_parameters parameters = read_parameters_from_file(argv[1], "3D");
        if (strcmp(parameters.sim_distrib_name, STRING_NOT_SET) == 0)
            sim_distrib = parameters.sim_distrib;
        if (parameters.ncx != INT_NOT_SET)
            ncx             = parameters.ncx;
        if (parameters.ncy != INT_NOT_SET)
            ncy             = parameters.ncy;
        if (parameters.ncz != INT_NOT_SET)
            ncz             = parameters.ncz;
        if (parameters.nb_particles != INT_NOT_SET)
            nb_particles    = parameters.nb_particles;
        if (parameters.num_iteration != INT_NOT_SET)
            num_iteration   = parameters.num_iteration;
        if (parameters.delta_t != DOUBLE_NOT_SET)
            delta_t       = parameters.delta_t;
        if (parameters.thermal_speed != DOUBLE_NOT_SET)
            thermal_speed = parameters.thermal_speed;
        // DRIFT_VELOCITIES_3D only
        if (parameters.drift_velocity != DOUBLE_NOT_SET)
            drift_velocity = parameters.drift_velocity;
        if (parameters.proportion_fast_particles != DOUBLE_NOT_SET)
            proportion_fast_particles = parameters.proportion_fast_particles;
        // LANDAU_3D_PROD_OF_ONE_PLUS_COS only
        if (parameters.L != DOUBLE_NOT_SET)
            L = parameters.L;
        // LANDAU_XXX only
        if (parameters.alpha != DOUBLE_NOT_SET)
            alpha   = parameters.alpha;
        if (parameters.kmode_x != DOUBLE_NOT_SET)
            kmode_x = parameters.kmode_x;
        if (parameters.kmode_y != DOUBLE_NOT_SET)
            kmode_y = parameters.kmode_y;
        if (parameters.kmode_z != DOUBLE_NOT_SET)
            kmode_z = parameters.kmode_z;
    } else
        printf("No parameter file was passed through the command line. I will use the default parameters.\n");

    // Random initialization or read from file.
    const char sim_initial = INIT_NOFILE;

    // Spatial parameters for initial density function.
    double *params;
    if (sim_distrib == LANDAU_1D_PROJ3D) {
        params = malloc(2 * sizeof(double));
        params[0] = alpha;
        params[1] = kmode_x;
    } else if (sim_distrib == LANDAU_2D_PROJ3D) {
        params = malloc(3 * sizeof(double));
        params[0] = alpha;
        params[1] = kmode_x;
        params[2] = kmode_y;
    } else if ((sim_distrib == LANDAU_3D_SUM_OF_COS) || (sim_distrib == LANDAU_3D_PROD_OF_COS)) {
        params = malloc(4 * sizeof(double));
        params[0] = alpha;
        params[1] = kmode_x;
        params[2] = kmode_y;
        params[3] = kmode_z;
    } else if (sim_distrib == LANDAU_3D_PROD_OF_ONE_PLUS_COS) {
        params = malloc(4 * sizeof(double));
        params[0] = alpha;
        params[1] = 2 * PI / L;
        params[2] = 2 * PI / L;
        params[3] = 2 * PI / L;
    }

    // Velocity parameters for initial density function.
    double *speed_params;
    if (sim_distrib == DRIFT_VELOCITIES_3D) {
        params = malloc(3 * sizeof(double));
        speed_params = malloc(3 * sizeof(double));
        speed_params[0] = thermal_speed;
        speed_params[1] = drift_velocity;
        speed_params[2] = proportion_fast_particles;
    } else {
        speed_params = malloc(1 * sizeof(double));
        speed_params[0] = thermal_speed;
    }

    // Mesh
    double x_min, y_min, z_min, x_max, y_max, z_max;
    const int num_cells_3d = ncx * ncy * ncz;
    x_min = 0.;
    y_min = 0.;
    z_min = 0.;
    if (sim_distrib == LANDAU_1D_PROJ3D) {
        x_max = 2 * PI / kmode_x;
        y_max = 1.;
        z_max = 1.;
    } else if (sim_distrib == LANDAU_2D_PROJ3D) {
        x_max = 2 * PI / kmode_x;
        y_max = 2 * PI / kmode_y;
        z_max = 1.;
    } else if (sim_distrib == LANDAU_3D_PROD_OF_ONE_PLUS_COS) {
        x_max = L;
        y_max = L;
        z_max = L;
    } else {
        x_max = 2 * PI / kmode_x;
        y_max = 2 * PI / kmode_y;
        z_max = 2 * PI / kmode_z;
    }
    cartesian_mesh_3d mesh = create_mesh_3d(ncx, ncy, ncz, x_min, x_max, y_min, y_max, z_min, z_max);
#if I_CELL_3D_TYPE == ROW_MAJOR
    const int icell_param1 = ncy;
    const int icell_param2 = ncz;
#endif
    const int ncxminusone = ncx - 1;
    const int ncyminusone = ncy - 1;
    const int nczminusone = ncz - 1;

    // Vectorization of the deposit
    int corner;
/*
 * x-axis : left  -> right
 * y-axis : front -> back
 * z-axis : down  -> top
 *
 * (0, 0, 0) : Left,  Front, Down : LFD
 * (0, 0, 1) : Left,  Front, Top  : LFT
 * (0, 1, 0) : Left,  Back,  Down : LBD
 * (0, 1, 1) : Left,  Back,  Top  : LBT
 * (1, 0, 0) : Right, Front, Down : RFD
 * (1, 0, 1) : Right, Front, Top  : RFT
 * (1, 1, 0) : Right, Back,  Down : RBD
 * (1, 1, 1) : Right, Back,  Top  : RBT
 *
 *
 *    LBT +————————+ RBT
 *       /'       /|
 *      / '   RFT/ |
 * LFT +————————+  |
 *     |  '     |  |
 *     |  +-----|--+ RBD
 *     | /LBD   | /
 *     |/       |/
 * LFD +————————+ RFD
 *
 */
    // Space coeffs                                                             LFD  LFT  LBD  LBT  RFD  RFT  RBD  RBT
    const float coeffs_x[NB_CORNERS_3D] __attribute__((aligned(VEC_ALIGN))) = {  1.,  1.,  1.,  1.,  0.,  0.,  0.,  0.};
    const float  signs_x[NB_CORNERS_3D] __attribute__((aligned(VEC_ALIGN))) = { -1., -1., -1., -1.,  1.,  1.,  1.,  1.};
    const float coeffs_y[NB_CORNERS_3D] __attribute__((aligned(VEC_ALIGN))) = {  1.,  1.,  0.,  0.,  1.,  1.,  0.,  0.};
    const float  signs_y[NB_CORNERS_3D] __attribute__((aligned(VEC_ALIGN))) = { -1., -1.,  1.,  1., -1., -1.,  1.,  1.};
    const float coeffs_z[NB_CORNERS_3D] __attribute__((aligned(VEC_ALIGN))) = {  1.,  0.,  1.,  0.,  1.,  0.,  1.,  0.};
    const float  signs_z[NB_CORNERS_3D] __attribute__((aligned(VEC_ALIGN))) = { -1.,  1., -1.,  1., -1.,  1., -1.,  1.};

    // Simulation parameters
    const double q           = -1.; // particle charge
    const double m           =  1.; // particle mass
    const double dt_q_over_m = delta_t * q / m;
    const double dt_over_dx  = delta_t / mesh.delta_x;
    const double dt_over_dy  = delta_t / mesh.delta_y;
    const double dt_over_dz  = delta_t / mesh.delta_z;
    char simulation_name[42]     = "Vlasov-Poisson 3d";
#ifdef SPARE_LOC_OPTIMIZED
    char data_structure_name[99] = "Array of [Optim] Chunkbags of SoA (1 private + 1 shared / cell)";
#else
    char data_structure_name[99] = "Array of Chunkbags of SoA (1 private + 1 shared / cell)";
#endif
    char sort_name[42]           = "always sort";

    // MPI + OpenMP parallelism
    int mpi_world_size, mpi_rank;
    double* send_buf = malloc(num_cells_3d * sizeof(double));
    double* recv_buf = malloc(num_cells_3d * sizeof(double));
    int mpi_thread_support;
    MPI_Init_thread(&argc, &argv, MPI_THREAD_FUNNELED, &mpi_thread_support);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_world_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    int num_threads;
    int thread_id;
    int offset;
    #pragma omp parallel
    num_threads = omp_get_num_threads();

    // Temporary variables.
    double x, y, z;                                                 // store the new position values
    int ic_x, ic_y, ic_z, i_cell;                                   // store the new position index values
    size_t i, j, k, i_time;                                         // loop indices
    double*** q_times_rho = allocate_3d_array(ncx+1, ncy+1, ncz+1); // to store q * rho = -rho
    aligned_int_array* i_cells = allocate_aligned_int_array_array(num_threads); // to vectorize the computation of the i_cells

    // The following matrices are (ncx+1) * (ncy+1) * (ncz+1) arrays, with the periodicity :
    //     M[ncx][ . ][ . ] = M[0][.][.]
    //     M[ . ][ncy][ . ] = M[.][0][.]
    //     M[ . ][ . ][ncz] = M[.][.][0]
    // rho the charge density
    // Ex the electric field on the x-axis
    // Ey the electric field on the y-axis
    // Ez the electric field on the z-axis
    double*** rho_3d = allocate_3d_array(ncx+1, ncy+1, ncz+1);
    double*** Ex = allocate_3d_array(ncx+1, ncy+1, ncz+1);
    double*** Ey = allocate_3d_array(ncx+1, ncy+1, ncz+1);
    double*** Ez = allocate_3d_array(ncx+1, ncy+1, ncz+1);

    // accumulators are num_cells_3d arrays : for each cell, the 8 corners values
    field_3d E_field = create_field_3d(ncx, ncy, ncz);
    // For each cell, the 8 corners values for vectorization ; each thread has its own copy
    double* charge_accu;
    if (posix_memalign((void**)&charge_accu, VEC_ALIGN, num_cells_3d * NB_CORNERS_3D * num_threads * sizeof(double))) {
        fprintf(stderr, "posix_memalign failed to initialize charge_accu.\n");
        exit(EXIT_FAILURE);
    }
    // For reduction of charge_accu over the threads.
    double* reduced_charge_accu;
    if (posix_memalign((void**)&reduced_charge_accu, VEC_ALIGN, num_cells_3d * NB_CORNERS_3D * sizeof(double))) {
        fprintf(stderr, "posix_memalign failed to initialize reduced_charge_accu.\n");
        exit(EXIT_FAILURE);
    }
#ifdef __INTEL_COMPILER
    __assume_aligned(charge_accu, VEC_ALIGN);
    __assume_aligned(reduced_charge_accu, VEC_ALIGN);
#else
    charge_accu = __builtin_assume_aligned(charge_accu, VEC_ALIGN);
    reduced_charge_accu = __builtin_assume_aligned(reduced_charge_accu, VEC_ALIGN);
#endif

    // Diagnostic energy.
    double kmode = 0.;
    switch(sim_distrib) {
        case LANDAU_1D_PROJ3D:
            kmode = kmode_x;
            break;
        case LANDAU_3D_SUM_OF_COS:
        case LANDAU_3D_PROD_OF_ONE_PLUS_COS:
            // The smaller kmode is, the more it's dominating (the decay is smaller).
            kmode = fmin(kmode_x, fmin(kmode_y, kmode_z));
            break;
        case LANDAU_2D_PROJ3D:
            kmode = sqrt(sqr(kmode_x) + sqr(kmode_y));
            break;
        case LANDAU_3D_PROD_OF_COS:
            kmode = sqrt(sqr(kmode_x) + sqr(kmode_y) + sqr(kmode_z));
            break;
    }
    damping_values* landau_values = get_damping_values(kmode);
    const double er         = landau_values->er;
    const double psi        = landau_values->psi;
    const double omega_real = landau_values->omega_real;
    const double omega_imag = landau_values->omega_imag;
    double exval_ee, val_ee, t;
    const double landau_mult_cstt = sqr(4. * alpha * er) * PI / kmode; // Landau
    const int diag_energy_size = 5;
    const int diag_speed_size  = 5;
    double** diag_energy = allocate_matrix(num_iteration, diag_energy_size);
    double** diag_speed  = allocate_matrix(num_iteration, diag_speed_size);

    // Poisson solver.
    poisson_3d_solver solver = new_poisson_3d_fft_solver(mesh);

    // Coloring
    int i_color;
    const int nb_color_3d = 8;

    // Particle data structure.
    bag* chunkbag;
    chunk* next_chunk;
    chunk* my_chunk;
    int ix_min, ix_max, iy_min, iy_max, iz_min, iz_max, ix, iy, iz;
    bag* particles = malloc(num_cells_3d * sizeof(bag));
    bag** particlesNext = malloc(NB_BAGS_PER_CELL * sizeof(bag*));
    for (j = 0; j < NB_BAGS_PER_CELL; j++)
        particlesNext[j] = malloc(num_cells_3d * sizeof(bag));
    init_all_chunks(NB_BAGS_PER_CELL, nb_particles, mesh, OMP_TILE_SIZE, OMP_TILE_BORDERS, &particlesNext);

    /* A "numerical particle" (we also say "macro particle") represents several
     * physical particles. The weight is the number of physical particles it
     * represents. The more particles we have in the simulation, the less this
     * weight will be. A numerical particle may represent a different number of
     * physical particles than another numerical particle, even though in this
     * simulation it's not the case.
     */
    float weight;
    if (sim_initial == INIT_READ) {
        time_start = omp_get_wtime();
        read_particle_array_3d(mpi_world_size, nb_particles, mesh, &weight, &particles);
        if (mpi_rank == 0)
            printf("Read time (%ld particles) : %g sec\n", nb_particles, (double) (omp_get_wtime() - time_start));
    } else {
        // printf("Mpi_rank %d\n", mpi_rank); // prints zero
        pic_vert_seed_double_RNG(mpi_rank);
//         Different random numbers at each run.
//         pic_vert_seed_double_RNG(seed_64bits(mpi_rank));
        // Creation of random particles and sorting.
        time_start = omp_get_wtime();
        create_particle_array_3d(mpi_world_size, nb_particles, mesh, sim_distrib,
            params, speed_params, &weight, &particles);
        if (mpi_rank == 0)
#ifdef PRINTPERF
            printf("Creation time (%ld particles) : %g sec\n", nb_particles, (double) (omp_get_wtime() - time_start));
#endif
        if (sim_initial == INIT_WRITE) {
            // Export the particles.
            char filename[30];
            sprintf(filename, "final_particles_yans_%ldkk.dat", nb_particles / 1000000);
            FILE* file_write_particles = fopen(filename, "w");
            fprintf(file_write_particles, "%d %d %d\n", ncx, ncy, ncz);
            fprintf(file_write_particles, "%ld\n", nb_particles);
            for (j = 0; j < num_cells_3d; j++) {
                chunkbag = &(particles[j]);
                for (my_chunk = chunkbag->front; my_chunk; my_chunk = my_chunk->next) {
                    for (i = 0; i < my_chunk->size; i++) {
                        fprintf(file_write_particles, "%ld %.*g %.*g %.*g %.*g %.*g %.*g\n", j,
                          DBL_DECIMAL_DIG, my_chunk->dx[i], DBL_DECIMAL_DIG, my_chunk->dy[i], DBL_DECIMAL_DIG, my_chunk->dz[i],
                          DBL_DECIMAL_DIG, my_chunk->vx[i], DBL_DECIMAL_DIG, my_chunk->vy[i], DBL_DECIMAL_DIG, my_chunk->vz[i]);
                    }
                }
            }
            fclose(file_write_particles);
            MPI_Finalize();
            return 0;
        }
    }

    // Because the weight is constant, the whole array can be multiplied by weight just once.
    // Because charge is the charge MASS and not the charge DENSITY, we have to divide.
    const double charge_factor = weight / (mesh.delta_x * mesh.delta_y * mesh.delta_z);
    // We just use the electric fields to update the speed, with always the same multiply.
    const double x_field_factor = dt_q_over_m * dt_over_dx;
    const double y_field_factor = dt_q_over_m * dt_over_dy;
    const double z_field_factor = dt_q_over_m * dt_over_dz;

#ifdef PRINTPARAMS
  if (mpi_rank == 0) {
    printf("#CHUNK_SIZE = %d\n", CHUNK_SIZE);
    printf("#VEC_ALIGN = %d\n", VEC_ALIGN);
    printf("#OMP_TILE_SIZE = %d\n", OMP_TILE_SIZE);
    printf("#OMP_TILE_BORDERS = %d\n", OMP_TILE_BORDERS);
    printf("#mpi_world_size = %d\n", mpi_world_size);
    printf("#num_threads = %d\n", num_threads);
    printf("#alpha = %.*g\n", DBL_DECIMAL_DIG, alpha);
    printf("#x_min = %.*g\n", DBL_DECIMAL_DIG, x_min);
    printf("#x_max = %.*g\n", DBL_DECIMAL_DIG, x_max);
    printf("#y_min = %.*g\n", DBL_DECIMAL_DIG, y_min);
    printf("#y_max = %.*g\n", DBL_DECIMAL_DIG, y_max);
    printf("#z_min = %.*g\n", DBL_DECIMAL_DIG, z_min);
    printf("#z_max = %.*g\n", DBL_DECIMAL_DIG, z_max);
    printf("#delta_t = %.*g\n", DBL_DECIMAL_DIG, delta_t);
    printf("#thermal_speed = %.*g\n", DBL_DECIMAL_DIG, thermal_speed);
    if (sim_distrib == DRIFT_VELOCITIES_3D) {
        printf("#drift_velocity = %.*g\n", DBL_DECIMAL_DIG, drift_velocity);
        printf("#proportion_fast_particles = %.*g\n", DBL_DECIMAL_DIG, proportion_fast_particles);
    } else if (sim_distrib == LANDAU_3D_PROD_OF_ONE_PLUS_COS) {
        printf("#L = %.*g\n", DBL_DECIMAL_DIG, L);
    }
    printf("#initial_function_case = %s\n", distribution_names_3d[sim_distrib]);
//    printf("#weight = %.*g\n", DBL_DECIMAL_DIG, weight);
//    printf("#charge_factor = %.*g\n", DBL_DECIMAL_DIG, charge_factor);
//    printf("#x_field_factor = %.*g\n", DBL_DECIMAL_DIG, x_field_factor);
//    printf("#y_field_factor = %.*g\n", DBL_DECIMAL_DIG, y_field_factor);
//    printf("#z_field_factor = %.*g\n", DBL_DECIMAL_DIG, z_field_factor);
    printf("#ncx = %d\n", ncx);
    printf("#ncy = %d\n", ncy);
    printf("#ncz = %d\n", ncz);
    printf("#nb_particles = %ld\n", nb_particles);
    printf("#num_iteration = %d\n", num_iteration);
  }
#endif

    reset_charge_3d_accumulator(ncx, ncy, ncz, num_threads, charge_accu);
    // Computes rho at initial time.
    #pragma omp parallel private(thread_id, offset)
    {
        thread_id = omp_get_thread_num();
        offset = thread_id * NB_CORNERS_3D * num_cells_3d;
        #pragma omp for private(i, j, chunkbag, my_chunk, corner)
        for (j = 0; j < num_cells_3d; j++) {
            chunkbag = &(particles[j]);
            for (my_chunk = chunkbag->front; my_chunk; my_chunk = my_chunk->next) {
                for (i = 0; i < my_chunk->size; i++) {
#ifdef PIC_VERT_OPENMP_4_0
                    #pragma omp simd aligned(coeffs_x, coeffs_y, coeffs_z, signs_x, signs_y, signs_z:VEC_ALIGN)
#endif
                    for (corner = 0; corner < NB_CORNERS_3D; corner++) {
                        charge_accu[offset + NB_CORNERS_3D * j + corner] +=
                            (coeffs_x[corner] + signs_x[corner] * my_chunk->dx[i]) *
                            (coeffs_y[corner] + signs_y[corner] * my_chunk->dy[i]) *
                            (coeffs_z[corner] + signs_z[corner] * my_chunk->dz[i]);
                    }
                }
            }
        }
        #pragma omp for private(i, j, corner)
        for (j = 0; j < num_cells_3d; j++) {
            for (corner = 0; corner < NB_CORNERS_3D; corner++)
                reduced_charge_accu[NB_CORNERS_3D * j + corner] = charge_accu[NB_CORNERS_3D * j + corner];
            for (i = 1; i < num_threads; i++) {
                offset = i * NB_CORNERS_3D * num_cells_3d;
                for (corner = 0; corner < NB_CORNERS_3D; corner++)
                    reduced_charge_accu[NB_CORNERS_3D * j + corner] += charge_accu[offset + NB_CORNERS_3D * j + corner];
            }
        }
    } // End parallel region
    convert_charge_to_rho_3d_per_per(reduced_charge_accu, ncx, ncy, ncz, charge_factor, rho_3d);
    mpi_reduce_rho_3d(mpi_world_size, send_buf, recv_buf, ncx, ncy, ncz, rho_3d);

    // Computes E at initial time.
    for (i = 0; i < ncx + 1; i++)
        for (j = 0; j < ncy + 1; j++)
            for (k = 0; k < ncz + 1; k++)
                q_times_rho[i][j][k] = q * rho_3d[i][j][k];


#ifdef DEBUG_CHARGE
    // printf("charge_factor = %g  = nbcells/nbParticles = %g\n", charge_factor, ((double) ncx*ncy*ncz) / nb_particles);
    double s = 0.;
    for (int i = 0; i < ncx; i++) {
        for (int j = 0; j < ncy; j++) {
            for (int k = 0; k < ncz; k++) {
                s += rho_3d[i][j][k];
                // printf("rho[%d][%d][%d] = %lf\n", i, j, k, rho[i][j][k]);
                printf("q_times_rho[%d][%d][%d] = %lf\n", i, j, k, q_times_rho[i][j][k]);
                // printf("q_times_rho[%d][%d][%d] / nbCells = %lf\n", i, j, k, q_times_rho[i][j][k] / (ncx * ncy * ncz));
                // printf("q_times_rho[%d][%d][%d] / charge_factor / nbParticles = %lf\n", i, j, k, q_times_rho[i][j][k] / charge_factor / nb_particles);
            }
        }
    }
   printf("total charge rho = %g\n", s);
#endif

    compute_E_from_rho_3d_fft(solver, q_times_rho, Ex, Ey, Ez, 1);

#ifdef DEBUG_FIELD
#if 1
  for (int i = 0; i < ncx; i++) {
    for (int j = 0; j < ncy; j++) {
      for (int k = 0; k < ncz; k++) {
        double r = (-q); // / nb_particles / charge_factor;
        printf("field[%d][%d][%d]*(-q) = %g %g %g\n", i, j, k,
            r*Ex[i][j][k], r*Ey[i][j][k], r*Ez[i][j][k]);
      }
    }
  }
#endif
#endif

    accumulate_field_3d(Ex, Ey, Ez, ncx, ncy, ncz, x_field_factor, y_field_factor, z_field_factor, E_field);

#ifdef DEBUG_FIELD
    for (int i = 0; i < ncx; i++) {
        for (int j = 0; j < ncy; j++) {
            for (int k = 0; k < ncz; k++) {
                //if (! (i == 0 && j == 0 && k ==0))
                //  break;
                i_cell = COMPUTE_I_CELL_3D(icell_param1, icell_param2, i, j, k);
                double r = - q; // / nb_particles / charge_factor; // /nbParticles/charge_factor
                printf("E_field<%d>[%d][%d][%d].left_front_down *(-q)/xyz_field_factor = %g %g %g\n", i_cell, i, j, k,
                  r/x_field_factor * E_field[i_cell].field_x.left_front_down,
                  r/y_field_factor * E_field[i_cell].field_y.left_front_down,
                  r/z_field_factor * E_field[i_cell].field_z.left_front_down);
            }
        }
    }
#endif

#ifdef DEBUG_ACCEL
    printf("nb_particles = %ld\n", nb_particles);
    printf("delta_t = %g\n", delta_t);
    printf("q = %g\n", q);
    printf("m = %g\n", m);
    printf("xrange = %g\n", mesh.x_max - mesh.x_min);
    printf("yrange = %g\n", mesh.y_max - mesh.y_min);
    printf("zrange = %g\n", mesh.z_max - mesh.z_min);
    printf("dx = %g\n", mesh.delta_x);
    printf("dy = %g\n", mesh.delta_y);
    printf("dz = %g\n", mesh.delta_z);
    printf("dt_over_dx = %g\n", dt_over_dx);
    printf("dt_q_over_m = %g\n", dt_q_over_m);
    printf("weight = %g\n", weight);
    printf("charge_factor = %g\n", charge_factor);
    printf("x_field_factor = %g\n", x_field_factor);
#endif

    // Computes speeds half time-step backward (leap-frog method).
    // WARNING : starting from here, v doesn't represent the speed, but speed * dt / dx.
    #pragma omp parallel for private(i, j, chunkbag, my_chunk)
    for (j = 0; j < num_cells_3d; j++) {
        chunkbag = &(particles[j]);
        for (my_chunk = chunkbag->front; my_chunk; my_chunk = my_chunk->next) {
#ifdef PIC_VERT_OPENMP_4_0
            #pragma omp simd
#endif
            for (i = 0; i < my_chunk->size; i++) {
#ifdef DEBUG_ACCEL
              int idp = my_chunk->id[i];
              if (idp == 0) {
                double r = - q / x_field_factor;
                printf("particle %d: topcorner_fieldx *(-q)/x_field_factor = %g\n", idp, r * E_field[j].field_x.left_front_down);
                double fieldx = (
                        (     my_chunk->dx[i]) * (     my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_x.right_back_top
                      + (1. - my_chunk->dx[i]) * (     my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_x.left_back_top
                      + (     my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_x.right_front_top
                      + (1. - my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_x.left_front_top
                      + (     my_chunk->dx[i]) * (     my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_x.right_back_down
                      + (1. - my_chunk->dx[i]) * (     my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_x.left_back_down
                      + (     my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_x.right_front_down
                      + (1. - my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_x.left_front_down);
                printf("particle %d: fieldx = %g\n", idp, fieldx);
                printf("particle %d: fieldx *(-q)/x_field_factor = %g\n", idp, r * fieldx);
                double delta_vx = - 0.5 * fieldx;
                printf("particle %d: delta_vx = %g\n", idp, delta_vx);
                printf("particle %d: delta_vx / (dt/dx) = %g\n", idp, delta_vx / dt_over_dx);
                printf("particle %d: oldvx / dt_over_dx = %g\n", idp, my_chunk->vx[i]);
                printf("particle %d: oldvx = %g\n", idp, my_chunk->vx[i] * dt_over_dx);
                printf("particle %d: newvx = %g\n", idp, my_chunk->vx[i] * dt_over_dx + delta_vx);
                printf("particle %d: newvx / (dt/dx) = %g\n", idp, (my_chunk->vx[i] * dt_over_dx + delta_vx) / dt_over_dx);
              }
#endif
                my_chunk->vx[i] = my_chunk->vx[i] * dt_over_dx - 0.5 * (
                      (     my_chunk->dx[i]) * (     my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_x.right_back_top
                    + (1. - my_chunk->dx[i]) * (     my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_x.left_back_top
                    + (     my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_x.right_front_top
                    + (1. - my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_x.left_front_top
                    + (     my_chunk->dx[i]) * (     my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_x.right_back_down
                    + (1. - my_chunk->dx[i]) * (     my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_x.left_back_down
                    + (     my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_x.right_front_down
                    + (1. - my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_x.left_front_down);
                my_chunk->vy[i] = my_chunk->vy[i] * dt_over_dy - 0.5 * (
                      (     my_chunk->dx[i]) * (     my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_y.right_back_top
                    + (1. - my_chunk->dx[i]) * (     my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_y.left_back_top
                    + (     my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_y.right_front_top
                    + (1. - my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_y.left_front_top
                    + (     my_chunk->dx[i]) * (     my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_y.right_back_down
                    + (1. - my_chunk->dx[i]) * (     my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_y.left_back_down
                    + (     my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_y.right_front_down
                    + (1. - my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_y.left_front_down);
                my_chunk->vz[i] = my_chunk->vz[i] * dt_over_dz - 0.5 * (
                      (     my_chunk->dx[i]) * (     my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_z.right_back_top
                    + (1. - my_chunk->dx[i]) * (     my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_z.left_back_top
                    + (     my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_z.right_front_top
                    + (1. - my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_z.left_front_top
                    + (     my_chunk->dx[i]) * (     my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_z.right_back_down
                    + (1. - my_chunk->dx[i]) * (     my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_z.left_back_down
                    + (     my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_z.right_front_down
                    + (1. - my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_z.left_front_down);
            }
        }
    }

    /********************************************************************************************
     *                         Simulation with 8 corners data structure                         *
     *                                  (no if, fast modulo)                                    *
     ********************************************************************************************/
#ifdef PAPI_LIB_INSTALLED
    start_diag_papi(&file_diag_papi, "diag_papi_8corners-opt.txt", papi_num_events, Events);
#endif

    time_start = omp_get_wtime();
    for (i_time = 0; i_time < num_iteration; i_time++) {
        if (0) { // DISABLED TO TEST AGAINST VERIFIED_TRANSFO
            // Diagnostics energy
            t = i_time * delta_t;
            exval_ee = landau_mult_cstt * exp(2. * omega_imag * t) *
                   (0.5 + 0.5 * cos(2. * (omega_real * t - psi)));
            switch(sim_distrib) {
                case LANDAU_1D_PROJ3D:
                    val_ee = integral_of_squared_field_3d(mesh, Ex);
                    break;
                case LANDAU_2D_PROJ3D:
                    val_ee = integral_of_squared_field_3d(mesh, Ex) + integral_of_squared_field_3d(mesh, Ey);
                    break;
                default:
                    val_ee = integral_of_squared_field_3d(mesh, Ex) + integral_of_squared_field_3d(mesh, Ey) + integral_of_squared_field_3d(mesh, Ez);
            }
            diag_energy[i_time][0] = t;                   // time
            diag_energy[i_time][1] = 0.5 * log(val_ee);   // log(Integral of squared E_field), simulated
            diag_energy[i_time][2] = 0.5 * log(exval_ee); // log(Integral of squared E_field), theoretical
            diag_energy[i_time][3] = val_ee;              // Integral of squared E_field, simulated
            diag_energy[i_time][4] = exval_ee;            // Integral of squared E_field, theoretical
        }

        time_mark1 = omp_get_wtime();

#ifdef PAPI_LIB_INSTALLED
        /* Read the counters */
        if (PAPI_read_counters(values, papi_num_events) != PAPI_OK)
            handle_error(1);
#endif

        reset_charge_3d_accumulator(ncx, ncy, ncz, num_threads, charge_accu);
        #pragma omp parallel private(thread_id, offset, my_chunk, chunkbag, next_chunk, i_color) firstprivate(nb_color_3d)
        {
            thread_id = omp_get_thread_num();
            offset = thread_id * NB_CORNERS_3D * num_cells_3d;
            // Loop on the 8 colors (in 3d), with synchronisation each time.
            for (i_color = 0; i_color < nb_color_3d; i_color++) {
                // Loop on the tiles of the grid, for the chosen color.
                #pragma omp for private(ix_min, ix_max, iy_min, iy_max, iz_min, iz_max, ix, iy, iz, i, j, corner, x, y, z, ic_x, ic_y, ic_z, i_cell) firstprivate(ncxminusone, ncyminusone, nczminusone, icell_param1, icell_param2) collapse(3)
                for (ix_min = (i_color & 1)     * OMP_TILE_SIZE; ix_min <= ncxminusone; ix_min += 2 * OMP_TILE_SIZE) {
                for (iy_min = (i_color & 2) / 2 * OMP_TILE_SIZE; iy_min <= ncyminusone; iy_min += 2 * OMP_TILE_SIZE) {
                for (iz_min = (i_color & 4) / 4 * OMP_TILE_SIZE; iz_min <= nczminusone; iz_min += 2 * OMP_TILE_SIZE) {
                    ix_max = min(ix_min + OMP_TILE_SIZE - 1, ncxminusone);
                    iy_max = min(iy_min + OMP_TILE_SIZE - 1, ncyminusone);
                    iz_max = min(iz_min + OMP_TILE_SIZE - 1, nczminusone);
                    // Nested loops on the cells of the tile.
                    for (ix = ix_min; ix <= ix_max; ix++) {
                    for (iy = iy_min; iy <= iy_max; iy++) {
                    for (iz = iz_min; iz <= iz_max; iz++) {
                        j = COMPUTE_I_CELL_3D(icell_param1, icell_param2, ix, iy, iz);
                        chunkbag = &(particles[j]);
                        for (my_chunk = chunkbag->front; my_chunk; ) {
#ifdef PIC_VERT_OPENMP_4_0
                            #pragma omp simd
#endif
                            for (i = 0; i < my_chunk->size; i++) {
                                my_chunk->vx[i] +=
                                         (     my_chunk->dx[i]) * (     my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_x.right_back_top
                                       + (1. - my_chunk->dx[i]) * (     my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_x.left_back_top
                                       + (     my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_x.right_front_top
                                       + (1. - my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_x.left_front_top
                                       + (     my_chunk->dx[i]) * (     my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_x.right_back_down
                                       + (1. - my_chunk->dx[i]) * (     my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_x.left_back_down
                                       + (     my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_x.right_front_down
                                       + (1. - my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_x.left_front_down;
                                my_chunk->vy[i] +=
                                         (     my_chunk->dx[i]) * (     my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_y.right_back_top
                                       + (1. - my_chunk->dx[i]) * (     my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_y.left_back_top
                                       + (     my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_y.right_front_top
                                       + (1. - my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_y.left_front_top
                                       + (     my_chunk->dx[i]) * (     my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_y.right_back_down
                                       + (1. - my_chunk->dx[i]) * (     my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_y.left_back_down
                                       + (     my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_y.right_front_down
                                       + (1. - my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_y.left_front_down;
                                my_chunk->vz[i] +=
                                         (     my_chunk->dx[i]) * (     my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_z.right_back_top
                                       + (1. - my_chunk->dx[i]) * (     my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_z.left_back_top
                                       + (     my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_z.right_front_top
                                       + (1. - my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (     my_chunk->dz[i]) * E_field[j].field_z.left_front_top
                                       + (     my_chunk->dx[i]) * (     my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_z.right_back_down
                                       + (1. - my_chunk->dx[i]) * (     my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_z.left_back_down
                                       + (     my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_z.right_front_down
                                       + (1. - my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * (1. - my_chunk->dz[i]) * E_field[j].field_z.left_front_down;
                            }
#    ifdef PIC_VERT_OPENMP_4_0
                            #pragma omp simd
#    endif
                            for (i = 0; i < my_chunk->size; i++) {
                                x = ((j / ncz) / ncy        ) + my_chunk->dx[i] + my_chunk->vx[i];
                                y = ((j / ncz) & ncyminusone) + my_chunk->dy[i] + my_chunk->vy[i];
                                z = (j & nczminusone        ) + my_chunk->dz[i] + my_chunk->vz[i];
                                ic_x = (int)x - (x < 0.);
                                ic_y = (int)y - (y < 0.);
                                ic_z = (int)z - (z < 0.);
                                i_cells[thread_id].array[i] = COMPUTE_I_CELL_3D(icell_param1, icell_param2, ic_x & ncxminusone, ic_y & ncyminusone, ic_z & nczminusone);
                                my_chunk->dx[i] = (float)(x - ic_x);
                                my_chunk->dy[i] = (float)(y - ic_y);
                                my_chunk->dz[i] = (float)(z - ic_z);
                            }
                            for (i = 0; i < my_chunk->size; i++) {
                                i_cell = i_cells[thread_id].array[i];
                                CHECKER_ONLY(int id = my_chunk->id[i];)
                                ic_x = ((i_cell / ncz) / ncy        );
                                ic_y = ((i_cell / ncz) & ncyminusone);
                                ic_z = (i_cell & nczminusone        );
                                if (((ic_x >= ix_min - OMP_TILE_BORDERS && ic_x <= ix_max + OMP_TILE_BORDERS) || (ix_min == 0 && ic_x >= ncx - OMP_TILE_BORDERS) || (ix_max == ncxminusone && ic_x <= OMP_TILE_BORDERS - 1)) && ((ic_y >= iy_min - OMP_TILE_BORDERS && ic_y <= iy_max + OMP_TILE_BORDERS) || (iy_min == 0 && ic_y >= ncy - OMP_TILE_BORDERS) || (iy_max == ncyminusone && ic_y <= OMP_TILE_BORDERS - 1)) && ((ic_z >= iz_min - OMP_TILE_BORDERS && ic_z <= iz_max + OMP_TILE_BORDERS) || (iz_min == 0 && ic_z >= ncz - OMP_TILE_BORDERS) || (iz_max == nczminusone && ic_z <= OMP_TILE_BORDERS - 1)))
                                    bag_push_serial(&(particlesNext[ID_PRIVATE_BAG][i_cell]), my_chunk->dx[i], my_chunk->dy[i], my_chunk->dz[i], my_chunk->vx[i], my_chunk->vy[i], my_chunk->vz[i], CHECKER_ONLY_COMMA(id) thread_id);
                                else
                                    bag_push_concurrent(&(particlesNext[ID_SHARED_BAG][i_cell]), my_chunk->dx[i], my_chunk->dy[i], my_chunk->dz[i], my_chunk->vx[i], my_chunk->vy[i], my_chunk->vz[i], CHECKER_ONLY_COMMA(id) thread_id);
#ifdef PIC_VERT_OPENMP_4_0
                                #pragma omp simd aligned(coeffs_x, coeffs_y, coeffs_z, signs_x, signs_y, signs_z:VEC_ALIGN)
#endif
                                for (corner = 0; corner < NB_CORNERS_3D; corner++) {
                                    charge_accu[offset + NB_CORNERS_3D * i_cell + corner] +=
                                        (coeffs_x[corner] + signs_x[corner] * my_chunk->dx[i]) *
                                        (coeffs_y[corner] + signs_y[corner] * my_chunk->dy[i]) *
                                        (coeffs_z[corner] + signs_z[corner] * my_chunk->dz[i]);
                                }
                            }
                            next_chunk = my_chunk->next;
                            chunk_free(my_chunk, thread_id);
                            my_chunk = next_chunk;
                        }
                    }}}
                }}}
            }
            #pragma omp single
            {
                time_mark2 = omp_get_wtime();
                compute_cumulative_free_list_sizes();
            }
            #pragma omp for private(i, j, corner, offset) schedule(static)
            for (j = 0; j < num_cells_3d; j++) {
                particles[j] = particlesNext[ID_SHARED_BAG][j];
                bag_init(&(particlesNext[ID_SHARED_BAG][j]), ID_SHARED_BAG, j, thread_id);
                bag_append(&(particles[j]), &(particlesNext[ID_PRIVATE_BAG][j]), ID_PRIVATE_BAG, j, thread_id);
                for (corner = 0; corner < NB_CORNERS_3D; corner++)
                    reduced_charge_accu[NB_CORNERS_3D * j + corner] = charge_accu[NB_CORNERS_3D * j + corner];
                for (i = 1; i < num_threads; i++) {
                    offset = i * NB_CORNERS_3D * num_cells_3d;
#ifdef PIC_VERT_OPENMP_4_0
                    #pragma omp simd
#endif
                    for (corner = 0; corner < NB_CORNERS_3D; corner++)
                        reduced_charge_accu[NB_CORNERS_3D * j + corner] += charge_accu[offset + NB_CORNERS_3D * j + corner];
                }
            }
        } // End parallel region
        update_free_list_sizes();
        time_mark3 = omp_get_wtime();

#ifdef PAPI_LIB_INSTALLED
        /* Read the counters */
        if (PAPI_read_counters(values, papi_num_events) != PAPI_OK)
            handle_error(1);
        fprintf(file_diag_papi, "%ld", i_time + 1);
        for (i = 0; i < papi_num_events; i++)
            fprintf(file_diag_papi, " %lld", values[i]);
        fprintf(file_diag_papi, "\n");
#endif

        // Converts accumulator to rho
        convert_charge_to_rho_3d_per_per(reduced_charge_accu, ncx, ncy, ncz, charge_factor, rho_3d);
        mpi_reduce_rho_3d(mpi_world_size, send_buf, recv_buf, ncx, ncy, ncz, rho_3d);
        time_mark4 = omp_get_wtime();

        // Solves Poisson and updates the field E
        for (i = 0; i < ncx + 1; i++)
            for (j = 0; j < ncy + 1; j++)
                for (k = 0; k < ncz + 1; k++)
                    q_times_rho[i][j][k] = q * rho_3d[i][j][k];
        compute_E_from_rho_3d_fft(solver, q_times_rho, Ex, Ey, Ez, 1);
        accumulate_field_3d(Ex, Ey, Ez, ncx, ncy, ncz, x_field_factor, y_field_factor, z_field_factor, E_field);
        time_mark5 = omp_get_wtime();

        // Diagnostics speed
        diag_speed[i_time][0] = time_mark1; // beginining of time loop
        diag_speed[i_time][1] = time_mark2; // after update v / x / deposit
        diag_speed[i_time][2] = time_mark3; // after append
        diag_speed[i_time][3] = time_mark4; // after all_reduce
        diag_speed[i_time][4] = time_mark5; // after Poisson solve
    }
    time_simu = (double) (omp_get_wtime() - time_start);
    time_particle_loop = 0.;
    time_append        = 0.;
    time_mpi_allreduce = 0.;
    time_poisson       = 0.;
    for (i_time = 0; i_time < num_iteration; i_time++) {
        time_particle_loop += diag_speed[i_time][1] - diag_speed[i_time][0];
        time_append        += diag_speed[i_time][2] - diag_speed[i_time][1];
        time_mpi_allreduce += diag_speed[i_time][3] - diag_speed[i_time][2];
        time_poisson       += diag_speed[i_time][4] - diag_speed[i_time][3];
    }

#ifdef PAPI_LIB_INSTALLED
    stop_diag_papi(file_diag_papi, papi_num_events, values);
#endif
    if (0) { // DISABLED TO TEST AGAINST VERIFIED_TRANSFO
        diag_energy_and_speed_chunkbags(mpi_rank,
            "diag_lee_8corners.txt",   num_iteration, diag_energy_size, diag_energy,
            "diag_speed_8corners.txt", num_iteration, diag_speed_size,  diag_speed);
        print_time_chunkbags(mpi_rank, mpi_world_size, nb_particles, num_iteration, time_simu, simulation_name, data_structure_name, sort_name,
            time_particle_loop, time_append, time_mpi_allreduce, time_poisson);
    } else {
#ifdef PRINTPERF
      printf("Exectime: %.3f sec\n", time_simu);
      printf("Throughput: %.1f million particles/sec\n", nb_particles * num_iteration / time_simu / 1000000);
#endif
    }
#ifdef CHECKER
    FILE* f = fopen(CHECKER_FILENAME, "wb");
    fwrite(&nb_particles, sizeof(int), 1, f);
    for (int j = 0; j < num_cells_3d; j++) {
      chunkbag = &particles[j];
      for (my_chunk = chunkbag->front; my_chunk; my_chunk = my_chunk->next) {
        for (i = 0; i < my_chunk->size; i++) {
            int ix = ((j / ncz) / ncy);
            int iy = ((j / ncz) & ncyminusone);
            int iz = (j & nczminusone);
            // printf("id=%d ix=%d iy=%d iz=%d dx=%f dy=%f dz=%f\n", my_chunk->id[i], ix, iy, iz, my_chunk->dx[i], my_chunk->dy[i], my_chunk->dz[i]);

            x = (ix + my_chunk->dx[i]) * mesh.delta_x + mesh.x_min;
            y = (iy + my_chunk->dy[i]) * mesh.delta_y + mesh.y_min;
            z = (iz + my_chunk->dz[i]) * mesh.delta_z + mesh.z_min;

            double vx = my_chunk->vx[i] / dt_over_dx;
            double vy = my_chunk->vy[i] / dt_over_dy;
            double vz = my_chunk->vz[i] / dt_over_dz;
#ifdef DEBUG_CHECKER
            printf("id=%d %f %f %f %g %g %g\n", my_chunk->id[i], x, y, z, vx, vy, vz);
#endif

            fwrite(&(my_chunk->id[i]), sizeof(int), 1, f);
            fwrite(&x, sizeof(double), 1, f);
            fwrite(&y, sizeof(double), 1, f);
            fwrite(&z, sizeof(double), 1, f);
            fwrite(&vx, sizeof(double), 1, f);
            fwrite(&vy, sizeof(double), 1, f);
            fwrite(&vz, sizeof(double), 1, f);
       }
      }
    }
    fclose(f);
#endif
    free(params);
    free(speed_params);
    deallocate_3d_array(q_times_rho, ncx+1, ncy+1, ncz+1);
    deallocate_3d_array(rho_3d, ncx+1, ncy+1, ncz+1);
    deallocate_3d_array(Ex, ncx+1, ncy+1, ncz+1);
    deallocate_3d_array(Ey, ncx+1, ncy+1, ncz+1);
    deallocate_3d_array(Ez, ncx+1, ncy+1, ncz+1);
    deallocate_matrix(diag_energy, num_iteration, diag_energy_size);
    deallocate_matrix(diag_speed,  num_iteration, diag_speed_size);
    free(charge_accu);
    free(reduced_charge_accu);
    deallocate_aligned_int_array_array(i_cells, num_threads);

    free(send_buf);
    free(recv_buf);
    free_poisson_3d(&solver);
    free_field_3d(E_field);
    pic_vert_free_RNG();
    MPI_Finalize();

    return 0;
}

