/**
 * PIC simulations in 2d.
 *
 * Contact:
 *   Yann Barsamian <ybarsamian@unistra.fr>
 */

//#define PAPI_LIB_INSTALLED

#include <math.h>                                         // functions cos, log
#include <mpi.h>                                          // constants MPI_COMM_WORLD, MPI_THREAD_FUNNELED
                                                          // functions MPI_Init, MPI_Finalize, MPI_Comm_size, MPI_Comm_rank
#include <omp.h>                                          // functions omp_get_wtime, omp_get_num_threads, omp_get_thread_num
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
#include "diagnostics.h"                                  // function  normL2_field_2d, get_damping_values
#include "fields.h"                                       // type      field_2d
                                                          // functions create_field_2d, free_field_2d, accumulate_field_2d
#include "initial_distributions.h"                        // constants LANDAU_1D_PROJ2D, LANDAU_2D, KELVIN_HELMHOLTZ, TWO_STREAM_BERNIER,
                                                          //           TWO_BEAMS_FIJALKOW, TWO_STREAM_1D_PROJ2D, TWO_STREAM_2D
#include "math_functions.h"                               // functions sqr, min
#include "matrix_functions.h"                             // functions allocate_matrix, deallocate_matrix, allocateMatrix, deallocateMatrix
#include "meshes.h"                                       // type      cartesian_mesh_2d, tiled_cartesian_mesh_2d
                                                          // function  create_mesh_2d, tiled_create_mesh_2d
#include "output.h"                                       // functions print_time_chunkbags, diag_energy_and_speed_chunkbags
#include "parameters.h"                                   // constants PI, EPSILON, VEC_ALIGN, DBL_DECIMAL_DIG, FLT_DECIMAL_DIG, NB_PARTICLE
#include "parameter_reader.h"                             // type      simulation_parameters
                                                          // constants STRING_NOT_SET, INT_NOT_SET, DOUBLE_NOT_SET
                                                          // function  read_parameters_from_file
#include "particle_type_concurrent_chunkbags_of_aos_2d.h" // types     particle, chunk, bag
                                                          // functions create_particle_array_2d, bag_init, bag_append init_freelists,
                                                          //           bag_push_concurrent, bag_push_serial
#include "poisson_solvers.h"                              // type      poisson_2d_solver
                                                          // function  new_poisson_2d_fft_solver, compute_E_from_rho_2d_fft, free_poisson_2d
#include "random.h"                                       // macros    pic_vert_seed_double_RNG, pic_vert_free_RNG
#include "rho.h"                                          // constant  NB_CORNERS_2D
                                                          // functions mpi_reduce_rho_2d, reset_charge_2d_accumulator, convert_charge_to_rho_2d_per_per
#include "space_filling_curves.h"                         // macro     COMPUTE_I_CELL_2D
                                                          // constant  I_CELL_2D_TYPE

/*****************************************************************************
 *                             Simulation 2d                                 *
 *****************************************************************************/

// All the simulations in this file follow the 'array of structures'
// data layout. The structure doesn't contains the weight, because it is known
// to be a constant.

const int INIT_READ = 0;
const int INIT_WRITE = 1;
const int INIT_NOFILE = 2;

// Initial distribution
// #ifndef INITIAL_DISTRIBUTION
const unsigned char INITIAL_DISTRIBUTION = LANDAU_2D;
// #endif

// #ifndef THERMAL_SPEED
const double THERMAL_SPEED = 1.;
// #endif

// Perturbation.
// #ifndef ALPHA
const double ALPHA = 0.01;
// #endif

const int ID_PRIVATE_BAG = 0;
const int ID_SHARED_BAG = 1;
const int NB_BAGS_PER_CELL = 2;

// OpenMP tiling.
// Each thread has private chunkbags for the cells in the tile
// it's working on + borders = TILE_SIZE / 2.
// We still have shared chunkbags, but they should not be used
// because particles should not move further than half-tile away.
// We use a coloring scheme to avoid races.
// #ifndef OMP_TILE_SIZE
const int OMP_TILE_SIZE = 4;
// #endif
const int OMP_TILE_BORDERS = (OMP_TILE_SIZE / 2);

int main(int argc, char** argv) {
    // Timing
    double time_start, time_simu;
    double time_mark1, time_mark2, time_mark3, time_mark4, time_mark5;
    double time_particle_loop, time_append, time_mpi_allreduce, time_poisson;
    
// #ifdef PAPI_LIB_INSTALLED
//     // Performance counters
//     int papi_num_events = 3;
//     int Events[papi_num_events];
//     Events[0] = PAPI_L1_DCM;
//     Events[1] = PAPI_L2_DCM;
//     Events[2] = PAPI_L3_TCM;
//     long_long values[papi_num_events];
//     FILE* file_diag_papi;
// #endif
    
    // Automatic values for the parameters.
    unsigned char sim_distrib = INITIAL_DISTRIBUTION; // Physical test case (LANDAU_1D_PROJ2D, TWO_BEAMS_FIJALKOW, LANDAU_2D,
                                                      // TWO_STREAM_2D, TWO_STREAM_BERNIER or TWO_STREAM_1D_PROJ2D).
    int ncx               = NCX;                      // Number of grid points, x-axis
    int ncy               = NCY;                      // Number of grid points, y-axis
    long int nb_particles = NB_PARTICLE;              // Number of particles
    int num_iteration     = NB_ITER;                  // Number of time steps
    double delta_t        = DELTA_T;                  // Time step
    double thermal_speed  = THERMAL_SPEED;            // Thermal speed
    double alpha   = ALPHA; // Landau perturbation amplitude
    double kmode_x = 0.5;   // Landau perturbation mode, x-axis
    double kmode_y = 0.5;   // Landau perturbation mode, y-axis
    
    // Read parameters from file.
    if (argc >= 2) {
        simulation_parameters parameters = read_parameters_from_file(argv[1], "2D");
        if (strcmp(parameters.sim_distrib_name, STRING_NOT_SET) != 0)
            sim_distrib = parameters.sim_distrib;
        if (parameters.ncx != INT_NOT_SET)
            ncx             = parameters.ncx;
        if (parameters.ncy != INT_NOT_SET)
            ncy             = parameters.ncy;
        if (parameters.nb_particles != INT_NOT_SET)
            nb_particles    = parameters.nb_particles;
        if (parameters.num_iteration != INT_NOT_SET)
            num_iteration   = parameters.num_iteration;
        if (parameters.delta_t != DOUBLE_NOT_SET)
            delta_t       = parameters.delta_t;
        if (parameters.thermal_speed != DOUBLE_NOT_SET)
            thermal_speed = parameters.thermal_speed;
        if (parameters.alpha != DOUBLE_NOT_SET)
            alpha   = parameters.alpha;
        if (parameters.kmode_x != DOUBLE_NOT_SET)
            kmode_x = parameters.kmode_x;
        if (parameters.kmode_y != DOUBLE_NOT_SET)
            kmode_y = parameters.kmode_y;
    } else
        printf("No parameter file was passed through the command line. I will use the default parameters.\n");
    
    // Random initialization or read from file.
    const char sim_initial = INIT_NOFILE;
    
    // Spatial parameters for initial density function.
    double *params;
    if (sim_distrib == LANDAU_1D_PROJ2D || sim_distrib == TWO_BEAMS_FIJALKOW) {
        params = (double *) malloc(2 * sizeof(double));
        params[0] = alpha;
        params[1] = kmode_x;
    } else if (sim_distrib == LANDAU_2D || sim_distrib == TWO_STREAM_2D) {
        params = (double *) malloc(3 * sizeof(double));
        params[0] = alpha;
        params[1] = kmode_x;
        params[2] = kmode_y;
    } else if (sim_distrib == TWO_STREAM_BERNIER) {
        params = (double *) malloc(7 * sizeof(double));
        params[0] = alpha;
        params[1] = kmode_x;
        params[2] = kmode_y;
        params[3] = 0.;
        params[4] = 1.;
        params[5] = 1.;
        params[6] = 1.;
    } else if (sim_distrib == TWO_STREAM_1D_PROJ2D) {
        params = (double *) malloc(5 * sizeof(double));
        params[0] = alpha;
        params[1] = kmode_x;
        params[2] = kmode_y;
        params[3] = 3.5; // i_modes_x = 1, 2, 3
        params[4] = 3.5; // i_modes_y = 1, 2, 3
    } else
        params = (double *) malloc(0 * sizeof(double));
    
    // Velocity parameters for initial density function.
    double *speed_params;
    if (sim_distrib == TWO_STREAM_1D_PROJ2D) {
        speed_params = (double *) malloc(2 * sizeof(double));
        speed_params[0] = thermal_speed;
        speed_params[1] = 2. * sqrt(2.) * thermal_speed;
    } else {
        speed_params = (double *) malloc(1 * sizeof(double));
        speed_params[0] = thermal_speed;
    }
    
    // Mesh
    double x_min, y_min, x_max, y_max;
    const int num_cells_2d = ncx * ncy;
    if (sim_distrib == LANDAU_1D_PROJ2D) {
        x_min = 0.;
        y_min = 0.;
        x_max = 2 * PI / kmode_x;
        y_max = 1.;
    } else if (sim_distrib == TWO_STREAM_1D_PROJ2D) {
        x_min = 0.;
        y_min = 0.;
        x_max = 26. * PI;
        y_max = 1.;
    } else {
        x_min = 0.;
        y_min = 0.;
        x_max = 2 * PI / kmode_x;
        y_max = 2 * PI / kmode_y;
    }
    cartesian_mesh_2d mesh = create_mesh_2d(ncx, ncy, x_min, x_max, y_min, y_max);
    const int icell_param = I_CELL_PARAM_2D(ncx, ncy);
    const int ncxminusone = ncx - 1;
    const int ncyminusone = ncy - 1;

    // Vectorization of the deposit
    int corner;
/*
 *    dx
 *   <———>
 *   +——————————*
 *   |xxx|======|
 *   |———O——————| ^
 *   |***|++++++| |
 *   |***|++++++| |dy
 *   =——————————x v
 *
 * The "=" corner is the south west corner. We deposit on this corner the fraction of the
 * particle charge corresponding to the surface of the "=" area. This fraction is equal to
 * (1. - dx) * (1. - dy), hence the SW coefficients.
 * The "+" corner is the north west corner. We deposit on this corner the fraction of the
 * particle charge corresponding to the surface of the "+" area. This fraction is equal to
 * (1. - dx) * (     dy), hence the NW coefficients.
 * The "x" corner is the south east corner. We deposit on this corner the fraction of the
 * particle charge corresponding to the surface of the "x" area. This fraction is equal to
 * (     dx) * (1. - dy), hence the SE coefficients.
 * The "*" corner is the north east corner. We deposit on this corner the fraction of the
 * particle charge corresponding to the surface of the "*" area. This fraction is equal to
 * (     dx) * (     dy), hence the NE coefficients.
 */
    // Space coeffs                                                             SW   NW   SE   NE
    // NB: const arrays are still heap allocated
    // NB: attributes disappear in clangml
    const float coeffs_x[NB_CORNERS_2D] __attribute__((aligned(VEC_ALIGN))) = {  1.,  1.,  0.,  0.};
    const float  signs_x[NB_CORNERS_2D] __attribute__((aligned(VEC_ALIGN))) = { -1., -1.,  1.,  1.};
    const float coeffs_y[NB_CORNERS_2D] __attribute__((aligned(VEC_ALIGN))) = {  1.,  0.,  1.,  0.};
    const float  signs_y[NB_CORNERS_2D] __attribute__((aligned(VEC_ALIGN))) = { -1.,  1., -1.,  1.};
    
    // Simulation parameters
    const double q           = -1.; // particle charge
    const double m           =  1.; // particle mass
    const double dt_q_over_m = delta_t * q / m;
    const double dt_over_dx  = delta_t / mesh.delta_x;
    const double dt_over_dy  = delta_t / mesh.delta_y;
    char simulation_name[42]     = "Vlasov-Poisson 2d";
    char data_structure_name[99] = "Array of Concurrent Chunkbags of AoS (1 private + 1 shared / cell)";
    char sort_name[42]           = "always sort";
    
    // MPI + OpenMP parallelism
    int mpi_world_size, mpi_rank;
    double* send_buf = allocateMatrix(ncx, ncy);
    double* recv_buf = allocateMatrix(ncx, ncy);
    int mpi_thread_support;
    // NB: missing compatibility for clangml
    // MPI_Init_thread(&argc, &argv, MPI_THREAD_FUNNELED, &mpi_thread_support);
    // MPI_Comm_size(MPI_COMM_WORLD, &mpi_world_size);
    // MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    int num_threads;
    int thread_id;
    int offset;
    // NB: missing compatibility for clangml
    // #pragma omp parallel
    // num_threads = omp_get_num_threads();
    
    // Temporary variables.
    double x, y;                                          // store the new position values
    int ic_x, ic_y, i_cell;                               // store the new position index values
    size_t i, j, i_time;                                  // loop indices
    double** q_times_rho = allocate_matrix(ncx+1, ncy+1); // to store q * rho = -rho
    
    // The following matrices are (ncx+1) * (ncy+1) arrays, with the periodicity :
    //     M[ncx][ . ] = M[0][.]
    //     M[ . ][ncy] = M[.][0]
    // rho the charge density
    // Ex the electric field on the x-axis
    // Ey the electric field on the y-axis
    double** rho_2d = allocate_matrix(ncx+1, ncy+1);
    double** Ex = allocate_matrix(ncx+1, ncy+1);
    double** Ey = allocate_matrix(ncx+1, ncy+1);
    
    // accumulators are num_cells_2d arrays : for each cell, the 4 corners values
    field_2d E_field = create_field_2d(ncx, ncy);
    /* For each cell, the 4 corners values ; for vectorization, each thread has its own copy */
    double* charge_accu;
    if (posix_memalign((void**)&charge_accu, VEC_ALIGN, num_cells_2d * NB_CORNERS_2D * num_threads * sizeof(double))) {
        fprintf(stderr, "posix_memalign failed to initialize charge_accu.\n");
	// NB: EXIT_FAILURE defined in stdlib.h will be printed as 1
        exit(EXIT_FAILURE);
    }
// NB: ignored by clangml
// #ifdef __INTEL_COMPILER
//     __assume_aligned(charge_accu, VEC_ALIGN);
// #else
//     charge_accu = __builtin_assume_aligned(charge_accu, VEC_ALIGN);
// #endif
    
    // Diagnostic energy.
    double kmode = 0.;
    switch(sim_distrib) {
        case LANDAU_1D_PROJ2D:
        case TWO_BEAMS_FIJALKOW:
            kmode = kmode_x;
            kmode++;
            break;
        case LANDAU_2D:
            kmode = sqrt(sqr(kmode_x) + sqr(kmode_y));
            break;
    }
    damping_values* landau_values = get_damping_values(kmode);
    const double er         = landau_values->er;
    const double psi        = landau_values->psi;
    const double omega_real = landau_values->omega_real;
    const double omega_imag = landau_values->omega_imag;
    double exval_ee, val_ee, t;
    const double l1d_mult_cstt = sqr(4. * alpha * er) * PI / kmode_x; // Landau
    const int diag_energy_size = 5;
    const int diag_speed_size  = 5;
    double** diag_energy = allocate_matrix(num_iteration, diag_energy_size);
    double** diag_speed  = allocate_matrix(num_iteration, diag_speed_size);
    
    // Poisson solver.
    poisson_2d_solver solver = new_poisson_2d_fft_solver(mesh);

    // Coloring
    int i_color;
    int nb_color_2d = 4;
    
    // Particle data structure.
    init_freelists(nb_particles);
    // Bags in cell i are taken from free list i % num_threads to
    // equally distribute the pressure on the different free lists.
    bag* particles = (bag *) malloc(num_cells_2d * sizeof(bag));
    for (i = 0; i < num_cells_2d; i++)
        bag_init(&(particles[i]), i % num_threads);
    bag** particlesNext = (bag **) malloc(NB_BAGS_PER_CELL * sizeof(bag*));
    for (j = 0; j < NB_BAGS_PER_CELL; j++) {
        particlesNext[j] = (bag *) malloc(num_cells_2d * sizeof(bag));
        for (i = 0; i < num_cells_2d; i++)
            bag_init(&(particlesNext[j][i]), i % num_threads);
    }
    bag* chunkbag;
    chunk* next_chunk;
    chunk* my_chunk;
    int ix_min, ix_max, iy_min, iy_max, ix, iy;
    
    /* A "numerical particle" (we also say "macro particle") represents several
     * physical particles. The weight is the number of physical particles it
     * represents. The more particles we have in the simulation, the less this
     * weight will be. A numerical particle may represent a different number of
     * physical particles than another numerical particle, even though in this
     * simulation it's not the case.
     */
    float weight;
    if (sim_initial == INIT_READ) {
        // NB: missing compatibility for clangml
        // time_start = omp_get_wtime();
        read_particle_array_2d(mpi_world_size, nb_particles, mesh, &weight, &particles);
	// NB: missing compatibility for clangml
        // if (mpi_rank == 0)
        //     printf("Read time (%ld particles) : %g sec\n", nb_particles, (double) (omp_get_wtime() - time_start));
    } else {
        pic_vert_seed_double_RNG(mpi_rank);
//         Different random numbers at each run.
//         pic_vert_seed_double_RNG(seed_64bits(mpi_rank));
        // Creation of random particles and sorting.
	// NB: missing compatibility for clangml
        // time_start = omp_get_wtime();
        create_particle_array_2d(mpi_world_size, nb_particles, mesh, sim_distrib,
            params, speed_params, &weight, &particles);
	// NB: missing compatibility for clangml
        // if (mpi_rank == 0)
        //     printf("Creation time (%ld particles) : %g sec\n", nb_particles, (double) (omp_get_wtime() - time_start));
        if (sim_initial == INIT_WRITE) {
            // Export the particles.
            char filename[30];
            sprintf(filename, "initial_particles_%ldkk.dat", nb_particles / 1000000);
            FILE* file_write_particles = fopen(filename, "w");
            fprintf(file_write_particles, "%d %d\n", ncx, ncy);
            fprintf(file_write_particles, "%ld\n", nb_particles);
            for (j = 0; j < num_cells_2d; j++) {
                chunkbag = &(particles[j]);
                for (my_chunk = chunkbag->front; my_chunk; my_chunk = my_chunk->next) {
                    for (i = 0; i < my_chunk->size; i++) {
                        fprintf(file_write_particles, "%ld %.*g %.*g %.*g %.*g\n", j,
                          FLT_DECIMAL_DIG, my_chunk->array[i].dx, FLT_DECIMAL_DIG, my_chunk->array[i].dy,
                          DBL_DECIMAL_DIG, my_chunk->array[i].vx, DBL_DECIMAL_DIG, my_chunk->array[i].vy);
                    }
                }
            }
            fclose(file_write_particles);
	    // NB: missing compatibility for clangml
            // MPI_Finalize();
            return 0;
        }
    }
    
    // Because the weight is constant, the whole array can be multiplied by weight just once.
    // Because charge is the charge MASS and not the charge DENSITY, we have to divide.
    const double charge_factor = weight / (mesh.delta_x * mesh.delta_y);
    // We just use the electric fields to update the speed, with always the same multiply.
    const double x_field_factor = dt_q_over_m * dt_over_dx;
    const double y_field_factor = dt_q_over_m * dt_over_dy;
    
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
    printf("#delta_t = %.*g\n", DBL_DECIMAL_DIG, delta_t);
    printf("#thermal_speed = %.*g\n", DBL_DECIMAL_DIG, thermal_speed);
    printf("#initial_function_case = %s\n", distribution_names_2d[sim_distrib]);
//    printf("#weight = %.*g\n", DBL_DECIMAL_DIG, weight);
//    printf("#charge_factor = %.*g\n", DBL_DECIMAL_DIG, charge_factor);
//    printf("#x_field_factor = %.*g\n", DBL_DECIMAL_DIG, x_field_factor);
//    printf("#y_field_factor = %.*g\n", DBL_DECIMAL_DIG, y_field_factor);
    printf("#ncx = %d\n", ncx);
    printf("#ncy = %d\n", ncy);
    printf("#nb_particles = %ld\n", nb_particles);
    printf("#num_iteration = %d\n", num_iteration);
  }
    
    reset_charge_2d_accumulator(ncx, ncy, num_threads, charge_accu);
    // Computes rho at initial time.
    // NB: missing compatibility for clangml
    // #pragma omp parallel private(thread_id, offset)
    {
        // NB: missing compatibility for clangml
        // thread_id = omp_get_thread_num();
        offset = thread_id * NB_CORNERS_2D * num_cells_2d;
    // NB: missing compatibility for clangml
    //     #pragma omp for private(i, j, chunkbag, my_chunk, corner)
        for (j = 0; j < num_cells_2d; j++) {
            chunkbag = &(particles[j]);
            for (my_chunk = chunkbag->front; my_chunk; my_chunk = my_chunk->next) {
                for (i = 0; i < my_chunk->size; i++) {
// NB: missing compatibility for clangml
// #ifdef PIC_VERT_OPENMP_4_0
//                     #pragma omp simd aligned(coeffs_x, coeffs_y, signs_x, signs_y:VEC_ALIGN)
// #endif
                    for (corner = 0; corner < NB_CORNERS_2D; corner++) {
                        charge_accu[offset + NB_CORNERS_2D * j + corner] +=
                            (coeffs_x[corner] + signs_x[corner] * my_chunk->array[i].dx) *
                            (coeffs_y[corner] + signs_y[corner] * my_chunk->array[i].dy);
                    }
                }
            }
        }
    } // End parallel region
    convert_charge_to_rho_2d_per_per(charge_accu, num_threads, ncx, ncy, charge_factor, rho_2d);
    mpi_reduce_rho_2d(mpi_world_size, send_buf, recv_buf, ncx, ncy, rho_2d);
    
    // Computes E at initial time.
    for (i = 0; i < ncx + 1; i++)
        for (j = 0; j < ncy + 1; j++)
            q_times_rho[i][j] = q * rho_2d[i][j];
    compute_E_from_rho_2d_fft(solver, q_times_rho, Ex, Ey);
    accumulate_field_2d(Ex, Ey, ncx, ncy, x_field_factor, y_field_factor, E_field);
    
    // Computes speeds half time-step backward (leap-frog method).
    // WARNING : starting from here, v doesn't represent the speed, but speed * dt / dx.
    // NB: missing compatibility for clangml
    // #pragma omp parallel for private(i, j, chunkbag, my_chunk)
    for (j = 0; j < num_cells_2d; j++) {
        chunkbag = &(particles[j]);
        for (my_chunk = chunkbag->front; my_chunk; my_chunk = my_chunk->next) {
// NB: missing compatibility for clangml
// #ifdef PIC_VERT_OPENMP_4_0
//             #pragma omp simd
// #endif
            for (i = 0; i < my_chunk->size; i++) {
                my_chunk->array[i].vx = my_chunk->array[i].vx * dt_over_dx - 0.5 * (
                                      (     my_chunk->array[i].dx) * (     my_chunk->array[i].dy) * E_field[j].field_x.north_east
                                    + (1. - my_chunk->array[i].dx) * (     my_chunk->array[i].dy) * E_field[j].field_x.north_west
                                    + (     my_chunk->array[i].dx) * (1. - my_chunk->array[i].dy) * E_field[j].field_x.south_east
                                    + (1. - my_chunk->array[i].dx) * (1. - my_chunk->array[i].dy) * E_field[j].field_x.south_west);
                my_chunk->array[i].vy = my_chunk->array[i].vy * dt_over_dy - 0.5 * (
                                      (     my_chunk->array[i].dx) * (     my_chunk->array[i].dy) * E_field[j].field_y.north_east
                                    + (1. - my_chunk->array[i].dx) * (     my_chunk->array[i].dy) * E_field[j].field_y.north_west
                                    + (     my_chunk->array[i].dx) * (1. - my_chunk->array[i].dy) * E_field[j].field_y.south_east
                                    + (1. - my_chunk->array[i].dx) * (1. - my_chunk->array[i].dy) * E_field[j].field_y.south_west);
            }
        }
    }
    
    /********************************************************************************************
     *                               Beginning of main time loop                                *
     ********************************************************************************************/
// #ifdef PAPI_LIB_INSTALLED
//     start_diag_papi(&file_diag_papi, "diag_papi_4corners-opt.txt", papi_num_events, Events);
// #endif
    

    // NB: missing compatibility for clangml
    // time_start = omp_get_wtime();
    for (i_time = 0; i_time < num_iteration; i_time++) {
        // Diagnostics energy
        t = i_time * delta_t;
        exval_ee = l1d_mult_cstt * exp(2. * omega_imag * t) *
               (0.5 + 0.5 * cos(2. * (omega_real * t - psi)));
        val_ee = normL2_field_2d(mesh, Ex) + normL2_field_2d(mesh, Ey);
        diag_energy[i_time][0] = t;                   // time
        diag_energy[i_time][1] = 0.5 * log(val_ee);   // E_field's log(L2-norm) (simulated)
        diag_energy[i_time][2] = 0.5 * log(exval_ee); // E_field's log(L2-norm) (expected)
        diag_energy[i_time][3] = val_ee;              // E_field's L2-norm (simulated)
        diag_energy[i_time][4] = exval_ee;            // E_field's L2-norm (expected)
        
	// NB: missing compatibility for clangml
        // time_mark1 = omp_get_wtime();
        
// #ifdef PAPI_LIB_INSTALLED
//         /* Read the counters */
//         if (PAPI_read_counters(values, papi_num_events) != PAPI_OK)
//             handle_error(1);
// #endif

        reset_charge_2d_accumulator(ncx, ncy, num_threads, charge_accu);
	// NB: missing compatibility for clangml
        // #pragma omp parallel private(thread_id, offset, my_chunk, chunkbag, next_chunk, i_color) firstprivate(nb_color_2d)
        {
	    // NB: missing compatibility for clangml
            // thread_id = omp_get_thread_num();
            offset = thread_id * NB_CORNERS_2D * num_cells_2d;
            // Loop on the 4 colors (in 2d), with synchronisation each time.
            for (i_color = 0; i_color < nb_color_2d; i_color++) {
                // Loop on the tiles of the grid, for the chosen color.
     	        // NB: missing compatibility for clangml
                // #pragma omp for private(ix_min, ix_max, iy_min, iy_max, ix, iy, i, j, corner, x, y, ic_x, ic_y, i_cell) firstprivate(ncxminusone, ncyminusone, icell_param) collapse(2)
                for (ix_min = (i_color & 1)     * OMP_TILE_SIZE; ix_min <= ncxminusone; ix_min += 2 * OMP_TILE_SIZE) {
                for (iy_min = (i_color & 2) / 2 * OMP_TILE_SIZE; iy_min <= ncyminusone; iy_min += 2 * OMP_TILE_SIZE) {
                    ix_max = min(ix_min + OMP_TILE_SIZE - 1, ncxminusone);
                    iy_max = min(iy_min + OMP_TILE_SIZE - 1, ncyminusone);
                    // Nested loops on the cells of the tile.
                    for (ix = ix_min; ix <= ix_max; ix++) {
                    for (iy = iy_min; iy <= iy_max; iy++) {
                        j = COMPUTE_I_CELL_2D(icell_param, ix, iy);
                        chunkbag = &(particles[j]);
                        // Loop on the chunks of the cell, nested with loops on the particles in those chunks.
                        for (my_chunk = chunkbag->front; my_chunk; ) {
// NB: missing compatibility for clangml
// #ifdef PIC_VERT_OPENMP_4_0
//                             #pragma omp simd
// #endif
                            for (i = 0; i < my_chunk->size; i++) {
                                my_chunk->array[i].vx +=
                                         (     my_chunk->array[i].dx) * (     my_chunk->array[i].dy) * E_field[j].field_x.north_east
                                       + (1. - my_chunk->array[i].dx) * (     my_chunk->array[i].dy) * E_field[j].field_x.north_west
                                       + (     my_chunk->array[i].dx) * (1. - my_chunk->array[i].dy) * E_field[j].field_x.south_east
                                       + (1. - my_chunk->array[i].dx) * (1. - my_chunk->array[i].dy) * E_field[j].field_x.south_west;
                                my_chunk->array[i].vy +=
                                         (     my_chunk->array[i].dx) * (     my_chunk->array[i].dy) * E_field[j].field_y.north_east
                                       + (1. - my_chunk->array[i].dx) * (     my_chunk->array[i].dy) * E_field[j].field_y.north_west
                                       + (     my_chunk->array[i].dx) * (1. - my_chunk->array[i].dy) * E_field[j].field_y.south_east
                                       + (1. - my_chunk->array[i].dx) * (1. - my_chunk->array[i].dy) * E_field[j].field_y.south_west;
                            }
                            for (i = 0; i < my_chunk->size; i++) {
                                x = (j / ncy        ) + my_chunk->array[i].dx + my_chunk->array[i].vx;
                                y = (j & ncyminusone) + my_chunk->array[i].dy + my_chunk->array[i].vy;
                                ic_x = (int)x - (x < 0.);
                                ic_y = (int)y - (y < 0.);
                                i_cell = COMPUTE_I_CELL_2D(icell_param, ic_x & ncxminusone, ic_y & ncyminusone);
                                if (ic_x >= ix_min - OMP_TILE_BORDERS && ic_x <= ix_max + OMP_TILE_BORDERS && ic_y >= iy_min - OMP_TILE_BORDERS && ic_y <= iy_max + OMP_TILE_BORDERS)
				  // NB: replace inlined particle with definition
                                  //  bag_push_serial(&(particlesNext[ID_PRIVATE_BAG][i_cell]), (particle){ .dx = (float)(x - ic_x), .dy = (float)(y - ic_y), .vx = my_chunk->array[i].vx, .vy = my_chunk->array[i].vy }, thread_id);
				  {
				    const particle p = {(float)(x - ic_x), (float)(y - ic_y), my_chunk->array[i].vx, my_chunk->array[i].vy};
				    bag_push_serial(&(particlesNext[ID_PRIVATE_BAG][i_cell]), p, thread_id);
				  }
                                else
				// NB: replace inlined particle with definition
                                //     bag_push_concurrent(&(particlesNext[ID_SHARED_BAG][i_cell]), (particle){ .dx = (float)(x - ic_x), .dy = (float)(y - ic_y), .vx = my_chunk->array[i].vx, .vy = my_chunk->array[i].vy }, thread_id);
				  {
				    const particle p = {(float)(x - ic_x), (float)(y - ic_y), my_chunk->array[i].vx, my_chunk->array[i].vy};
				    bag_push_concurrent(&(particlesNext[ID_SHARED_BAG][i_cell]), p, thread_id);
				  }
// NB: missing compatibility for clangml
// #ifdef PIC_VERT_OPENMP_4_0
//                                 #pragma omp simd aligned(coeffs_x, coeffs_y, signs_x, signs_y:VEC_ALIGN)
// #endif
                                for (corner = 0; corner < NB_CORNERS_2D; corner++) {
                                    charge_accu[offset + NB_CORNERS_2D * i_cell + corner] +=
                                        (coeffs_x[corner] + signs_x[corner] * (x - ic_x)) *
                                        (coeffs_y[corner] + signs_y[corner] * (y - ic_y));
                                }
                            }
                            next_chunk = my_chunk->next;
                            chunk_free(my_chunk, thread_id);
                            my_chunk = next_chunk;
                        }
                    }}
                }}
            }
	    // NB: missing compatibility for clangml
            // time_mark2 = omp_get_wtime();
        //     #pragma omp for private(j)
            for (j = 0; j < num_cells_2d; j++) {
                particles[j] = particlesNext[ID_PRIVATE_BAG][j];
                bag_init(&(particlesNext[ID_PRIVATE_BAG][j]), thread_id);
                bag_append(&(particles[j]), &(particlesNext[ID_SHARED_BAG][j]), thread_id);
            }
        } // End parallel region
	// NB: missing compatibility for clangml
        // time_mark3 = omp_get_wtime();
        
// #ifdef PAPI_LIB_INSTALLED
//         /* Read the counters */
//         if (PAPI_read_counters(values, papi_num_events) != PAPI_OK)
//             handle_error(1);
//         fprintf(file_diag_papi, "%ld", i_time + 1);
//         for (i = 0; i < papi_num_events; i++)
//             fprintf(file_diag_papi, " %lld", values[i]);
//         fprintf(file_diag_papi, "\n");
// #endif
        
        // Converts accumulator to rho
        convert_charge_to_rho_2d_per_per(charge_accu, num_threads, ncx, ncy, charge_factor, rho_2d);
        mpi_reduce_rho_2d(mpi_world_size, send_buf, recv_buf, ncx, ncy, rho_2d);
	// NB: missing compatibility for clangml
        // time_mark4 = omp_get_wtime();
        
        // Solves Poisson and updates the field E
        for (i = 0; i < ncx + 1; i++)
            for (j = 0; j < ncy + 1; j++)
                q_times_rho[i][j] = q * rho_2d[i][j];
        compute_E_from_rho_2d_fft(solver, q_times_rho, Ex, Ey);
        accumulate_field_2d(Ex, Ey, ncx, ncy, x_field_factor, y_field_factor, E_field);
	// NB: missing compatibility for clangml
        // time_mark5 = omp_get_wtime();
        
        // Diagnostics speed
        diag_speed[i_time][0] = time_mark1; // beginining of time loop
        diag_speed[i_time][1] = time_mark2; // after update v / x / deposit
        diag_speed[i_time][2] = time_mark3; // after append
        diag_speed[i_time][3] = time_mark4; // after all_reduce
        diag_speed[i_time][4] = time_mark5; // after Poisson solve
    }
    // NB: missing compatibility for clangml
    // time_simu = (double) (omp_get_wtime() - time_start);
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
    
// #ifdef PAPI_LIB_INSTALLED
//     stop_diag_papi(file_diag_papi, papi_num_events, values);
// #endif
    diag_energy_and_speed_chunkbags(mpi_rank,
        "diag_lee_4corners.txt",   num_iteration, diag_energy_size, diag_energy,
        "diag_speed_4corners.txt", num_iteration, diag_speed_size,  diag_speed);
    print_time_chunkbags(mpi_rank, mpi_world_size, nb_particles, num_iteration, time_simu, simulation_name, data_structure_name, sort_name,
        time_particle_loop, time_append, time_mpi_allreduce, time_poisson);
    
    free(params);
    free(speed_params);
    deallocate_matrix(q_times_rho, ncx+1, ncy+1);
    deallocate_matrix(rho_2d, ncx+1, ncy+1);
    deallocate_matrix(Ex, ncx+1, ncy+1);
    deallocate_matrix(Ey, ncx+1, ncy+1);
    deallocate_matrix(diag_energy, num_iteration, diag_energy_size);
    deallocate_matrix(diag_speed,  num_iteration, diag_speed_size);
    free(charge_accu);
    
    free(send_buf);
    free(recv_buf);
    free_poisson_2d(&solver);
    free_field_2d(E_field);
    pic_vert_free_RNG();
    // NB: missing compatibility for clangml
    // MPI_Finalize();
    
    return 0;
}

