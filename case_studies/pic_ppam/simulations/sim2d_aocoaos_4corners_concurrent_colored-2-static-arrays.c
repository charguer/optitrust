/**
 * sim2d_aocoaos_4corners_concurrent_colored-2-static-arrays.c: PIC simulations in 2d.
 *
 * Compilation :
 *     compile_aocoaos_2d.sh
 *
 * Launching the simulation :
 *     sim_aocoaos_2d.sh
 *
 * Contact:
 *   Yann Barsamian <ybarsamian@unistra.fr>
 */

//#define PAPI_LIB_INSTALLED

#include <stdio.h>                                        // function  printf, fprintf (output strings on a stream)
                                                          // constant  stderr (standard error output stream)
#include <stdlib.h>                                       // functions srand48 (random generator)
                                                          //           malloc, free ((de)allocate memory)
                                                          //           exit (error handling)
                                                          // constant  EXIT_FAILURE (error handling)
                                                          // type      size_t
#include <math.h>                                         // functions cos, log, fabs
#include <time.h>                                         // function  time (for random seed initialization)
#include <omp.h>                                          // functions omp_get_wtime, omp_get_num_threads, omp_get_thread_num
#ifdef PAPI_LIB_INSTALLED
#include <papi.h>                                         // constants PAPI_OK, PAPI_L1_DCM, PAPI_L2_DCM, PAPI_L3_TCM...
                                                          // functions PAPI_read_counters
                                                          // type      long_long
#include "papi_handlers.h"
#endif
#include <float.h>                                        // constant  DBL_DECIMAL_DIG (17 : number of decimals a double has, only in C11)
                                                          // constant  FLT_DECIMAL_DIG ( 9 : number of decimals a flaot  has, only in C11)
// According to https://en.wikipedia.org/wiki/Floating_point#Internal_representation
#include <mpi.h>                                          // constants MPI_COMM_WORLD, MPI_THREAD_FUNNELED
                                                          // functions MPI_Init, MPI_Finalize, MPI_Comm_size, MPI_Comm_rank
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
#include "parameters.h"                                   // constants PI, EPSILON, VEC_ALIGN
#include "particle_type_concurrent_chunkbags_of_aos_2d.h" // types     particle, chunk, bag
                                                          // functions create_particle_array_2d, bag_init, bag_append init_freelists,
                                                          //           bag_push_concurrent, bag_push_serial
                                                          // constants NB_PARTICLE, NB_PROC
#include "poisson_solvers.h"                              // type      poisson_2d_solver
                                                          // function  new_poisson_2d_fft_solver, compute_E_from_rho_2d_fft, free_poisson_2d
#include "rho.h"                                          // constant  NB_CORNERS_2D
                                                          // functions mpi_reduce_rho_2d, reset_charge_2d_accumulator, convert_charge_to_rho_2d_per_per
#include "space_filling_curves.h"                         // macro     COMPUTE_I_CELL_2D
                                                          // constant  I_CELL_2D_TYPE

/*****************************************************************************
 *                             Simulation 2d                                 *
 *                                                                           *
 * selalib/src/simulations/parallel/pic_vp_2d2v_cart_optim_push/             *
 *     sll_m_sim_pic_vp_2d2v_cart_optim_push.F90                             *
 *****************************************************************************/

// All the simulations in this file follow the 'array of structures'
// data layout. The structure doesn't contains the weight, because it is known
// to be a constant.

#define INIT_READ   0
#define INIT_WRITE  1
#define INIT_NOFILE 2

#define ID_PRIVATE_BAG   0
#define ID_SHARED_BAG    1
#define NB_BAGS_PER_CELL 2

// OpenMP tiling.
// Each thread has private chunkbags for the cells in the tile
// it's working on + borders = TILE_SIZE / 2.
// We still have shared chunkbags, but they should not be used
// because particles should not move further than half-tile away.
// We use a coloring scheme to avoid races.
#ifndef OMP_TILE_SIZE
#   define OMP_TILE_SIZE 4
#endif
#define OMP_TILE_BORDERS (OMP_TILE_SIZE / 2)

// Time step.
#ifndef DELTA_T
#   define DELTA_T 0.1
#endif

// Grid size.
#ifndef NCX
#   define NCX 128
#endif
#ifndef NCY
#   define NCY 128
#endif

int main(int argc, char** argv) {
    // Timing
    double time_start, time_simu;
    double time_mark1, time_mark2, time_mark3, time_mark4, time_mark5;
    double time_particle_loop, time_append, time_mpi_allreduce, time_poisson;
    
#ifdef PAPI_LIB_INSTALLED
    // Performance counters
    int Events[NUM_EVENTS] = {PAPI_L1_DCM, PAPI_L2_DCM, PAPI_L3_TCM};
    long_long values[NUM_EVENTS];
    FILE* file_diag_papi;
#endif
    
    // Type of the simulation
    const unsigned char sim_distrib = LANDAU_2D;
    const char sim_initial = INIT_NOFILE;

    // Electric parameters, for the perturbation :
    const double kmode_x = 0.5;
    const double kmode_y = 0.5;
    const double alpha   = 0.01;
    double *params;
    if (sim_distrib == LANDAU_1D_PROJ2D || sim_distrib == TWO_BEAMS_FIJALKOW) {
        params = malloc(2 * sizeof(double));
        params[0] = alpha;
        params[1] = kmode_x;
    } else if (sim_distrib == LANDAU_2D || sim_distrib == TWO_STREAM_2D) {
        params = malloc(3 * sizeof(double));
        params[0] = alpha;
        params[1] = kmode_x;
        params[2] = kmode_y;
    } else if (sim_distrib == TWO_STREAM_BERNIER) {
        params = malloc(7 * sizeof(double));
        params[0] = alpha;
        params[1] = kmode_x;
        params[2] = kmode_y;
        params[3] = 0.;
        params[4] = 1.;
        params[5] = 1.;
        params[6] = 1.;
    } else if (sim_distrib == TWO_STREAM_1D_PROJ2D) {
        params = malloc(5 * sizeof(double));
        params[0] = alpha;
        params[1] = kmode_x;
        params[2] = kmode_y;
        params[3] = 3.5; // i_modes_x = 1, 2, 3
        params[4] = 3.5; // i_modes_y = 1, 2, 3
    }
    
    // Other physical parameters
    const double q             = -1.;            // particle charge
    const double m             =  1.;            // particle mass
    const double thermal_speed =  1.;            // thermal speed
    double *speed_params;
    if (sim_distrib == TWO_STREAM_1D_PROJ2D) {
        speed_params = malloc(2 * sizeof(double));
        speed_params[0] = thermal_speed;
        speed_params[1] = 2. * sqrt(2.) * thermal_speed;
    } else {
        speed_params = malloc(1 * sizeof(double));
        speed_params[0] = thermal_speed;
    }
    
    // Mesh
    double x_min, y_min, x_max, y_max;
    const int ncx = NCX;
    const int ncy = NCY;
    const int num_cells_2d = ncx * ncy;
    if (sim_distrib == LANDAU_1D_PROJ2D) {
        x_min = 0.;
        y_min = 0.;
        x_max = 2 * PI / kmode_x;
        y_max = 1.;
    } else if (sim_distrib == LANDAU_2D || sim_distrib == TWO_STREAM_BERNIER || sim_distrib == TWO_BEAMS_FIJALKOW || sim_distrib == TWO_STREAM_2D) {
        x_min = 0.;
        y_min = 0.;
        x_max = 2 * PI / kmode_x;
        y_max = 2 * PI / kmode_y;
    } else if (sim_distrib == TWO_STREAM_1D_PROJ2D) {
        x_min = 0.;
        y_min = 0.;
        x_max = 26. * PI;
        y_max = 1.;
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
    const float coeffs_x[NB_CORNERS_2D] __attribute__((aligned(VEC_ALIGN))) = {  1.,  1.,  0.,  0.};
    const float  signs_x[NB_CORNERS_2D] __attribute__((aligned(VEC_ALIGN))) = { -1., -1.,  1.,  1.};
    const float coeffs_y[NB_CORNERS_2D] __attribute__((aligned(VEC_ALIGN))) = {  1.,  0.,  1.,  0.};
    const float  signs_y[NB_CORNERS_2D] __attribute__((aligned(VEC_ALIGN))) = { -1.,  1., -1.,  1.};
    
    // Simulation parameters
    const double delta_t             = DELTA_T;           // time step
    const double dt_q_over_m         = delta_t * q / m;
    const double dt_over_dx          = delta_t / mesh.delta_x;
    const double dt_over_dy          = delta_t / mesh.delta_y;
    const unsigned int num_iteration = 100;           // number of time steps
    char simulation_name[42]     = "Vlasov-Poisson 2d";
    char data_structure_name[99] = "Array of Concurrent Chunkbags of AoS (1 private + 1 shared / cell)";
    char sort_name[42]           = "always sort";
    
    // MPI + OpenMP parallelism
    int mpi_world_size, mpi_rank;
    double* send_buf = allocateMatrix(ncx, ncy);
    double* recv_buf = allocateMatrix(ncx, ncy);
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
        exit(EXIT_FAILURE);
    }
#ifdef __INTEL_COMPILER
    __assume_aligned(charge_accu, VEC_ALIGN);
#else
    charge_accu = __builtin_assume_aligned(charge_accu, VEC_ALIGN);
#endif
    
    // Diagnostic energy.
    double kmode = 0.;
    switch(sim_distrib) {
        case LANDAU_1D_PROJ2D:
        case TWO_BEAMS_FIJALKOW:
            kmode = kmode_x;
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
    double** diag_energy = allocate_matrix(num_iteration, 5);
    double** diag_speed  = allocate_matrix(num_iteration, 5);
    FILE* file_diag_energy;
    FILE* file_diag_speed;
    
    // Poisson solver.
    poisson_2d_solver solver = new_poisson_2d_fft_solver(mesh);

    // Coloring
    int i_color;
    int nb_color_2d = 4;
    
    // Particle data structure.
    init_freelists();
    // Bags in cell i are taken from free list i % NB_PROC to
    // equally distribute the pressure on the different free lists.
    bag* particles = malloc(num_cells_2d * sizeof(bag));
    for (i = 0; i < num_cells_2d; i++)
        bag_init(&(particles[i]), i % NB_PROC);
    bag** particlesNext = malloc(NB_BAGS_PER_CELL * sizeof(bag*));
    for (j = 0; j < NB_BAGS_PER_CELL; j++) {
        particlesNext[j] = malloc(num_cells_2d * sizeof(bag));
        for (i = 0; i < num_cells_2d; i++)
            bag_init(&(particlesNext[j][i]), i % NB_PROC);
    }
    bag* bag;
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
        time_start = omp_get_wtime();
        read_particle_array_2d(mpi_world_size, NB_PARTICLE, mesh, &weight, &particles);
        if (mpi_rank == 0)
            printf("Read time (%d particles) : %g sec\n", NB_PARTICLE, (double) (omp_get_wtime() - time_start));
    } else {
        // Different random numbers at each run ; can also use the seed from /dev/random.
        srand48((long)mpi_rank);
//        srand48((long)time(NULL) + (long)mpi_rank);
        // Creation of random particles and sorting.
        time_start = omp_get_wtime();
        create_particle_array_2d(mpi_world_size, NB_PARTICLE, mesh, sim_distrib,
            params, speed_params, &weight, &particles);
        if (mpi_rank == 0)
            printf("Creation time (%d particles) : %g sec\n", NB_PARTICLE, (double) (omp_get_wtime() - time_start));
        if (sim_initial == INIT_WRITE) {
            // Export the particles.
            char filename[30];
            sprintf(filename, "initial_particles_%dkk.dat", NB_PARTICLE / 1000000);
            FILE* file_write_particles = fopen(filename, "w");
            fprintf(file_write_particles, "%d %d\n", ncx, ncy);
            fprintf(file_write_particles, "%d\n", NB_PARTICLE);
            for (j = 0; j < num_cells_2d; j++) {
                bag = &(particles[j]);
                for (my_chunk = bag->front; my_chunk != NULL; my_chunk = my_chunk->next) {
                    for (i = 0; i < my_chunk->size; i++) {
                        fprintf(file_write_particles, "%ld %.*g %.*g %.*g %.*g\n", j,
                          FLT_DECIMAL_DIG, my_chunk->array[i].dx, FLT_DECIMAL_DIG, my_chunk->array[i].dy,
                          DBL_DECIMAL_DIG, my_chunk->array[i].vx, DBL_DECIMAL_DIG, my_chunk->array[i].vy);
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
    const double charge_factor = weight / (mesh.delta_x * mesh.delta_y);
    // We just use the electric fields to update the speed, with always the same multiply.
    const double x_field_factor = dt_q_over_m * dt_over_dx;
    const double y_field_factor = dt_q_over_m * dt_over_dy;
    
  if (mpi_rank == 0) {
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
    printf("#NB_PARTICLE = %d\n", NB_PARTICLE);
    printf("#num_iteration = %d\n", num_iteration);
  }
    
    reset_charge_2d_accumulator(ncx, ncy, num_threads, charge_accu);
    // Computes rho at initial time.
    #pragma omp parallel private(thread_id, offset)
    {
        thread_id = omp_get_thread_num();
        offset = thread_id * NB_CORNERS_2D * num_cells_2d;
        #pragma omp for private(i, j, bag, my_chunk, corner)
        for (j = 0; j < num_cells_2d; j++) {
            bag = &(particles[j]);
            for (my_chunk = bag->front; my_chunk != NULL; my_chunk = my_chunk->next) {
                for (i = 0; i < my_chunk->size; i++) {
#ifdef PIC_VERT_OPENMP_4_0
                    #pragma omp simd aligned(coeffs_x, coeffs_y, signs_x, signs_y:VEC_ALIGN)
#endif
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
    #pragma omp for private(i, j, bag, my_chunk)
    for (j = 0; j < num_cells_2d; j++) {
        bag = &(particles[j]);
        for (my_chunk = bag->front; my_chunk != NULL; my_chunk = my_chunk->next) {
#ifdef PIC_VERT_OPENMP_4_0
            #pragma omp simd
#endif
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
#ifdef PAPI_LIB_INSTALLED
    start_diag_papi(&file_diag_papi, "diag_papi_4corners-opt.txt", Events);
#endif
    
    time_start = omp_get_wtime();
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
        
        time_mark1 = omp_get_wtime();
        
#ifdef PAPI_LIB_INSTALLED
        /* Read the counters */
        if (PAPI_read_counters(values, NUM_EVENTS) != PAPI_OK)
            handle_error(1);
#endif

        reset_charge_2d_accumulator(ncx, ncy, num_threads, charge_accu);
        #pragma omp parallel private(thread_id, offset, my_chunk, bag, next_chunk, i_color) firstprivate(nb_color_2d)
        {
            thread_id = omp_get_thread_num();
            offset = thread_id * NB_CORNERS_2D * num_cells_2d;
            // Loop on the 4 colors (in 2d), with synchronisation each time.
            for (i_color = 0; i_color < nb_color_2d; i_color++) {
                // Loop on the tiles of the grid, for the chosen color.
                #pragma omp for private(ix_min, ix_max, iy_min, iy_max, ix, iy, i, j, corner, x, y, ic_x, ic_y, i_cell) firstprivate(ncxminusone, ncyminusone, icell_param) collapse(2)
                for (ix_min = (i_color & 1)     * OMP_TILE_SIZE; ix_min <= ncxminusone; ix_min += 2 * OMP_TILE_SIZE) {
                for (iy_min = (i_color & 2) / 2 * OMP_TILE_SIZE; iy_min <= ncyminusone; iy_min += 2 * OMP_TILE_SIZE) {
                    ix_max = min(ix_min + OMP_TILE_SIZE - 1, ncxminusone);
                    iy_max = min(iy_min + OMP_TILE_SIZE - 1, ncyminusone);
                    // Nested loops on the cells of the tile.
                    for (ix = ix_min; ix <= ix_max; ix++) {
                    for (iy = iy_min; iy <= iy_max; iy++) {
                        j = COMPUTE_I_CELL_2D(icell_param, ix, iy);
                        bag = &(particles[j]);
                        // Loop on the chunks of the cell, nested with loops on the particles in those chunks.
                        for (my_chunk = bag->front; my_chunk != NULL; ) {
#ifdef PIC_VERT_OPENMP_4_0
                            #pragma omp simd
#endif
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
                                    bag_push_serial(&(particlesNext[ID_PRIVATE_BAG][i_cell]), (particle){ .dx = (float)(x - ic_x), .dy = (float)(y - ic_y), .vx = my_chunk->array[i].vx, .vy = my_chunk->array[i].vy }, thread_id);
                                else
                                    bag_push_concurrent(&(particlesNext[ID_SHARED_BAG][i_cell]), (particle){ .dx = (float)(x - ic_x), .dy = (float)(y - ic_y), .vx = my_chunk->array[i].vx, .vy = my_chunk->array[i].vy }, thread_id);
#ifdef PIC_VERT_OPENMP_4_0
                                #pragma omp simd aligned(coeffs_x, coeffs_y, signs_x, signs_y:VEC_ALIGN)
#endif
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
            time_mark2 = omp_get_wtime();
            #pragma omp for private(j)
            for (j = 0; j < num_cells_2d; j++) {
                particles[j] = particlesNext[ID_PRIVATE_BAG][j];
                bag_init(&(particlesNext[ID_PRIVATE_BAG][j]), thread_id);
                bag_append(&(particles[j]), &(particlesNext[ID_SHARED_BAG][j]), thread_id);
            }
        } // End parallel region
        time_mark3 = omp_get_wtime();

        // Testing purposes only
/*
        int nb_part_tot = 0;
        for (j = 0; j < num_cells_2d; j++) {
            nb_part_tot += bag_size(&(particles[j]));
        }
        printf("#Iteration %ld: nb_part_tot = %d\n", i_time + 1, nb_part_tot);
*/
        
#ifdef PAPI_LIB_INSTALLED
        /* Read the counters */
        if (PAPI_read_counters(values, NUM_EVENTS) != PAPI_OK)
            handle_error(1);
        fprintf(file_diag_papi, "%ld", i_time + 1);
        for (i = 0; i < NUM_EVENTS; i++)
            fprintf(file_diag_papi, " %lld", values[i]);
        fprintf(file_diag_papi, "\n");
#endif

        // Converts accumulator to rho
        convert_charge_to_rho_2d_per_per(charge_accu, num_threads, ncx, ncy, charge_factor, rho_2d);
        mpi_reduce_rho_2d(mpi_world_size, send_buf, recv_buf, ncx, ncy, rho_2d);
        time_mark4 = omp_get_wtime();
        
        // Solves Poisson and updates the field E
        for (i = 0; i < ncx + 1; i++)
            for (j = 0; j < ncy + 1; j++)
                q_times_rho[i][j] = q * rho_2d[i][j];
        compute_E_from_rho_2d_fft(solver, q_times_rho, Ex, Ey);
        accumulate_field_2d(Ex, Ey, ncx, ncy, x_field_factor, y_field_factor, E_field);
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
    stop_diag_papi(file_diag_papi, values);
#endif
    diag_energy_and_speed_chunkbags(mpi_rank, num_iteration,
        &file_diag_energy, "diag_lee_4corners-opt-1loop.txt",   diag_energy,
        &file_diag_speed,  "diag_speed_4corners-opt-1loop.txt", diag_speed);
    print_time_chunkbags(mpi_rank, mpi_world_size, NB_PARTICLE, num_iteration, time_simu, simulation_name, data_structure_name, sort_name,
        time_particle_loop, time_append, time_mpi_allreduce, time_poisson);
    
    free(params);
    deallocate_matrix(q_times_rho, ncx+1, ncy+1);
    deallocate_matrix(rho_2d, ncx+1, ncy+1);
    deallocate_matrix(Ex, ncx+1, ncy+1);
    deallocate_matrix(Ey, ncx+1, ncy+1);
    deallocate_matrix(diag_energy, num_iteration, 5);
    deallocate_matrix(diag_speed, num_iteration, 5);

    free(send_buf);
    free(recv_buf);
    free_poisson_2d(&solver);
    free_field_2d(E_field);
    MPI_Finalize();
    
    return 0;
}

