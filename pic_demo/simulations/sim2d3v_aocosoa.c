/**
 * sim2d3v_aocosoa.c: PIC simulations in 2d3v.
 *
 * Compilation :
 *     Pic-Vert/scripts/2d3v_performance
 *
 * Contact:
 *   Yann Barsamian <ybarsamian@unistra.fr>
 */

//#define PAPI_LIB_INSTALLED

#include <omp.h>                                            // functions omp_get_wtime, omp_get_num_threads, omp_get_thread_num
#include <math.h>                                           // functions cos, log
#include <mpi.h>                                            // constants MPI_COMM_WORLD, MPI_THREAD_FUNNELED
                                                            // functions MPI_Init, MPI_Finalize, MPI_Comm_size, MPI_Comm_rank
#include <stdio.h>                                          // functions printf, fprintf, fgets, fopen
                                                            // constant  stderr (standard error output stream)
#include <stdlib.h>                                         // functions malloc, free ((de)allocate memory)
                                                            //           exit (error handling)
                                                            // constant  EXIT_FAILURE (error handling)
                                                            // type      size_t
#include <string.h>                                         // function  strcmp
#ifdef PAPI_LIB_INSTALLED
#    include <papi.h>                                       // constants PAPI_OK, PAPI_L1_DCM, PAPI_L2_DCM, PAPI_L3_TCM...
                                                            // functions PAPI_read_counters
                                                            // type      long_long
#    include "papi_handlers.h"                              // functions start_diag_papi, stop_diag_papi
#endif
#include "compiler_test.h"                                  // constant  PIC_VERT_OPENMP_4_0
#include "diagnostics.h"                                    // functions get_damping_values, integral_of_squared_field_2d, energy_fourier_mode_2d
#include "fields.h"                                         // type      field_2d
                                                            // functions create_field_2d, free_field_2d, accumulate_field_2d
#include "hdf5_io.h"                                        // function  plot_f_cartesian_mesh_2d
#include "initial_distributions.h"                          // constants ELECTRON_HOLES_2D3V, BI_MAXWELLIAN_2D3V, LANDAU_1D_PROJ2D3V
#include "math_functions.h"                                 // functions sqr, min, is_power_of_two
#include "matrix_functions.h"                               // functions allocate_matrix, allocate_aligned_double_matrix, deallocate_matrix
                                                            //           allocateMatrix, deallocateMatrix
#include "meshes.h"                                         // type      cartesian_mesh_2d
                                                            // function  create_mesh_2d
#include "output.h"                                         // functions print_time_chunkbags, diag_energy_and_speed_chunkbags, print_time_left
#include "parameter_reader.h"                               // types     couple, phase_space_position, simulation_parameters
                                                            // constants PICVERT_MAX_NB_FOURIER_MODES, STRING_NOT_SET, INT_NOT_SET, DOUBLE_NOT_SET
                                                            // function  read_parameters_from_file
#include "parameters.h"                                     // constants PI, EPSILON, VEC_ALIGN, DBL_DECIMAL_DIG, FLT_DECIMAL_DIG, NB_PARTICLE
#include "particle_type_concurrent_chunkbags_of_soa_2d3v.h" // types     chunk, bag
                                                            // functions create_particle_array_2d3v, init_all_chunks, bag_push_concurrent, bag_push_serial,
                                                            //           bag_init, bag_append
#include "poisson_solvers.h"                                // type      poisson_2d_solver
                                                            // function  new_poisson_2d_fft_solver, compute_E_from_rho_2d_fft, free_poisson_2d
#include "random.h"                                         // macros    pic_vert_seed_double_RNG, pic_vert_free_RNG
#include "rho.h"                                            // constant  NB_CORNERS_2D
                                                            // functions mpi_reduce_rho_2d, reset_charge_2d_accumulator, convert_charge_to_rho_2d_per_per
#include "space_filling_curves.h"                           // macro     COMPUTE_I_CELL_2D
                                                            // constant  I_CELL_2D_TYPE


typedef struct simulation_variables simulation_variables;
struct simulation_variables {
    int num_iteration;
    double delta_t;
    double q;
    double dt_over_dx;
    double dt_over_dy;
    cartesian_mesh_2d mesh;
};

typedef struct diag_energy_variables diag_energy_variables;
struct diag_energy_variables {
    unsigned char sim_distrib;
    int diag_nb_outputs;
    int diag_delta_step_output;
    damping_values* landau_values;
    double landau_mult_cstt;
    int nb_mpi_diagnostics; // Kinetic energy, momentum in each of 3 directions
    double* my_diagnostics; // Values of diagnostics local to each MPI process
    double* diagnostics;    // MPI reduction of all diagnostics
    double** local_diagnostics;
    double** sum;
    double** compensation;
    double** sum2;
    double** term_plus_compensation;
    FILE* file_diag_energy_update;
    float weight_xy;
    int nb_fourier_modes;
    couple* fourier_modes;
    double s_half_step_forward;
    double c_half_step_forward;
};

typedef struct diag_hdf5_variables diag_hdf5_variables;
struct diag_hdf5_variables {
    int hdf5_nb_outputs;
    int hdf5_delta_step_output;
    cartesian_mesh_2d mesh_xvx;
    double* send_buf_hdf5;
    double* recv_buf_hdf5;
    float weight_x_vx;
};

// #define PIC_VERT_TEST_INITIAL
// #define PIC_VERT_KINETIC_ENERGY
#define PIC_VERT_OUTPUT_X_VX

void diagnostics(size_t i_time, int mpi_rank, int mpi_world_size,
        double** Ex, double** Ey, field_2d E_field, double** rho_2d, bag* particles,
        diag_energy_variables* energy_variables, diag_hdf5_variables* hdf5_variables, simulation_variables* sim_variables,
        double** diag_energy, int diag_energy_size) {
    size_t i, j;
    double time;
    bag* chunkbag;
    chunk* my_chunk;
    time = sim_variables->delta_t * i_time;
    double q = sim_variables->q;
    double dt_over_dx = sim_variables->dt_over_dx;
    double dt_over_dy = sim_variables->dt_over_dy;
    cartesian_mesh_2d mesh = sim_variables->mesh;
    int ncx = mesh.num_cell_x;
    int ncy = mesh.num_cell_y;
    int num_cells_2d = ncx * ncy;
    
    // Diagnostics energy
    if ((i_time % energy_variables->diag_delta_step_output == 0) || (i_time == sim_variables->num_iteration)) {
        double exval_ee, val_ee;
        int i_diag_output = (i_time == sim_variables->num_iteration)
            ? energy_variables->diag_nb_outputs - 1
            : i_time / energy_variables->diag_delta_step_output;
        
        diag_energy[i_diag_output][0] = time;                              // time
        switch(energy_variables->sim_distrib) {
            case LANDAU_1D_PROJ2D3V:
                exval_ee = energy_variables->landau_mult_cstt * exp(2. * energy_variables->landau_values->omega_imag * time) *
                       (0.5 + 0.5 * cos(2. * (energy_variables->landau_values->omega_real * time - energy_variables->landau_values->psi)));
                val_ee = integral_of_squared_field_2d(mesh, Ex);
                diag_energy[i_diag_output][1] = 0.5 * log(val_ee);         // log(Integral of squared Ex_field), simulated
                diag_energy[i_diag_output][2] = 0.5 * log(exval_ee);       // log(Integral of squared Ex_field), theoretical
                diag_energy[i_diag_output][3] = val_ee;                    // Integral of squared Ex_field, simulated
                diag_energy[i_diag_output][4] = exval_ee;                  // Integral of squared Ex_field, theoretical
                break;
            default:
                diag_energy[i_diag_output][1] = integral_of_squared_field_2d(mesh, Ex); // Integral of squared Ex_field, simulated
                diag_energy[i_diag_output][2] = integral_of_squared_field_2d(mesh, Ey); // Integral of squared Ey_field, simulated
#ifdef PIC_VERT_KINETIC_ENERGY
                size_t k;
                double s_half_step_forward = energy_variables->s_half_step_forward;
                double c_half_step_forward = energy_variables->c_half_step_forward;
                double ey_field_half_step_accel, vy_minus;
                double vx_local, vy_local, vz_local;
                int num_threads;
                int thread_id;
                // Computes kinetic energy. Uses Kahan Summation Formula, cf.
                // Kahan, "Further remarks on reducing truncation errors", 1965
                // https://en.wikipedia.org/wiki/Kahan_summation_algorithm
                #pragma omp parallel private(k, thread_id)
                {
                    thread_id = omp_get_thread_num();
                    num_threads = omp_get_num_threads();
                    for (k = 0; k < energy_variables->nb_mpi_diagnostics; k++) {
                        energy_variables->sum[thread_id][k]          = 0.;
                        energy_variables->compensation[thread_id][k] = 0.;
                    }
                    #pragma omp for private(i, j, chunkbag, my_chunk, ey_field_half_step_accel, vy_minus, vx_local, vy_local, vz_local)
                    for (j = 0; j < num_cells_2d; j++) {
                        chunkbag = &(particles[j]);
                        for (my_chunk = chunkbag->front; my_chunk; my_chunk = my_chunk->next) {
                            for (i = 0; i < my_chunk->size; i++) {
                                vx_local = my_chunk->vx[i] + 0.5 * (
                                         (     my_chunk->dx[i]) * (     my_chunk->dy[i]) * E_field[j].field_x.north_east
                                       + (1. - my_chunk->dx[i]) * (     my_chunk->dy[i]) * E_field[j].field_x.north_west
                                       + (     my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * E_field[j].field_x.south_east
                                       + (1. - my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * E_field[j].field_x.south_west);
                                ey_field_half_step_accel = 0.25 * (
                                         (     my_chunk->dx[i]) * (     my_chunk->dy[i]) * E_field[j].field_y.north_east
                                       + (1. - my_chunk->dx[i]) * (     my_chunk->dy[i]) * E_field[j].field_y.north_west
                                       + (     my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * E_field[j].field_y.south_east
                                       + (1. - my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * E_field[j].field_y.south_west);
                                vy_minus = my_chunk->vy[i] + ey_field_half_step_accel;
                                vy_local =  c_half_step_forward * vy_minus + s_half_step_forward * my_chunk->vz[i] + ey_field_half_step_accel;
                                vz_local = -s_half_step_forward * vy_minus + c_half_step_forward * my_chunk->vz[i];
                                energy_variables->term_plus_compensation[thread_id][0] = sqr(vx_local / dt_over_dx) + sqr(vy_local / dt_over_dy)
                                                          + sqr(vz_local / dt_over_dy) - energy_variables->compensation[thread_id][0];
                                energy_variables->term_plus_compensation[thread_id][1] = vx_local / dt_over_dx - energy_variables->compensation[thread_id][1];
                                energy_variables->term_plus_compensation[thread_id][2] = vy_local / dt_over_dy - energy_variables->compensation[thread_id][2];
                                energy_variables->term_plus_compensation[thread_id][3] = vz_local / dt_over_dy - energy_variables->compensation[thread_id][3];
                                for (k = 0; k < energy_variables->nb_mpi_diagnostics; k++) {
                                    energy_variables->sum2[thread_id][k] = energy_variables->sum[thread_id][k] + energy_variables->term_plus_compensation[thread_id][k];
                                    energy_variables->compensation[thread_id][k] = (energy_variables->sum2[thread_id][k] - energy_variables->sum[thread_id][k]) - energy_variables->term_plus_compensation[thread_id][k];
                                    energy_variables->sum[thread_id][k] = energy_variables->sum2[thread_id][k];
                                }
                            }
                        }
                    } // End parallel region.
                    energy_variables->local_diagnostics[thread_id][0] = 0.5 * energy_variables->sum[thread_id][0] * energy_variables->weight_xy;
                    for (k = 1; k < energy_variables->nb_mpi_diagnostics; k++)
                        energy_variables->local_diagnostics[thread_id][k] = energy_variables->sum[thread_id][k] * energy_variables->weight_xy;
                }
                for (k = 0; k < energy_variables->nb_mpi_diagnostics; k++) {
                    energy_variables->my_diagnostics[k] = 0.;
                    for (thread_id = 0; thread_id < num_threads; thread_id++)
                        energy_variables->my_diagnostics[k] += energy_variables->local_diagnostics[thread_id][k];
                }
                MPI_Allreduce(&(energy_variables->my_diagnostics[0]), &(energy_variables->diagnostics[0]), energy_variables->nb_mpi_diagnostics, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD);
#endif
                if (energy_variables->file_diag_energy_update) {
                    fprintf(energy_variables->file_diag_energy_update, "%f", diag_energy[i_diag_output][0]);
                    for (j = 1; j < diag_energy_size; j++)
                        fprintf(energy_variables->file_diag_energy_update, " %.*g", DBL_DECIMAL_DIG, diag_energy[i_diag_output][j]);
#ifdef PIC_VERT_KINETIC_ENERGY
                    fprintf(energy_variables->file_diag_energy_update, " %.*g %.*g",
                            DBL_DECIMAL_DIG, energy_variables->diagnostics[0],
                            DBL_DECIMAL_DIG, energy_variables->diagnostics[0] + 0.5 * (diag_energy[i_diag_output][1] + diag_energy[i_diag_output][2]));
                    for (k = 1; k < energy_variables->nb_mpi_diagnostics; k++)
                        fprintf(energy_variables->file_diag_energy_update, " %.*g", DBL_DECIMAL_DIG, energy_variables->diagnostics[k]);
#endif
                    for (i = 0; i < energy_variables->nb_fourier_modes; i++)
                        fprintf(energy_variables->file_diag_energy_update, " %.*g", DBL_DECIMAL_DIG,
                                energy_fourier_mode_2d(mesh, energy_variables->fourier_modes[i].value1, energy_variables->fourier_modes[i].value2, Ex, Ey));
                    fprintf(energy_variables->file_diag_energy_update, "\n");
                }
        }
    }
    
    // Diagnostics hdf5.
    if ((hdf5_variables->hdf5_nb_outputs > 0) &&
            ((i_time % hdf5_variables->hdf5_delta_step_output == 0) || (i_time == sim_variables->num_iteration))) {
        int i_hdf5_output = (i_time == sim_variables->num_iteration)
            ? hdf5_variables->hdf5_nb_outputs + 1
            : i_time / hdf5_variables->hdf5_delta_step_output + 1;
        int hdf5_ncx  = hdf5_variables->mesh_xvx.num_cell_x;
        int hdf5_ncvx = hdf5_variables->mesh_xvx.num_cell_y;
        double f_x_y[ncx+1][ncy+1]; // Array allocated contiguously for hdf5 outputs.
        
        if (mpi_rank == 0) {
            for (i = 0; i <= ncx; i++)
                for (j = 0; j <= ncy; j++)
                    f_x_y[i][j] = q * (rho_2d[i][j] - 1.);
            plot_f_cartesian_mesh_2d(i_hdf5_output, f_x_y[0], mesh, time, "rho");
            for (i = 0; i <= ncx; i++)
                for (j = 0; j <= ncy; j++)
                    f_x_y[i][j] = Ex[i][j];
            plot_f_cartesian_mesh_2d(i_hdf5_output, f_x_y[0], mesh, time, "Ex");
            for (i = 0; i <= ncx; i++)
                for (j = 0; j <= ncy; j++)
                    f_x_y[i][j] = Ey[i][j];
            plot_f_cartesian_mesh_2d(i_hdf5_output, f_x_y[0], mesh, time, "Ey");
        }
#ifdef PIC_VERT_OUTPUT_X_VX
        double hdf5_vx;
        int hdf5_ic_x, hdf5_ic_vx;
        float hdf5_dvx;
        double hdf5_vx_min = hdf5_variables->mesh_xvx.y_min;
        double f_x_vx[hdf5_ncx+1][hdf5_ncvx+1]; // Array allocated contiguously for hdf5 outputs.
        // First, all MPI processes participate in the reduce
        for (i = 0; i <= hdf5_ncx; i++)
            for (j = 0; j <= hdf5_ncvx; j++)
                f_x_vx[i][j] = 0;
        // TODO: Parallelize with OpenMP / Vectorization this deposit (like in the "real" deposit).
        for (j = 0; j < num_cells_2d; j++) {
            chunkbag = &(particles[j]);
            for (my_chunk = chunkbag->front; my_chunk; my_chunk = my_chunk->next) {
                for (i = 0; i < my_chunk->size; i++) {
                    hdf5_vx = (my_chunk->vx[i] / dt_over_dx - hdf5_vx_min) / hdf5_variables->mesh_xvx.delta_y;
                    hdf5_ic_vx = (int)hdf5_vx;
                    hdf5_dvx = (float)(hdf5_vx - hdf5_ic_vx);
                    hdf5_ic_x = (j / ncy) + (hdf5_ncx - ncx) / 2;
                    if ((hdf5_ic_vx >= 0) && (hdf5_ic_vx < hdf5_ncvx) && (hdf5_ic_x >= 0) && (hdf5_ic_x < hdf5_ncx)) {
                        f_x_vx[hdf5_ic_x    ][hdf5_ic_vx    ] += (1. - my_chunk->dx[i]) * (1. - hdf5_dvx);
                        f_x_vx[hdf5_ic_x    ][hdf5_ic_vx + 1] += (1. - my_chunk->dx[i]) * (     hdf5_dvx);
                        f_x_vx[hdf5_ic_x + 1][hdf5_ic_vx    ] += (     my_chunk->dx[i]) * (1. - hdf5_dvx);
                        f_x_vx[hdf5_ic_x + 1][hdf5_ic_vx + 1] += (     my_chunk->dx[i]) * (     hdf5_dvx);
                    }
                }
            }
        }
        // Periodicity
        for (i = 0; i < hdf5_ncx + 1; i++) {
            f_x_vx[i][    0    ] += f_x_vx[i][hdf5_ncvx];
            f_x_vx[i][hdf5_ncvx]  = f_x_vx[i][    0    ];
        }
        for (j = 0; j < hdf5_ncvx + 1; j++) {
            f_x_vx[   0    ][j] += f_x_vx[hdf5_ncx][j];
            f_x_vx[hdf5_ncx][j]  = f_x_vx[   0    ][j];
        }
        // Reduce
        if (mpi_world_size > 1) {
            for (i = 0; i < hdf5_ncx; i++) {
                for (j = 0; j < hdf5_ncvx; j++) {
                    hdf5_variables->send_buf_hdf5[i * hdf5_ncvx + j] = f_x_vx[i][j];
                    hdf5_variables->recv_buf_hdf5[i * hdf5_ncvx + j] = 0.;
                }
            }
            MPI_Allreduce(hdf5_variables->send_buf_hdf5, hdf5_variables->recv_buf_hdf5, hdf5_ncx * hdf5_ncvx, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD);
            for (i = 0; i < hdf5_ncx; i++)
                for (j = 0; j < hdf5_ncvx; j++)
                    f_x_vx[i][j] = hdf5_variables->recv_buf_hdf5[i * hdf5_ncvx + j];
            // Periodicity
            for (i = 0; i < hdf5_ncx + 1; i++)
                f_x_vx[i][hdf5_ncvx] = f_x_vx[i][0];
            for (j = 0; j < hdf5_ncvx + 1; j++)
                f_x_vx[hdf5_ncx][j] = f_x_vx[0][j];
        }
        for (i = 0; i <= hdf5_ncx; i++)
            for (j = 0; j <= hdf5_ncvx; j++)
                f_x_vx[i][j] *= hdf5_variables->weight_x_vx / (hdf5_variables->mesh_xvx.delta_x * hdf5_variables->mesh_xvx.delta_y);
        // Then only one MPI process outputs the reduce
        if (mpi_rank == 0)
            plot_f_cartesian_mesh_2d(i_hdf5_output, f_x_vx[0], hdf5_variables->mesh_xvx, time, "f_x_vx");
#endif
    }
}

/*****************************************************************************
 *                             Simulation 2d3v                               *
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

// Magnetic field.
#ifndef MAGNETIC_FIELD
#   define MAGNETIC_FIELD 2.0
#endif

// Thermal speed.
#ifndef THERMAL_SPEED
#   define THERMAL_SPEED 1.0
#endif

int main(int argc, char** argv) {
    // Timing
    double time_start, time_simu;
    double time_mark1, time_mark2, time_mark3, time_mark4, time_mark5;
    double time_particle_loop, time_append, time_mpi_allreduce, time_poisson;
    
    // Temporary variables.
    double x, y;            // store the new position values
    int ic_x, ic_y, i_cell; // store the new position index values
    size_t i, j, i_time;    // loop indices
    
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
    int nb_fourier_modes = 0;
    couple fourier_modes[PICVERT_MAX_NB_FOURIER_MODES];
    for (i = 0; i < PICVERT_MAX_NB_FOURIER_MODES; i++)
        fourier_modes[i] = (couple) {.value1 = INT_NOT_SET, .value2 = INT_NOT_SET};
    unsigned char sim_distrib = ELECTRON_HOLES_2D3V; // Physical test case (ELECTRON_HOLES_2D3V, BI_MAXWELLIAN_2D3V or LANDAU_1D_PROJ2D3V).
    int ncx               = NCX;                     // Number of grid points, x-axis
    int ncy               = NCY;                     // Number of grid points, y-axis
    long int nb_particles = NB_PARTICLE;             // Number of particles
    int num_iteration     = NB_ITER;                 // Number of time steps
    int diag_nb_outputs   = NB_ITER;                 // Number of diagnostic energy outputs
    int hdf5_nb_outputs   = 1000;                    // Number of hdf5 outputs
    double delta_t        = DELTA_T;                 // Time step
    double thermal_speed  = THERMAL_SPEED;           // Thermal speed
    double B_field        = MAGNETIC_FIELD;          // Constant magnetic field on the x-axis (always reset to 0. for LANDAU_1D_PROJ2D3V)
                                                     // The cyclotron frequency Omega_e has the same adimensionned value as B_field.
    // ELECTRON_HOLES_2D3V only
    double vx_min    = -6.;   // Minimum speed at initialization
    double vx_max    =  6.;   // Maximum speed at initialization
    double ell       = 16.;   // Middle of the physical domain (parallel to B_0 : in x)
    double delta_par =  3.;   // Half-width of the electron hole in x (parallel to B_0 : in x)
    double psi       =  1.;   // Allows to define the bounce frequency omega_b = sqrt(psi / delta_par**2)
    double epsilon   =  0.3;  // Measure of the perturbation
    double ky        =  0.39; // Wave number (transverse to B_0 : in y)
    // BI_MAXWELLIAN_2D3V only
    double vx_drift = 4. * thermal_speed; // Drift of the second electron beam (first one is 0)
    // LANDAU_1D_PROJ2D3V only
    double alpha   = 0.01; // Landau1d perturbation amplitude
    double kmode_x = 0.5;  // Landau1d perturbation mode
    
    // Read parameters from file.
    if (argc >= 2) {
        simulation_parameters parameters = read_parameters_from_file(argv[1], "2D3V");
        if (parameters.nb_fourier_modes != INT_NOT_SET) {
            nb_fourier_modes = parameters.nb_fourier_modes;
            for (i = 0; i < nb_fourier_modes; i++)
                fourier_modes[i] = parameters.fourier_modes[i];
        }
        if (strcmp(parameters.sim_distrib_name, STRING_NOT_SET) == 0)
            sim_distrib = parameters.sim_distrib;
        if (parameters.ncx != INT_NOT_SET)
            ncx             = parameters.ncx;
        if (parameters.ncy != INT_NOT_SET)
            ncy             = parameters.ncy;
        if (parameters.nb_particles != INT_NOT_SET)
            nb_particles    = parameters.nb_particles;
        if (parameters.num_iteration != INT_NOT_SET)
            num_iteration   = parameters.num_iteration;
        if (parameters.diag_nb_outputs != INT_NOT_SET)
            diag_nb_outputs = parameters.diag_nb_outputs;
        if (parameters.hdf5_nb_outputs != INT_NOT_SET)
            hdf5_nb_outputs = parameters.hdf5_nb_outputs;
        if (parameters.delta_t != DOUBLE_NOT_SET)
            delta_t       = parameters.delta_t;
        if (parameters.thermal_speed != DOUBLE_NOT_SET)
            thermal_speed = parameters.thermal_speed;
        if (parameters.B_field != DOUBLE_NOT_SET)
            B_field       = parameters.B_field;
        // ELECTRON_HOLES_2D3V only
        if (parameters.vx_min != DOUBLE_NOT_SET)
            vx_min    = parameters.vx_min;
        if (parameters.vx_max != DOUBLE_NOT_SET)
            vx_max    = parameters.vx_max;
        if (parameters.ell != DOUBLE_NOT_SET)
            ell       = parameters.ell;
        if (parameters.delta_par != DOUBLE_NOT_SET)
            delta_par = parameters.delta_par;
        if (parameters.psi != DOUBLE_NOT_SET)
            psi       = parameters.psi;
        if (parameters.epsilon != DOUBLE_NOT_SET)
            epsilon   = parameters.epsilon;
        if (parameters.ky != DOUBLE_NOT_SET)
            ky        = parameters.ky;
        // BI_MAXWELLIAN_2D3V only
        if (parameters.vx_drift != DOUBLE_NOT_SET)
            vx_drift = parameters.vx_drift;
        // LANDAU_1D_PROJ2D3V only
        if (parameters.alpha != DOUBLE_NOT_SET)
            alpha   = parameters.alpha;
        if (parameters.kmode_x != DOUBLE_NOT_SET)
            kmode_x = parameters.kmode_x;
    } else
        printf("No parameter file was passed through the command line. I will use the default parameters.\n");
    
    // Random initialization or read from file.
    const char sim_initial = INIT_NOFILE;
    
    // Spatial parameters for initial density function.
    double *params;
    if (sim_distrib == ELECTRON_HOLES_2D3V) {
        params = malloc(5 * sizeof(double));
        params[0] = ell;
        params[1] = delta_par;
        params[2] = psi;
        params[3] = epsilon;
        params[4] = ky;
    } else if (sim_distrib == LANDAU_1D_PROJ2D3V) {
        params = malloc(2 * sizeof(double));
        params[0] = alpha;
        params[1] = kmode_x;
    } else {
        params = malloc(0 * sizeof(double));
    }
    
    // Velocity parameters for initial density function.
    double *speed_params;
    if (sim_distrib == ELECTRON_HOLES_2D3V) {
        // In this test case, the thermal speed is in the transverse direction (perpendicular to B_0 : for y and z)
        speed_params = malloc(3 * sizeof(double));
        speed_params[0] = thermal_speed;
        speed_params[1] = vx_min;
        speed_params[2] = vx_max;
    } else if (sim_distrib == BI_MAXWELLIAN_2D3V) {
        // In this test case, the thermal speed is for all the directions
        speed_params = malloc(2 * sizeof(double));
        speed_params[0] = thermal_speed;
        speed_params[1] = vx_drift;
    } else {
        speed_params = malloc(1 * sizeof(double));
        speed_params[0] = thermal_speed;
    }
    
    // Mesh
    double x_min, y_min, x_max, y_max;
    const int num_cells_2d = ncx * ncy;
    if (sim_distrib == ELECTRON_HOLES_2D3V) {
        x_min = 0.;
        y_min = 0.;
        x_max = 2 * ell;
        y_max = 2 * ell;
    } else if (sim_distrib == BI_MAXWELLIAN_2D3V) {
        x_min = 0.;
        y_min = 0.;
        x_max = (double)ncx;
        y_max = (double)ncy;
    } else {
        x_min = 0.;
        y_min = 0.;
        x_max = 2 * PI / kmode_x;
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
    const double q           = -1.; // particle charge
    const double m           =  1.; // particle mass
    const double dt_q_over_m = delta_t * q / m;
    const double dt_over_dx  = delta_t / mesh.delta_x;
    const double dt_over_dy  = delta_t / mesh.delta_y;
    char simulation_name[42]     = "Vlasov-Poisson 2d3v";
    char data_structure_name[99] = "Array of Concurrent Chunkbags of SoA (1 private + 1 shared / cell)";
    char sort_name[42]           = "always sort";
    
   /*
    * Useful variables to update the velocity using the method from
    * Birdsall & Langdon, "Plasma Physics via Computer Simulation"
    * Chap. 4-4 Implementation of the v x B rotation, pp.61-62
    * (in the special case where B_field is parallel to an axis).
    */
    B_field = (sim_distrib == LANDAU_1D_PROJ2D3V) ? 0. : B_field;
    const double t = B_field * 0.5 * dt_q_over_m;
    const double s = 2. * t / (1. + sqr(t));
    const double c = (1. - sqr(t)) / (1. + sqr(t));
    // Half-step backward for initialization (leap-frog scheme, computes (x(n), v(n-1/2))
    const double t_half_step_backward = -t / 2.;
    const double s_half_step_backward = 2. * t_half_step_backward / (1. + sqr(t_half_step_backward));
    const double c_half_step_backward = (1. - sqr(t_half_step_backward)) / (1. + sqr(t_half_step_backward));
    double ey_field_half_step_accel, vy_minus;
    
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
    
    // Hdf5 outputs.
    H5open(); // Initializes the HDF5 library.
    hdf5_nb_outputs = min(1000, hdf5_nb_outputs);                       // With more than 1,000 outputs, VisIt crashes.
    hdf5_nb_outputs = min(num_iteration, hdf5_nb_outputs);              // In case we put more outputs than iterations...
    const int hdf5_delta_step_output = (hdf5_nb_outputs == 0) ? 0 : num_iteration / hdf5_nb_outputs;
    hdf5_nb_outputs = (hdf5_nb_outputs == 0) ? 0 : hdf5_nb_outputs + 1; // For the last diagnostic
    // Hdf5 outputs for x-vx cuts.
    const double hdf5_x_min  = x_min;
    const double hdf5_x_max  = x_max;
    const double hdf5_vx_min = (sim_distrib == ELECTRON_HOLES_2D3V)
        ? vx_min
        : ((sim_distrib == BI_MAXWELLIAN_2D3V)
            ? -vx_drift * 1.5
            : -6.);
    const double hdf5_vx_max = (sim_distrib == ELECTRON_HOLES_2D3V)
        ? vx_max
        : ((sim_distrib == BI_MAXWELLIAN_2D3V)
            ? vx_drift * 2.5
            : 6.);
    const int hdf5_ncx = min(256, lowest_even_number_greater_or_equal_than((int)((hdf5_x_max - hdf5_x_min) / (x_max - x_min) * ncx)));
    const int hdf5_ncvx = 128;
    diag_hdf5_variables hdf5_variables;
    hdf5_variables.hdf5_nb_outputs = hdf5_nb_outputs;
    hdf5_variables.hdf5_delta_step_output = hdf5_delta_step_output;
    hdf5_variables.mesh_xvx = create_mesh_2d(hdf5_ncx, hdf5_ncvx, hdf5_x_min, hdf5_x_max, hdf5_vx_min, hdf5_vx_max);
    hdf5_variables.send_buf_hdf5 = allocateMatrix(hdf5_ncx, hdf5_ncvx);
    hdf5_variables.recv_buf_hdf5 = allocateMatrix(hdf5_ncx, hdf5_ncvx);
    hdf5_variables.weight_x_vx = (float)(x_max - x_min) / ((float)mpi_world_size * (float)nb_particles); // Weight for deposit on the x-vx plane.
    
    // The following matrices are (ncx+1) * (ncy+1) arrays, with the periodicity :
    //     M[ncx][ . ] = M[0][.]
    //     M[ . ][ncy] = M[.][0]
    // rho the charge density
    // Ex the electric field on the x-axis
    // Ey the electric field on the y-axis
    double** rho_2d = allocate_matrix(ncx+1, ncy+1);
    double** Ex = allocate_matrix(ncx+1, ncy+1);
    double** Ey = allocate_matrix(ncx+1, ncy+1);
    double** q_times_rho = allocate_matrix(ncx+1, ncy+1); // to store q * rho = -rho
    
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
        case LANDAU_1D_PROJ2D3V:
            kmode = kmode_x;
            break;
    }
    diag_nb_outputs = min(num_iteration, diag_nb_outputs);    // In case we put more outputs than iterations...
    const int diag_delta_step_output = num_iteration / diag_nb_outputs;
    diag_nb_outputs = num_iteration / diag_delta_step_output; // In case the above division does not give integer value
    diag_nb_outputs = diag_nb_outputs + 1;                    // For the last diagnostic
    const int diag_energy_size = (sim_distrib == LANDAU_1D_PROJ2D3V) ? 5 : 3;
    const int diag_speed_size  = 5;
    double** diag_energy = allocate_matrix(diag_nb_outputs, diag_energy_size);
    double** diag_speed  = allocate_matrix(num_iteration,   diag_speed_size);
    FILE* file_diag_energy_update = (void*)0; // Updated all simulation long, contrary to the previous ones which are only created at the end
    if (mpi_rank == 0) {
        file_diag_energy_update = fopen("diag_energy.txt", "w");
        fprintf(file_diag_energy_update, "Time | Int(Ex^2) | Int(Ey^2)");
#ifdef PIC_VERT_KINETIC_ENERGY
        fprintf(file_diag_energy_update, " | Kinetic Energy | Total Energy (= Kinetic Energy + 0.5 * (Int(Ex^2) + Int(Ey^2)))");
        fprintf(file_diag_energy_update, " | Momentum (x) | Momentum (y) | Momentum (z)");
#endif
        for (i = 0; i < nb_fourier_modes; i++)
            fprintf(file_diag_energy_update, " | Energy Fourier mode(%d, %d)", fourier_modes[i].value1, fourier_modes[i].value2);
        fprintf(file_diag_energy_update, "\n");
    }
    
  if (mpi_rank == 0) {
    printf("#VEC_ALIGN = %d\n", VEC_ALIGN);
    printf("#OMP_TILE_SIZE = %d\n", OMP_TILE_SIZE);
    printf("#OMP_TILE_BORDERS = %d\n", OMP_TILE_BORDERS);
    printf("#mpi_world_size = %d\n", mpi_world_size);
    printf("#num_threads = %d\n", num_threads);
    printf("#x_min = %.*g\n", DBL_DECIMAL_DIG, x_min);
    printf("#x_max = %.*g\n", DBL_DECIMAL_DIG, x_max);
    printf("#y_min = %.*g\n", DBL_DECIMAL_DIG, y_min);
    printf("#y_max = %.*g\n", DBL_DECIMAL_DIG, y_max);
    printf("#ncx = %d\n", ncx);
    printf("#ncy = %d\n", ncy);
    printf("#nb_particles = %ld\n", nb_particles);
    printf("#num_iteration = %d\n", num_iteration);
    printf("#diag_nb_outputs = %d\n", diag_nb_outputs);
    printf("#hdf5_nb_outputs = %d\n", hdf5_nb_outputs);
    printf("#q = %.*g\n", DBL_DECIMAL_DIG, q);
    printf("#m = %.*g\n", DBL_DECIMAL_DIG, m);
    printf("#delta_t = %.*g\n", DBL_DECIMAL_DIG, delta_t);
    printf("#thermal_speed = %.*g\n", DBL_DECIMAL_DIG, thermal_speed);
    printf("#B_field = %.*g\n", DBL_DECIMAL_DIG, B_field);
    printf("#fourier_modes = ");
    for (i = 0; i < nb_fourier_modes; i++)
        printf("(%d, %d) ", fourier_modes[i].value1, fourier_modes[i].value2);
    printf("\n");
    printf("#initial_function_case = %s\n", distribution_names_2d3v[sim_distrib]);
    if (sim_distrib == ELECTRON_HOLES_2D3V) {
        printf("#vx_min = %.*g\n", DBL_DECIMAL_DIG, vx_min);
        printf("#vx_max = %.*g\n", DBL_DECIMAL_DIG, vx_max);
        printf("#ell = %.*g\n", DBL_DECIMAL_DIG, ell);
        printf("#delta_par = %.*g\n", DBL_DECIMAL_DIG, delta_par);
        printf("#psi = %.*g\n", DBL_DECIMAL_DIG, psi);
        printf("#epsilon = %.*g\n", DBL_DECIMAL_DIG, epsilon);
        printf("#ky = %.*g\n", DBL_DECIMAL_DIG, ky);
    } else if (sim_distrib == BI_MAXWELLIAN_2D3V) {
        printf("#vx_drift = %.*g\n", DBL_DECIMAL_DIG, vx_drift);
    } else if (sim_distrib == LANDAU_1D_PROJ2D3V) {
        printf("#alpha = %.*g\n", DBL_DECIMAL_DIG, alpha);
        printf("#kmode_x = %.*g\n", DBL_DECIMAL_DIG, kmode_x);
    }
  }
    
    // Poisson solver.
    poisson_2d_solver solver = new_poisson_2d_fft_solver(mesh);

    // Coloring
    int i_color;
    const int nb_color_2d = 4;
    
    // Particle data structure.
    bag* chunkbag;
    chunk* next_chunk;
    chunk* my_chunk;
    int ix_min, ix_max, iy_min, iy_max, ix, iy;
    bag* particles = malloc(num_cells_2d * sizeof(bag));
    bag** particlesNext = malloc(NB_BAGS_PER_CELL * sizeof(bag*));
    for (j = 0; j < NB_BAGS_PER_CELL; j++)
        particlesNext[j] = malloc(num_cells_2d * sizeof(bag));
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
        read_particle_array_2d3v(mpi_rank, mpi_world_size, nb_particles, mesh, &weight, &particles);
        if (mpi_rank == 0)
            printf("Read time (%ld particles) : %g sec\n", nb_particles, (double) (omp_get_wtime() - time_start));
    } else {
        pic_vert_seed_double_RNG(mpi_rank);
//         Different random numbers at each run.
//         pic_vert_seed_double_RNG(seed_64bits(mpi_rank));
        // Creation of random particles and sorting.
        time_start = omp_get_wtime();
        create_particle_array_2d3v(mpi_world_size, nb_particles, mesh, sim_distrib,
            params, speed_params, &weight, &particles);
        if (mpi_rank == 0)
            printf("Creation time (%ld particles) : %g sec\n", nb_particles, (double) (omp_get_wtime() - time_start));
        if (sim_initial == INIT_WRITE) {
            write_particle_array_2d3v(mpi_rank, nb_particles, mesh, particles);
            MPI_Finalize();
            return 0;
        }
    }
    
    // Because the weight is constant, the whole array can be multiplied by weight just once.
    // Because charge is the charge MASS and not the charge DENSITY, we have to divide.
    const double charge_factor = weight / (mesh.delta_x * mesh.delta_y);
    // We just use the electric fields to update the speed, with always the same multiply.
    // WARNING : there is a dt_over_dx factor because speeds are represented that way.
    const double x_field_factor = dt_q_over_m * dt_over_dx;
    const double y_field_factor = dt_q_over_m * dt_over_dy;
    
    simulation_variables sim_variables;
    sim_variables.num_iteration = num_iteration;
    sim_variables.delta_t = delta_t;
    sim_variables.q = q;
    sim_variables.dt_over_dx = dt_over_dx;
    sim_variables.dt_over_dy = dt_over_dy;
    sim_variables.mesh = mesh;
    
    diag_energy_variables energy_variables;
    energy_variables.sim_distrib = sim_distrib;
    energy_variables.diag_nb_outputs = diag_nb_outputs;
    energy_variables.diag_delta_step_output = diag_delta_step_output;
    energy_variables.landau_values = get_damping_values(kmode);
    energy_variables.landau_mult_cstt = sqr(4. * alpha * energy_variables.landau_values->er) * PI / kmode; // Landau
    energy_variables.nb_mpi_diagnostics = 4; // Kinetic energy, momentum in each of 3 directions
    energy_variables.my_diagnostics = malloc(energy_variables.nb_mpi_diagnostics * sizeof(double)); // Values of diagnostics local to each MPI process
    energy_variables.diagnostics    = malloc(energy_variables.nb_mpi_diagnostics * sizeof(double)); // MPI reduction of all diagnostics
    energy_variables.local_diagnostics      = allocate_aligned_double_matrix(num_threads, energy_variables.nb_mpi_diagnostics);
    energy_variables.sum                    = allocate_aligned_double_matrix(num_threads, energy_variables.nb_mpi_diagnostics);
    energy_variables.compensation           = allocate_aligned_double_matrix(num_threads, energy_variables.nb_mpi_diagnostics);
    energy_variables.sum2                   = allocate_aligned_double_matrix(num_threads, energy_variables.nb_mpi_diagnostics);
    energy_variables.term_plus_compensation = allocate_aligned_double_matrix(num_threads, energy_variables.nb_mpi_diagnostics);
    energy_variables.file_diag_energy_update = file_diag_energy_update;
    energy_variables.weight_xy = weight;
    energy_variables.nb_fourier_modes = nb_fourier_modes;
    energy_variables.fourier_modes = malloc(nb_fourier_modes * sizeof(couple));
    for (i = 0; i < nb_fourier_modes; i++) {
        energy_variables.fourier_modes[i].value1 = fourier_modes[i].value1;
        energy_variables.fourier_modes[i].value2 = fourier_modes[i].value2;
    }
    // Half-step forward for diagnostics (you have to recompute v(n) from v(n-1/2))
    const double t_half_step_forward = t / 2.;
    energy_variables.s_half_step_forward = 2. * t_half_step_forward / (1. + sqr(t_half_step_forward));
    energy_variables.c_half_step_forward = (1. - sqr(t_half_step_forward)) / (1. + sqr(t_half_step_forward));
    
#ifdef PIC_VERT_TEST_INITIAL
    diagnostics(0, mpi_rank, mpi_world_size, Ex, Ey, E_field, rho_2d, particles,
        &energy_variables, &hdf5_variables, &sim_variables, diag_energy, diag_energy_size);
    MPI_Finalize();
    return 0;
#endif
    
    reset_charge_2d_accumulator(ncx, ncy, num_threads, charge_accu);
    // Computes rho at initial time.
    #pragma omp parallel private(thread_id, offset)
    {
        thread_id = omp_get_thread_num();
        offset = thread_id * NB_CORNERS_2D * num_cells_2d;
        #pragma omp for private(i, j, chunkbag, my_chunk, corner)
        for (j = 0; j < num_cells_2d; j++) {
            chunkbag = &(particles[j]);
            for (my_chunk = chunkbag->front; my_chunk; my_chunk = my_chunk->next) {
                for (i = 0; i < my_chunk->size; i++) {
#ifdef PIC_VERT_OPENMP_4_0
                    #pragma omp simd aligned(coeffs_x, coeffs_y, signs_x, signs_y:VEC_ALIGN)
#endif
                    for (corner = 0; corner < NB_CORNERS_2D; corner++) {
                        charge_accu[offset + NB_CORNERS_2D * j + corner] +=
                            (coeffs_x[corner] + signs_x[corner] * my_chunk->dx[i]) *
                            (coeffs_y[corner] + signs_y[corner] * my_chunk->dy[i]);
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
    // WARNING : starting from here, v doesn't represent (vx), but (vx * dt / dx).
    //                                                   (vy)      (vy * dt / dy)
    //                                                   (vz)      (vz * dt / dy) [yes, dy]
    #pragma omp parallel for private(i, j, chunkbag, my_chunk, ey_field_half_step_accel, vy_minus) firstprivate(dt_over_dx, dt_over_dy, c_half_step_backward, s_half_step_backward)
    for (j = 0; j < num_cells_2d; j++) {
        chunkbag = &(particles[j]);
        for (my_chunk = chunkbag->front; my_chunk; my_chunk = my_chunk->next) {
#ifdef PIC_VERT_OPENMP_4_0
            #pragma omp simd
#endif
            for (i = 0; i < my_chunk->size; i++) {
                my_chunk->vx[i] *= dt_over_dx;
                my_chunk->vy[i] *= dt_over_dy;
                my_chunk->vz[i] *= dt_over_dy; // yes, dy
                my_chunk->vx[i] -= 0.5 * (
                         (     my_chunk->dx[i]) * (     my_chunk->dy[i]) * E_field[j].field_x.north_east
                       + (1. - my_chunk->dx[i]) * (     my_chunk->dy[i]) * E_field[j].field_x.north_west
                       + (     my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * E_field[j].field_x.south_east
                       + (1. - my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * E_field[j].field_x.south_west);
                ey_field_half_step_accel = -0.25 * (
                         (     my_chunk->dx[i]) * (     my_chunk->dy[i]) * E_field[j].field_y.north_east
                       + (1. - my_chunk->dx[i]) * (     my_chunk->dy[i]) * E_field[j].field_y.north_west
                       + (     my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * E_field[j].field_y.south_east
                       + (1. - my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * E_field[j].field_y.south_west);
                vy_minus = my_chunk->vy[i] + ey_field_half_step_accel;
                my_chunk->vy[i] =  c_half_step_backward * vy_minus + s_half_step_backward * my_chunk->vz[i] + ey_field_half_step_accel;
                my_chunk->vz[i] = -s_half_step_backward * vy_minus + c_half_step_backward * my_chunk->vz[i];
            }
        }
    }
    
    /********************************************************************************************
     *                               Beginning of main time loop                                *
     ********************************************************************************************/
    int nb_stars = 0;
#ifdef PAPI_LIB_INSTALLED
    start_diag_papi(&file_diag_papi, "diag_papi_4corners-opt.txt", papi_num_events, Events);
#endif
    
    time_start = omp_get_wtime();
    for (i_time = 0; i_time < num_iteration; i_time++) {
        // Diagnostics energy and hdf5
        diagnostics(i_time, mpi_rank, mpi_world_size, Ex, Ey, E_field, rho_2d, particles,
            &energy_variables, &hdf5_variables, &sim_variables, diag_energy, diag_energy_size);
        
        time_mark1 = omp_get_wtime();
        
#ifdef PAPI_LIB_INSTALLED
        /* Read the counters */
        if (PAPI_read_counters(values, papi_num_events) != PAPI_OK)
            handle_error(1);
#endif
        
        reset_charge_2d_accumulator(ncx, ncy, num_threads, charge_accu);
        #pragma omp parallel private(thread_id, offset, my_chunk, chunkbag, next_chunk, i_color) firstprivate(nb_color_2d)
        {
            thread_id = omp_get_thread_num();
            offset = thread_id * NB_CORNERS_2D * num_cells_2d;
            // Loop on the 4 colors (in 2d), with synchronisation each time.
            for (i_color = 0; i_color < nb_color_2d; i_color++) {
                // Loop on the tiles of the grid, for the chosen color.
                #pragma omp for private(ix_min, ix_max, iy_min, iy_max, ix, iy, i, j, corner, x, y, ic_x, ic_y, i_cell, ey_field_half_step_accel, vy_minus) firstprivate(ncxminusone, ncyminusone, icell_param, c, s) collapse(2)
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
#ifdef PIC_VERT_OPENMP_4_0
                            #pragma omp simd
#endif
                            for (i = 0; i < my_chunk->size; i++) {
                                my_chunk->vx[i] +=
                                         (     my_chunk->dx[i]) * (     my_chunk->dy[i]) * E_field[j].field_x.north_east
                                       + (1. - my_chunk->dx[i]) * (     my_chunk->dy[i]) * E_field[j].field_x.north_west
                                       + (     my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * E_field[j].field_x.south_east
                                       + (1. - my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * E_field[j].field_x.south_west;
                                ey_field_half_step_accel = 0.5 * (
                                         (     my_chunk->dx[i]) * (     my_chunk->dy[i]) * E_field[j].field_y.north_east
                                       + (1. - my_chunk->dx[i]) * (     my_chunk->dy[i]) * E_field[j].field_y.north_west
                                       + (     my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * E_field[j].field_y.south_east
                                       + (1. - my_chunk->dx[i]) * (1. - my_chunk->dy[i]) * E_field[j].field_y.south_west);
                                vy_minus = my_chunk->vy[i] + ey_field_half_step_accel;
                                my_chunk->vy[i] =  c * vy_minus + s * my_chunk->vz[i] + ey_field_half_step_accel;
                                my_chunk->vz[i] = -s * vy_minus + c * my_chunk->vz[i];
                            }
                            for (i = 0; i < my_chunk->size; i++) {
                                x = (j / ncy        ) + my_chunk->dx[i] + my_chunk->vx[i];
                                y = (j & ncyminusone) + my_chunk->dy[i] + my_chunk->vy[i];
                                ic_x = (int)x - (x < 0.);
                                ic_y = (int)y - (y < 0.);
                                i_cell = COMPUTE_I_CELL_2D(icell_param, ic_x & ncxminusone, ic_y & ncyminusone);
                                if (ic_x >= ix_min - OMP_TILE_BORDERS && ic_x <= ix_max + OMP_TILE_BORDERS && ic_y >= iy_min - OMP_TILE_BORDERS && ic_y <= iy_max + OMP_TILE_BORDERS)
                                    bag_push_serial(&(particlesNext[ID_PRIVATE_BAG][i_cell]), (float)(x - ic_x), (float)(y - ic_y), my_chunk->vx[i], my_chunk->vy[i], my_chunk->vz[i], thread_id);
                                else
                                    bag_push_concurrent(&(particlesNext[ID_SHARED_BAG][i_cell]), (float)(x - ic_x), (float)(y - ic_y), my_chunk->vx[i], my_chunk->vy[i], my_chunk->vz[i], thread_id);
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
            #pragma omp single
            {
                time_mark2 = omp_get_wtime();
                compute_cumulative_free_list_sizes();
            }
            #pragma omp for private(i, j)
            for (j = 0; j < num_cells_2d; j++) {
                particles[j] = particlesNext[ID_SHARED_BAG][j];
                bag_init(&(particlesNext[ID_SHARED_BAG][j]), ID_SHARED_BAG, j, thread_id);
                bag_append(&(particles[j]), &(particlesNext[ID_PRIVATE_BAG][j]), ID_PRIVATE_BAG, j, thread_id);
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
        if (mpi_rank == 0)
            print_time_left(diag_speed[0][0], i_time + 1, num_iteration, &nb_stars);
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
    // Last diagnostic.
    diagnostics(num_iteration, mpi_rank, mpi_world_size, Ex, Ey, E_field, rho_2d, particles,
        &energy_variables, &hdf5_variables, &sim_variables, diag_energy, diag_energy_size);
    if (file_diag_energy_update)
        fclose(file_diag_energy_update);
    diag_energy_and_speed_chunkbags(mpi_rank,
        "diag_lee_4corners.txt",   diag_nb_outputs, diag_energy_size, diag_energy,
        "diag_speed_4corners.txt", num_iteration,   diag_speed_size,  diag_speed);
    print_time_chunkbags(mpi_rank, mpi_world_size, nb_particles, num_iteration, time_simu, simulation_name, data_structure_name, sort_name,
        time_particle_loop, time_append, time_mpi_allreduce, time_poisson);
    
    // Export particles for eventual restart.
//    write_particle_array_2d3v(mpi_rank, nb_particles, mesh, particles);
    
    free(params);
    free(speed_params);
    deallocate_matrix(q_times_rho, ncx+1, ncy+1);
    deallocate_matrix(rho_2d, ncx+1, ncy+1);
    deallocate_matrix(Ex, ncx+1, ncy+1);
    deallocate_matrix(Ey, ncx+1, ncy+1);
    deallocate_matrix(diag_energy, diag_nb_outputs, diag_energy_size);
    deallocate_matrix(diag_speed,  num_iteration,   diag_speed_size);
    deallocate_matrix(energy_variables.local_diagnostics,      num_threads, energy_variables.nb_mpi_diagnostics);
    deallocate_matrix(energy_variables.sum,                    num_threads, energy_variables.nb_mpi_diagnostics);
    deallocate_matrix(energy_variables.compensation,           num_threads, energy_variables.nb_mpi_diagnostics);
    deallocate_matrix(energy_variables.sum2,                   num_threads, energy_variables.nb_mpi_diagnostics);
    deallocate_matrix(energy_variables.term_plus_compensation, num_threads, energy_variables.nb_mpi_diagnostics);
    free(charge_accu);
    
    free(send_buf);
    free(recv_buf);
    free(hdf5_variables.send_buf_hdf5);
    free(hdf5_variables.recv_buf_hdf5);
    free_poisson_2d(&solver);
    free_field_2d(E_field);
    pic_vert_free_RNG();
    MPI_Finalize();
    
    return 0;
}

