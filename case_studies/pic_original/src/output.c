#include <stdio.h>      // function printf (output strings on a stream)
#include <stdlib.h>     // type     size_t
#include <omp.h>        // function omp_get_wtime
#include "parameters.h" // constant DBL_DECIMAL_DIG

/* Prints an approximation of the time left in the simulation.
 * Also prints stars to show the approximate percentage already done (50 stars
 * in total, so each star accounts for 2%).
 *
 * @param[in]      start_time the time when the simulation started.
 * @param[in]      id_step    the current step count, in [1 ; nb_steps].
 * @param[in]      nb_steps   the total number of steps in the simulation.
 * @param[in, out] nb_stars   the number of stars to print.
 *                              in : the last number of stars printed
 *                              out : the new number of stars printed, if it changed
 */
void print_time_left(double start_time, int id_step, int nb_steps, int nb_stars[static 1]) {
    int nb_sec_left, i;
    double nb_sec_up_to_now;
    
    if (50 * id_step / nb_steps > *nb_stars) {
        *nb_stars = 50 * id_step / nb_steps;
        nb_sec_up_to_now = omp_get_wtime() - start_time;
        nb_sec_left = (int)((double)(nb_steps - id_step) * nb_sec_up_to_now / ((double)id_step));
        if (nb_sec_left / 3600 > 0) {
            printf("[");
            for (i = 0; i < *nb_stars; i++)
                printf("*");
            for (i = 0; i < 50 - *nb_stars; i++)
                printf(" ");
            printf("] %dh%02d left\n", nb_sec_left / 3600, (nb_sec_left % 3600) / 60);
        } else if (nb_sec_left / 60 > 0) {
            printf("[");
            for (i = 0; i < *nb_stars; i++)
                printf("*");
            for (i = 0; i < 50 - *nb_stars; i++)
                printf(" ");
            printf("] %02d'%02d'' left\n", nb_sec_left / 60, nb_sec_left % 60);
        } else {
            printf("[");
            for (i = 0; i < *nb_stars; i++)
                printf("*");
            for (i = 0; i < 50 - *nb_stars; i++)
                printf(" ");
            printf("]    %02d'' left\n", nb_sec_left);
        }
    }
}



/*****************************************************************************
 *                  Outputs of the simulation, chunkbags                     *
 *****************************************************************************/

/*
 * At the end of a simulation, prints the timings.
 *
 * @param[in] mpi_rank the rank of the MPI process.
 * @param[in] mpi_world_size the number of MPI processes.
 * @param[in] num_particle number of particles in the simulation.
 * @param[in] num_iteration number of time iterations in the simulation.
 * @param[in] time_simulation total time of the simulation.
 * @param[in] *_name specific names.
 * @param[in] time_* specific timing values.
 */
void print_time_chunkbags(int mpi_rank, int mpi_world_size, long int num_particle, unsigned int num_iteration,
        double time_simulation, char* simulation_name, char* data_structure_name, char* sort_name,
        double time_particle_loop, double time_append, double time_mpi_reduce, double time_poisson) {
    double nb_particles_processed = (double)mpi_world_size * (double)num_particle * (double)num_iteration;
    if (mpi_rank == 0) {
        printf("---------- %s - %s ----------\n", simulation_name, data_structure_name);
        printf("---------- %s ----------\n", sort_name);
        printf("Execution time (total)    : %g s\n", time_simulation);
        printf("Array of particles update : %g s (%g%%)\n", (time_particle_loop + time_append), 100. * (time_particle_loop + time_append) / time_simulation);
        printf("- Including particle loop : %g s (%g%%)\n", time_particle_loop, 100. * time_particle_loop / time_simulation);
        printf("- Including append        : %g s (%g%%)\n", time_append,        100. * time_append        / time_simulation);
        printf("Reduction of rho          : %g s (%g%%)\n", time_mpi_reduce,    100. * time_mpi_reduce    / time_simulation);
        printf("Poisson solver            : %g s (%g%%)\n", time_poisson,       100. * time_poisson       / time_simulation);
        printf("Nb. particles / s : %g\n", nb_particles_processed / time_simulation);
        printf("Time / particle / iteration : %g ns\n", (time_particle_loop + time_append) / nb_particles_processed * 1000000000.);
        printf("\n");
    }
    char filename[30];
    sprintf(filename, "print_time_sorting_rank%d.dat", mpi_rank);
    FILE* file_timing = fopen(filename, "w");
    fprintf(file_timing, "---------- %s - %s ----------\n", simulation_name, data_structure_name);
    fprintf(file_timing, "---------- %s ----------\n", sort_name);
    fprintf(file_timing, "Execution time (total)    : %g s\n", time_simulation);
    fprintf(file_timing, "Array of particles update : %g s (%g%%)\n", (time_particle_loop + time_append), 100. * (time_particle_loop + time_append) / time_simulation);
    fprintf(file_timing, "- Including particle loop : %g s (%g%%)\n", time_particle_loop, 100. * time_particle_loop / time_simulation);
    fprintf(file_timing, "- Including append        : %g s (%g%%)\n", time_append,        100. * time_append        / time_simulation);
    fprintf(file_timing, "Reduction of rho          : %g s (%g%%)\n", time_mpi_reduce,    100. * time_mpi_reduce    / time_simulation);
    fprintf(file_timing, "Poisson solver            : %g s (%g%%)\n", time_poisson,       100. * time_poisson       / time_simulation);
    fprintf(file_timing, "Nb. particles / s : %g\n", nb_particles_processed / time_simulation);
    fprintf(file_timing, "Time / particle / iteration : %g ns\n", (time_particle_loop + time_append) / nb_particles_processed * 1000000000.);
    fprintf(file_timing, "\n");
    fclose(file_timing);
}

/*
 * At the end of a simulation, outputs the diagnostics into files.
 *
 * @param[in] mpi_rank the rank of the MPI process.
 * @param[in] filename_energy the name of the file where to output the energy diagnostics.
 * @param[in] nb_diag_energy number of energy diagnostics in the simulation.
 * @param[in] diag_energy_size size of the diag_energy array.
 * @param[in] diag_energy[nb_diag_energy][diag_energy_size] the energy values to output the energy diagnostics.
 * @param[in] filename_speed the name of the file where to output the speed diagnostics.
 * @param[in] nb_diag_speed number of energy diagnostics in the simulation.
 * @param[in] diag_speed_size size of the diag_speed array.
 * @param[in] diag_speed[nb_diag_speed][diag_speed_size] the speed values to output the speed diagnostics.
 */
void diag_energy_and_speed_chunkbags(int mpi_rank,
        char* filename_energy, unsigned int nb_diag_energy, int diag_energy_size, double** diag_energy,
        char* filename_speed,  unsigned int nb_diag_speed,  int diag_speed_size,  double** diag_speed) {
  if (mpi_rank == 0) {
    size_t i, j;
    
    FILE* file_diag_energy = fopen(filename_energy, "w");
    if (diag_energy_size == 3)
        fprintf(file_diag_energy, "Time | Int(Ex^2) | Int(Ey^2)\n");
    else if (diag_energy_size == 5)
        fprintf(file_diag_energy, "Time | log(Int(E^2))-simu | log(Int(E^2))-theo | Int(E^2)-simu | Int(E^2)-theo\n");
    else if (diag_energy_size == 7)
        fprintf(file_diag_energy, "Time | log(Int(Ex^2)) | Int(Ex^2) | log(Int(Ey^2)) | Int(Ey^2) | log(Int(Ez^2)) | Int(Ez^2)\n");
    else
        fprintf(file_diag_energy, "Time | No info on the diagnostics ; edit diag_energy_and_speed_chunkbags in %s for this testcase.\n", __FILE__);
    for (i = 0; i < nb_diag_energy; i++) {
        fprintf(file_diag_energy, "%f", diag_energy[i][0]);
        for (j = 1; j < diag_energy_size; j++)
            fprintf(file_diag_energy, " %.*g", DBL_DECIMAL_DIG, diag_energy[i][j]);
        fprintf(file_diag_energy, "\n");
    }
    fclose(file_diag_energy);
    
    FILE* file_diag_speed = fopen(filename_speed, "w");
    if (diag_speed_size == 5)
        fprintf(file_diag_speed, "Iteration | Time for particle loop (s) | Time for append (s) | Time for rho reduction + allreduce (s) | Time for Poisson solver (s) | Total time (s)\n");
    else if (diag_speed_size == 9)
        fprintf(file_diag_speed, "Iteration | Time for particle loop from overlapped region (s) | Time for launching the send/recv of particles (s) | Time for particle loop from interior region (s) | Time for append (s) | Time waiting for the send/recv of particles (s) | Time for depositing the received particles (s) | Time for rho reduction + allgather (s) | Time for Poisson solve (s) | Total time (s)\n");
    else
        fprintf(file_diag_speed, "Iteration | No info on the diagnostics ; edit diag_energy_and_speed_chunkbags in %s for this testcase.\n", __FILE__);
    for (i = 0; i < nb_diag_speed; i++) {
        fprintf(file_diag_speed, "%ld ", i + 1);
        for (j = 0; j < 4; j++)
            fprintf(file_diag_speed, "%.*g ", DBL_DECIMAL_DIG, diag_speed[i][j + 1] - diag_speed[i][j]);
        fprintf(file_diag_speed, "%.*g\n", DBL_DECIMAL_DIG, diag_speed[i][4] - diag_speed[i][0]);
    }
    fclose(file_diag_speed);
  }
}



/*****************************************************************************
 *                Outputs of the simulation, 3 loops + sort                  *
 *****************************************************************************/

/*
 * At the end of a simulation, prints the timings.
 *
 * @param[in] mpi_rank the rank of the MPI process.
 * @param[in] mpi_world_size the number of MPI processes.
 * @param[in] num_particle number of particles in the simulation.
 * @param[in] num_iteration number of time iterations in the simulation.
 * @param[in] time_simulation total time of the simulation.
 * @param[in] *_name specific names.
 * @param[in] time_* specific timing values.
 */
void print_time_sorting(int mpi_rank, int mpi_world_size, unsigned int num_particle, unsigned int num_iteration,
        double time_simulation, char* simulation_name, char* data_structure_name, char* sort_name,
        double time_update_v, double time_update_x, double time_deposit, double time_sort,
        double time_mpi_reduce, double time_poisson) {
    double nb_particles_processed = (double)mpi_world_size * (double)num_particle * (double)num_iteration;
    if (mpi_rank == 0) {
        printf("---------- %s - %s ----------\n", simulation_name, data_structure_name);
#ifdef PIC_VERT_SFC_WITH_ADDITIONAL_ARRAYS
        printf("---------- With additional arrays for space-filling curves ----------\n");
#endif
        printf("---------- %s ----------\n", sort_name);
        printf("Execution time (total)    : %g s\n", time_simulation);
        printf("Array of particles sort   : %g s (%g%%)\n", time_sort,       100. * time_sort       / time_simulation);
        printf("Array of particles update : %g s (%g%%)\n", (time_update_v + time_update_x + time_deposit), 100. * (time_update_v + time_update_x + time_deposit) / time_simulation);
        printf("- Including update v      : %g s (%g%%)\n", time_update_v,   100. * time_update_v   / time_simulation);
        printf("- Including update x      : %g s (%g%%)\n", time_update_x,   100. * time_update_x   / time_simulation);
        printf("- Including deposit       : %g s (%g%%)\n", time_deposit,    100. * time_deposit    / time_simulation);
        printf("Reduction of rho          : %g s (%g%%)\n", time_mpi_reduce, 100. * time_mpi_reduce / time_simulation);
        printf("Poisson solver            : %g s (%g%%)\n", time_poisson,    100. * time_poisson    / time_simulation);
        printf("Nb. particles / s : %g\n", nb_particles_processed / time_simulation);
        printf("Time / particle / iteration (push)    : %g ns\n", (time_update_v + time_update_x) / nb_particles_processed * 1000000000.);
        printf("Time / particle / iteration (deposit) : %g ns\n", time_deposit / nb_particles_processed * 1000000000.);
        printf("\n");
    }
    char filename[30];
    sprintf(filename, "print_time_sorting_rank%d.dat", mpi_rank);
    FILE* file_timing = fopen(filename, "w");
    fprintf(file_timing, "---------- %s - %s ----------\n", simulation_name, data_structure_name);
#ifdef PIC_VERT_SFC_WITH_ADDITIONAL_ARRAYS
    fprintf(file_timing, "---------- With additional arrays for space-filling curves ----------\n");
#endif
    fprintf(file_timing, "---------- %s ----------\n", sort_name);
    fprintf(file_timing, "Execution time (total)    : %g s\n", time_simulation);
    fprintf(file_timing, "Array of particles sort   : %g s (%g%%)\n", time_sort,       100. * time_sort       / time_simulation);
    fprintf(file_timing, "Array of particles update : %g s (%g%%)\n", (time_update_v + time_update_x + time_deposit), 100. * (time_update_v + time_update_x + time_deposit) / time_simulation);
    fprintf(file_timing, "- Including update v      : %g s (%g%%)\n", time_update_v,   100. * time_update_v   / time_simulation);
    fprintf(file_timing, "- Including update x      : %g s (%g%%)\n", time_update_x,   100. * time_update_x   / time_simulation);
    fprintf(file_timing, "- Including deposit       : %g s (%g%%)\n", time_deposit,    100. * time_deposit    / time_simulation);
    fprintf(file_timing, "Reduction of rho          : %g s (%g%%)\n", time_mpi_reduce, 100. * time_mpi_reduce / time_simulation);
    fprintf(file_timing, "Poisson solver            : %g s (%g%%)\n", time_poisson,    100. * time_poisson    / time_simulation);
    fprintf(file_timing, "Nb. particles / s : %g\n", nb_particles_processed / time_simulation);
    fprintf(file_timing, "Time / particle / iteration (push)    : %g ns\n", (time_update_v + time_update_x) / nb_particles_processed * 1000000000.);
    fprintf(file_timing, "Time / particle / iteration (deposit) : %g ns\n", time_deposit / nb_particles_processed * 1000000000.);
    fprintf(file_timing, "\n");
    fclose(file_timing);
}

/*
 * At the end of a simulation, outputs the diagnostics into files.
 *
 * @param[in] mpi_rank the rank of the MPI process.
 * @param[in] filename_energy the name of the file where to output the energy diagnostics.
 * @param[in] nb_diag_energy number of energy diagnostics in the simulation.
 * @param[in] diag_energy_size size of the diag_energy array.
 * @param[in] diag_energy[nb_diag_energy][diag_energy_size] the energy values to output the energy diagnostics.
 * @param[in] filename_speed the name of the file where to output the speed diagnostics.
 * @param[in] nb_diag_speed number of energy diagnostics in the simulation.
 * @param[in] diag_speed_size size of the diag_speed array.
 * @param[in] diag_speed[nb_diag_speed][diag_speed_size] the speed values to output the speed diagnostics.
 */
void diag_energy_and_speed_sorting(int mpi_rank,
        char* filename_energy, unsigned int nb_diag_energy, int diag_energy_size, double** diag_energy,
        char* filename_speed,  unsigned int nb_diag_speed,  int diag_speed_size,  double** diag_speed) {
  if (mpi_rank == 0) {
    size_t i, j;
    
    FILE* file_diag_energy = fopen(filename_energy, "w");
    if (diag_energy_size == 3)
        fprintf(file_diag_energy, "Time | Int(Ex^2) | Int(Ey^2)\n");
    else if (diag_energy_size == 5)
        fprintf(file_diag_energy, "Time | log(Int(E^2))-simu | log(Int(E^2))-theo | Int(E^2)-simu | Int(E^2)-theo\n");
    else if (diag_energy_size == 7)
        fprintf(file_diag_energy, "Time | log(Int(Ex^2)) | Int(Ex^2) | log(Int(Ey^2)) | Int(Ey^2) | log(Int(Ez^2)) | Int(Ez^2)\n");
    else
        fprintf(file_diag_energy, "Time | No info on the diagnostics ; edit diag_energy_and_speed_sorting in %s for this testcase.\n", __FILE__);
    for (i = 0; i < nb_diag_energy; i++) {
        fprintf(file_diag_energy, "%f", diag_energy[i][0]);
        for (j = 1; j < diag_energy_size; j++)
            fprintf(file_diag_energy, " %.*g", DBL_DECIMAL_DIG, diag_energy[i][j]);
        fprintf(file_diag_energy, "\n");
    }
    fclose(file_diag_energy);
    
    FILE* file_diag_speed = fopen(filename_speed, "w");
    if (diag_speed_size == 5)
        fprintf(file_diag_speed, "Iteration | Time for sort (s) | Time for particle loop (s) | Time for rho reduction (s) | Time for Poisson solver (s) | Total time (s)\n");
    else if (diag_speed_size == 6)
        fprintf(file_diag_speed, "Iteration | Time for sort (s) | Time for update v (s) | Time for update x + deposit (s) | Time for rho reduction (s) | Time for Poisson solver (s) | Total time (s)\n");
    else if (diag_speed_size == 7)
        fprintf(file_diag_speed, "Iteration | Time for sort (s) | Time for update v (s) | Time for update x (s) | Time for deposit (s) | Time for rho reduction (s) | Time for Poisson solver (s) | Total time (s)\n");
    else if (diag_speed_size == 9)
        fprintf(file_diag_speed, "Iteration | Time for sort (s) | Time for computation of x_n_star / rho_n_star (s) | Time waiting for MPI_Iallreduce of rho_n (s) | Time for Poisson solver E_n (s) | Time for computation of x_n+1 / rho_n+1 (s) | Time waiting for MPI_Iallreduce of rho_n_star (s) | Time for Poisson solver E_n_star (s) | Time for computation of v_n+1 (s) | Total time (s)\n");
    else if (diag_speed_size == 10)
        fprintf(file_diag_speed, "Iteration | Time for sort (s) | Time for computation of x_n_star / rho_n_star (s) | Time waiting for MPI_Iallreduce of rho_n (s) | Time for Poisson solver E_n (s) | Time for computation of v_n_star (s) | Time for computation of x_n+1 / rho_n+1 (s) | Time waiting for MPI_Iallreduce of rho_n_star (s) | Time for Poisson solver E_n_star (s) | Time for computation of v_n+1 (s) | Total time (s)\n");
    else
        fprintf(file_diag_speed, "Iteration | No info on the diagnostics ; edit diag_energy_and_speed_sorting in %s for this testcase.\n", __FILE__);
    for (i = 0; i < nb_diag_speed; i++) {
        fprintf(file_diag_speed, "%ld ", i + 1);
        for (j = 0; j < diag_speed_size - 1; j++)
            fprintf(file_diag_speed, "%.*g ", DBL_DECIMAL_DIG, diag_speed[i][j + 1] - diag_speed[i][j]);
        fprintf(file_diag_speed, "%.*g\n", DBL_DECIMAL_DIG, diag_speed[i][diag_speed_size - 1] - diag_speed[i][0]);
    }
    fclose(file_diag_speed);
  }
}

