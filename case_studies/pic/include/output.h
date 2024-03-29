#ifndef PIC_VERT_OUTPUT
#define PIC_VERT_OUTPUT

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
void print_time_left(double start_time, int id_step, int nb_steps, int nb_stars[static 1]);



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
        double time_particle_loop, double time_append, double time_mpi_reduce, double time_poisson);

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
        char* filename_speed,  unsigned int nb_diag_speed,  int diag_speed_size,  double** diag_speed);



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
        double time_mpi_reduce, double time_poisson);

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
        char* filename_speed,  unsigned int nb_diag_speed,  int diag_speed_size,  double** diag_speed);

#endif // ifndef PIC_VERT_OUTPUT

