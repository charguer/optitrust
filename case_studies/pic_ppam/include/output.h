#ifndef PIC_VERT_OUTPUT
#define PIC_VERT_OUTPUT

#include "variadic.h" // macros VARIADIC, NUMARG9, NUMARG10

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
void print_time_left(double start_time, int id_step, int nb_steps, int* nb_stars);



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
void print_time_chunkbags(int mpi_rank, int mpi_world_size, unsigned int num_particle, unsigned int num_iteration,
        double time_simulation, char* simulation_name, char* data_structure_name, char* sort_name,
        double time_particle_loop, double time_append, double time_mpi_reduce, double time_poisson);


// When calling with 8 arguments, the macro will set diag_energy_size to 5.
#define diag_energy_and_speed_chunkbags_8(a, b, c, d, e,    g, h, i) a, b, c, d, e, 5, g, h, i
#define diag_energy_and_speed_chunkbags_9(a, b, c, d, e, f, g, h, i) a, b, c, d, e, f, g, h, i
#define diag_energy_and_speed_chunkbags(...) VARIADIC(diag_energy_and_speed_chunkbags, NUMARG9(__VA_ARGS__), __VA_ARGS__)

/*
 * At the end of a simulation, outputs the diagnostics into files.
 * Note: also calls the VARIADIC macro, but you wouldn't notice unless this portion of code doesn't compile :)
 *
 * @param[in] mpi_rank the rank of the MPI process.
 * @param[in]      num_iteration number of time iterations in the simulation.
 * @param[in, out] file_energy the file where to output the energy diagnostics.
 * @param[in]      filename_energy the name of the file where to output the energy diagnostics.
 * @param[in]      diag_energy[num_iteration][diag_energy_size] the energy values to output the energy diagnostics.
 * @param[in]      diag_energy_size size of the diag_energy array. Optional, defaults to 5.
 * @param[in, out] file_speed the file where to output the speed diagnostics.
 * @param[in]      filename_speed the name of the file where to output the speed diagnostics.
 * @param[in]      diag_speed[num_iteration][5] the speed values to output the speed diagnostics.
 */
void diag_energy_and_speed_chunkbags(int mpi_rank, unsigned int num_iteration,
        FILE** file_energy, char* filename_energy, double** diag_energy, int diag_energy_size,
        FILE** file_speed,  char* filename_speed,  double** diag_speed);



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

// When calling with 8 arguments, the macro will set diag_energy_size to 5 and diag_speed_size to 7.
// There is a ambiguity with 9 arguments. Either diag_energy_size or diag_speed_size is missing. This
// is checked by the type of the 6th argument (that should be either int either FILE**).
#define diag_energy_and_speed_sorting_8( a, b, c, d, e,    g, h, i   ) a, b, c, d, e, 5, g, h, i, 7
#define diag_energy_and_speed_sorting_9( a, b, c, d, e, f, g, h, i   ) a, b, c, d, e, \
  _Generic((f), FILE**: 5, default: f), \
  _Generic((f), FILE**: f, default: g), \
  _Generic((f), FILE**: g, default: h), \
  _Generic((f), FILE**: h, default: i), \
  _Generic((f), FILE**: i, default: 7)
#define diag_energy_and_speed_sorting_10(a, b, c, d, e, f, g, h, i, j) a, b, c, d, e, f, g, h, i, j
#define diag_energy_and_speed_sorting(...) VARIADIC(diag_energy_and_speed_sorting, NUMARG10(__VA_ARGS__), __VA_ARGS__)

/*
 * At the end of a simulation, outputs the diagnostics into files.
 * Note: also calls the VARIADIC macro, but you wouldn't notice unless this portion of code doesn't compile :)
 *
 * @param[in] mpi_rank the rank of the MPI process.
 * @param[in]      num_iteration number of time iterations in the simulation.
 * @param[in, out] file_energy the file where to output the energy diagnostics.
 * @param[in]      filename_energy the name of the file where to output the energy diagnostics.
 * @param[in]      diag_energy[num_iteration][diag_energy_size] the energy values to output the energy diagnostics.
 * @param[in]      diag_energy_size size of the diag_energy array. Optional, defaults to 5.
 * @param[in, out] file_speed the file where to output the speed diagnostics.
 * @param[in]      filename_speed the name of the file where to output the speed diagnostics.
 * @param[in]      diag_speed[num_iteration][diag_speed_size] the speed values to output the speed diagnostics.
 * @param[in]      diag_speed_size size of the diag_speed array. Optional, defaults to 7.
 */
void diag_energy_and_speed_sorting(int mpi_rank, unsigned int num_iteration,
        FILE** file_energy, char* filename_energy, double** diag_energy, int diag_energy_size,
        FILE** file_speed,  char* filename_speed,  double** diag_speed,  int diag_speed_size);

#endif // ifndef PIC_VERT_OUTPUT

