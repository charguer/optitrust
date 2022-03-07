#include <stdio.h>                 // functions printf, fgets, fopen
#include <stdlib.h>                // functions exit (error handling), strtol (converts string to long)
                                   // constant  EXIT_FAILURE (error handling)
#include <ctype.h>                 // function  isspace
#include <errno.h>                 // variable  errno (e.g. set by fgets)
                                   // constant  ERANGE
#include <string.h>                // functions memcpy, strcmp, strlen, strstr
#include <stdbool.h>               // type      bool
#include "initial_distributions.h" // constants distribution_names_1d, distribution_names_2d,
                                   //           distribution_names_2d3v, distribution_names_3d
#include "math_functions.h"        // function  is_power_of_two
#include "parameter_reader.h"      // types     couple, phase_space_position, simulation_parameters
                                   // constants PICVERT_MAX_NB_FOURIER_MODES, PICVERT_MAX_LINE_LENGTH,
                                   //           DELIM, STRING_NOT_SET, INT_NOT_SET, DOUBLE_NOT_SET

#define CONTINUE_MESSAGE(...) \
    do {                      \
        printf(__VA_ARGS__);  \
        continue;             \
    } while(0)

void remove_spaces(char* restrict str_trimmed, const char* restrict str_untrimmed) {
    while (*str_untrimmed != '\0') {
        if (!isspace(*str_untrimmed) && (*str_untrimmed != ';')) {
            *str_trimmed = *str_untrimmed;
            str_trimmed++;
        }
        str_untrimmed++;
    }
    *str_trimmed = '\0';
}

void remove_comments(char* str) {
    while (*str != '\0') {
        if (*str == '#' ||                        // Script or Python-like comment
                *str == '!' ||                    // Fortran-like comment
                *str == '%' ||                    // Latex-like comment
                (*str == '/' && *(str+1) == '/')) // C-like comment
            break;
        str++;
    }
    *str = '\0';
}

/*
 * Read a long from a string.
 *
 * @param[in, out] string the string that should contain an int to read. Outputs the first character after the int read.
 * @param[out]     long_value the value of the long read (0 if nothing could be read).
 * @param[in]      count the line number where this string was
 * @param[in]      line the line where this string was
 */
bool long_from_string(char** string, long int* long_value, int count, const char* line) {
    errno = 0;
    char* strtol_after_read;
    *long_value = strtol(*string, &strtol_after_read, 10);
    if (errno == ERANGE || *string == strtol_after_read) {
        printf("I could not read the value of the parameter on line %d (%s). I use default value instead.\n", count, line);
        return false;
    } else if (*long_value < 0) {
        printf("The value of the parameter should be >= 0 on line %d (%s). I use default value instead.\n", count, line);
        return false;
    }
    *string = strtol_after_read;
    return true;
}

/*
 * Read an int from a string.
 *
 * @param[in, out] string the string that should contain an int to read. Outputs the first character after the int read.
 * @param[out]     int_value the value of the int read (0 if nothing could be read).
 * @param[in]      count the line number where this string was
 * @param[in]      line the line where this string was
 */
bool int_from_string(char** string, int* int_value, int count, const char* line) {
    long int long_value;
    if (long_from_string(string, &long_value, count, line)) {
        *int_value = (int)long_value;
        if (*int_value < 0) {
            printf("The value of the parameter should be >= 0 on line %d (%s). I use default value instead.\n", count, line);
            return false;
        }
        return true;
    }
    return false;
}

/*
 * Read a double from a string.
 *
 * @param[in, out] string the string that should contain a double to read. Outputs the first character after the double read.
 * @param[out]     double_value the value of the double read (0. if nothing could be read).
 * @param[in]      count the line number where this string was
 * @param[in]      line the line where this string was
 */
bool double_from_string(char** string, double* double_value, int count, const char* line) {
    errno = 0;
    char* strtod_after_read;
    *double_value = strtod(*string, &strtod_after_read);
    if (errno == ERANGE || *string == strtod_after_read) {
        printf("I could not read the value of the parameter on line %d (%s). I use default value instead.\n", count, line);
        return false;
    }
    *string = strtod_after_read;
    return true;
}

simulation_parameters read_parameters_from_file(const char* filename, const char* sim_dimensions) {
    char line[PICVERT_MAX_LINE_LENGTH];         // Each line read
    char trimmed_line[PICVERT_MAX_LINE_LENGTH]; // Each line without spaces
    int count = 0;                              // Number of the last line read
    char* parameter_name;
    char* parameter_value;
    
    // Number of dimensions of the phase-space.
    int nb_phase_space_dims;
    if (strcmp(sim_dimensions, "1D") == 0) {
        nb_phase_space_dims = 2;
    } else if (strcmp(sim_dimensions, "2D") == 0) {
        nb_phase_space_dims = 4;
    } else if (strcmp(sim_dimensions, "2D3V") == 0) {
        nb_phase_space_dims = 5;
    } else if (strcmp(sim_dimensions, "3D") == 0) {
        nb_phase_space_dims = 6;
    } else {
        printf("Wrong value for sim_dimensions (%s). Possible values are 1D, 2D, 2D3V or 3D.\n", sim_dimensions);
        exit(EXIT_FAILURE);
    }
    double initial_tracked_array[nb_phase_space_dims];
    for (int i = 0; i < nb_phase_space_dims; i++)
        initial_tracked_array[i] = DOUBLE_NOT_SET;
    
    // Unset parameter values.
    int nb_fourier_modes = INT_NOT_SET;
    couple fourier_modes[PICVERT_MAX_NB_FOURIER_MODES];
    for (int i = 0; i < PICVERT_MAX_NB_FOURIER_MODES; i++)
        fourier_modes[i] = (couple) {.value1 = INT_NOT_SET, .value2 = INT_NOT_SET};
    char sim_distrib_name[PICVERT_MAX_LINE_LENGTH] = STRING_NOT_SET;
    unsigned char sim_distrib = 0;         // Physical test case.
    int ncx               = INT_NOT_SET;   // Number of grid points, x-axis
    int ncy               = INT_NOT_SET;   // Number of grid points, y-axis
    int ncz               = INT_NOT_SET;   // Number of grid points, z-axis
    long int nb_particles = INT_NOT_SET;   // Number of particles
    int num_iteration     = INT_NOT_SET;   // Number of time steps
    int diag_nb_outputs   = INT_NOT_SET;   // Number of diagnostics (energy, speed) outputs
    int hdf5_nb_outputs   = INT_NOT_SET;   // Number of hdf5 outputs
    double delta_t       = DOUBLE_NOT_SET; // Time step
    double thermal_speed = DOUBLE_NOT_SET; // Thermal speed
    double B_field       = DOUBLE_NOT_SET; // Constant magnetic field.
                                           // The cyclotron frequency Omega_e has the same adimensionned value as B_field.
    phase_space_position initial_tracked = (phase_space_position) {
        .x  = DOUBLE_NOT_SET, .y  = DOUBLE_NOT_SET, .z  = DOUBLE_NOT_SET,
        .vx = DOUBLE_NOT_SET, .vy = DOUBLE_NOT_SET, .vz = DOUBLE_NOT_SET }; // Wanted initial position for the tracked particle.
    // ELECTRON_HOLES_2D3V only
    double vx_min    = DOUBLE_NOT_SET; // Minimum speed at initialization
    double vx_max    = DOUBLE_NOT_SET; // Maximum speed at initialization
    double ell       = DOUBLE_NOT_SET; // Middle of the physical domain (parallel to B_0 : in x)
    double delta_par = DOUBLE_NOT_SET; // Half-width of the electron hole in x (parallel to B_0 : in x)
    double psi       = DOUBLE_NOT_SET; // Allows to define the bounce frequency omega_b = sqrt(psi / delta_par**2)
    double epsilon   = DOUBLE_NOT_SET; // Measure of the perturbation
    double ky        = DOUBLE_NOT_SET; // Wave number (transverse to B_0 : in y)
    // BI_MAXWELLIAN_2D3V only
    double vx_drift = DOUBLE_NOT_SET;  // Drift of the second electron beam (first one is 0)
    // DRIFT_VELOCITIES_3D only
    double drift_velocity            = DOUBLE_NOT_SET; // Center of the second maxwellian
    double proportion_fast_particles = DOUBLE_NOT_SET; // Proportions of the particles in the second maxwellian
    // LANDAU_3D_PROD_OF_ONE_PLUS_COS only
    double L = DOUBLE_NOT_SET;       // Length of the physical space
    // LANDAU_XXX only
    double alpha   = DOUBLE_NOT_SET; // Landau perturbation amplitude
    double kmode_x = DOUBLE_NOT_SET; // Landau perturbation mode, x-axis
    double kmode_y = DOUBLE_NOT_SET; // Landau perturbation mode, y-axis
    double kmode_z = DOUBLE_NOT_SET; // Landau perturbation mode, z-axis
    
    // Read the parameter file.
    FILE* fp = fopen(filename, "r");
    if (!fp) { // Error in file opening
        printf("I could not open %s.\n", filename);
        exit(EXIT_FAILURE);
    }
    while (fgets(line, PICVERT_MAX_LINE_LENGTH, fp)) { // Successful line reading
        count++;
        line[strlen(line) - 1] = '\0'; // Useful when we need to print the line
        remove_spaces(trimmed_line, line); // Allows the  user to put any spaces in the definition of parameters
        remove_comments(trimmed_line); // Allows the  user to put comments in the parameter file
        if (trimmed_line[0] == '\0') // Empty line
            continue;
        parameter_value = strstr(trimmed_line, DELIM);
        if (!parameter_value) // DELIM was not found in the line
            CONTINUE_MESSAGE("Missing %s on line %d (%s). I drop this line.\n", DELIM, count, line);
        *parameter_value = '\0';                           // Ending the string here for parameter_name
        parameter_value = parameter_value + strlen(DELIM); // The value is just after DELIM...
        parameter_name = trimmed_line;                     // ...and the name is just before.
        if (strcmp(parameter_name, "sim_distrib") == 0) {
            // Read the value as a string.
            if (strcmp(sim_dimensions, "1D") == 0) {
                for (int i = 0; i < NB_TEST_CASE_1D; i++)
                    if (strcmp(parameter_value, distribution_names_1d[i]) == 0) {
                        sim_distrib = i;
                        memcpy(sim_distrib_name, parameter_value, strlen(parameter_value) + 1);
                    }
            } else if (strcmp(sim_dimensions, "2D") == 0) {
                for (int i = 0; i < NB_TEST_CASE_2D; i++)
                    if (strcmp(parameter_value, distribution_names_2d[i]) == 0) {
                        sim_distrib = i;
                        memcpy(sim_distrib_name, parameter_value, strlen(parameter_value) + 1);
                    }
            } else if (strcmp(sim_dimensions, "2D3V") == 0) {
                for (int i = 0; i < NB_TEST_CASE_2D3V; i++)
                    if (strcmp(parameter_value, distribution_names_2d3v[i]) == 0) {
                        sim_distrib = i;
                        memcpy(sim_distrib_name, parameter_value, strlen(parameter_value) + 1);
                    }
            } else if (strcmp(sim_dimensions, "3D") == 0) {
                for (int i = 0; i < NB_TEST_CASE_3D; i++)
                    if (strcmp(parameter_value, distribution_names_3d[i]) == 0) {
                        sim_distrib = i;
                        memcpy(sim_distrib_name, parameter_value, strlen(parameter_value) + 1);
                    }
            }
            if (strcmp(sim_distrib_name, STRING_NOT_SET) == 0)
                CONTINUE_MESSAGE("Unkwnown value for sim_distrib on line %d (%s). I use default value instead.\n", count, line);
        } else if (strcmp(parameter_name, "fourier_modes") == 0) {
            // Read the values as couples of integers
            nb_fourier_modes = 0;
            while (*parameter_value == '(') {
                if (nb_fourier_modes + 1 > PICVERT_MAX_NB_FOURIER_MODES)
                    CONTINUE_MESSAGE("You cannot ask for more than %d Fourier modes. I drop the last ones on line %d (%s).\n", PICVERT_MAX_NB_FOURIER_MODES, count, line);
                parameter_value++;
                if (!int_from_string(&parameter_value, &(fourier_modes[nb_fourier_modes].value1), count, line))
                    CONTINUE_MESSAGE("I did not understand the %d-th Fourier mode value on line %d (%s). I drop the rest of the line.\n", nb_fourier_modes + 1, count, line);
                if (*parameter_value != ',')
                    CONTINUE_MESSAGE("Missing ',' in the %d-th Fourier mode value on line %d (%s). I drop the rest of the line.\n", nb_fourier_modes + 1, count, line);
                parameter_value++;
                if (!int_from_string(&parameter_value, &(fourier_modes[nb_fourier_modes].value2), count, line))
                    CONTINUE_MESSAGE("I did not understand the %d-th Fourier mode value on line %d (%s). I drop the rest of the line.\n", nb_fourier_modes + 1, count, line);
                if (*parameter_value != ')')
                    CONTINUE_MESSAGE("Missing ')' in the %d-th Fourier mode value on line %d (%s). I drop the rest of the line.\n", nb_fourier_modes + 1, count, line);
                parameter_value++;
                nb_fourier_modes++;
            }
        } else if (strcmp(parameter_name, "nb_cells_x") == 0) {
            // Read the value as an integer
            if (int_from_string(&parameter_value, &ncx, count, line)) {
                if (!is_power_of_two(ncx))
                    printf("Warning: nb_cells_x is not a power of two on line %d (%s). Be sure to handle this case in the simulation.\n", count, line);
            }
        } else if (strcmp(parameter_name, "nb_cells_y") == 0) {
            // Read the value as an integer
            if (int_from_string(&parameter_value, &ncy, count, line)) {
                if (!is_power_of_two(ncy))
                    printf("Warning: nb_cells_y is not a power of two on line %d (%s). Be sure to handle this case in the simulation.\n", count, line);
            }
        } else if (strcmp(parameter_name, "nb_cells_z") == 0) {
            // Read the value as an integer
            if (int_from_string(&parameter_value, &ncz, count, line)) {
                if (!is_power_of_two(ncz))
                    printf("Warning: nb_cells_z is not a power of two on line %d (%s). Be sure to handle this case in the simulation.\n", count, line);
            }
        } else if (strcmp(parameter_name, "nb_iterations") == 0) {
            // Read the value as an integer
            int_from_string(&parameter_value, &num_iteration, count, line);
        } else if (strcmp(parameter_name, "nb_particles") == 0) {
            // Read the value as an long integer
            long_from_string(&parameter_value, &nb_particles, count, line);
        } else if (strcmp(parameter_name, "diag_nb_outputs") == 0) {
            // Read the value as an integer
            int_from_string(&parameter_value, &diag_nb_outputs, count, line);
        } else if (strcmp(parameter_name, "hdf5_nb_outputs") == 0) {
            // Read the value as an integer
            int_from_string(&parameter_value, &hdf5_nb_outputs, count, line);
        } else if (strcmp(parameter_name, "delta_t") == 0) {
            // Read the value as a double
            double_from_string(&parameter_value, &delta_t, count, line);
        } else if (strcmp(parameter_name, "thermal_speed") == 0) {
            // Read the value as a double
            double_from_string(&parameter_value, &thermal_speed, count, line);
        } else if (strcmp(parameter_name, "B_field") == 0) {
            // Read the value as a double
            double_from_string(&parameter_value, &B_field, count, line);
        } else if (strcmp(parameter_name, "track_particle_around") == 0) {
            // Read the value as n-uple of doubles
            if (*parameter_value != '(')
                CONTINUE_MESSAGE("Missing '(' on line %d (%s). I use default value instead.\n", count, line);
            parameter_value++;
            if (!double_from_string(&parameter_value, &initial_tracked_array[0], count, line))
                CONTINUE_MESSAGE("I did not understand the first position value on line %d (%s). I use default value instead.\n", count, line);
            for (int i = 1; i < nb_phase_space_dims; i++) {
                if (*parameter_value != ',')
                    CONTINUE_MESSAGE("Missing ',' for the %d-th position value on line %d (%s). I use default value instead.\n", i + 1, count, line);
                parameter_value++;
                if (!double_from_string(&parameter_value, &initial_tracked_array[i], count, line))
                    CONTINUE_MESSAGE("I did not understand the %d-th position value on line %d (%s). I use default value instead.\n", i + 1, count, line);
                if (*parameter_value != ')')
                    CONTINUE_MESSAGE("Missing ')' on line %d (%s). I nevertheless use the values read.\n", count, line);
            }
        } else if (strcmp(parameter_name, "vx_min") == 0) {
            // Read the value as a double
            double_from_string(&parameter_value, &vx_min, count, line);
        } else if (strcmp(parameter_name, "vx_max") == 0) {
            // Read the value as a double
            double_from_string(&parameter_value, &vx_max, count, line);
        } else if (strcmp(parameter_name, "ell") == 0) {
            // Read the value as a double
            double_from_string(&parameter_value, &ell, count, line);
        } else if (strcmp(parameter_name, "delta_par") == 0) {
            // Read the value as a double
            double_from_string(&parameter_value, &delta_par, count, line);
        } else if (strcmp(parameter_name, "psi") == 0) {
            // Read the value as a double
            double_from_string(&parameter_value, &psi, count, line);
        } else if (strcmp(parameter_name, "epsilon") == 0) {
            // Read the value as a double
            double_from_string(&parameter_value, &epsilon, count, line);
        } else if (strcmp(parameter_name, "ky") == 0) {
            // Read the value as a double
            double_from_string(&parameter_value, &ky, count, line);
        } else if (strcmp(parameter_name, "vx_drift") == 0) {
            // Read the value as a double
            double_from_string(&parameter_value, &vx_drift, count, line);
        } else if (strcmp(parameter_name, "drift_velocity") == 0) {
            // Read the value as a double
            double_from_string(&parameter_value, &drift_velocity, count, line);
        } else if (strcmp(parameter_name, "proportion_fast_particles") == 0) {
            // Read the value as a double
            double_from_string(&parameter_value, &proportion_fast_particles, count, line);
        } else if (strcmp(parameter_name, "L") == 0) {
            // Read the value as a double
            double_from_string(&parameter_value, &L, count, line);
        } else if (strcmp(parameter_name, "alpha") == 0) {
            // Read the value as a double
            double_from_string(&parameter_value, &alpha, count, line);
        } else if (strcmp(parameter_name, "kmode_x") == 0) {
            // Read the value as a double
            double_from_string(&parameter_value, &kmode_x, count, line);
        } else if (strcmp(parameter_name, "kmode_y") == 0) {
            // Read the value as a double
            double_from_string(&parameter_value, &kmode_y, count, line);
        } else if (strcmp(parameter_name, "kmode_z") == 0) {
            // Read the value as a double
            double_from_string(&parameter_value, &kmode_z, count, line);
        } else { // The name of the parameter was wrong
            CONTINUE_MESSAGE("I did not understand the parameter name on line %d (%s). I drop this line.\n", count, line);
        }
    }
    if (!feof(fp)) { // Error different from EndOfFile
        printf("Error while reading %s.\n", filename);
        exit(EXIT_FAILURE);
    }
    
    if (strcmp(sim_dimensions, "1D") == 0) {
        initial_tracked.x  = initial_tracked_array[0];
        initial_tracked.vx = initial_tracked_array[1];
    } else if (strcmp(sim_dimensions, "2D") == 0) {
        initial_tracked.x  = initial_tracked_array[0];
        initial_tracked.y  = initial_tracked_array[1];
        initial_tracked.vx = initial_tracked_array[2];
        initial_tracked.vy = initial_tracked_array[3];
    } else if (strcmp(sim_dimensions, "2D3V") == 0) {
        initial_tracked.x  = initial_tracked_array[0];
        initial_tracked.y  = initial_tracked_array[1];
        initial_tracked.vx = initial_tracked_array[2];
        initial_tracked.vy = initial_tracked_array[3];
        initial_tracked.vz = initial_tracked_array[4];
    } else if (strcmp(sim_dimensions, "3D") == 0) {
        initial_tracked.x  = initial_tracked_array[0];
        initial_tracked.y  = initial_tracked_array[1];
        initial_tracked.z  = initial_tracked_array[2];
        initial_tracked.vx = initial_tracked_array[3];
        initial_tracked.vy = initial_tracked_array[4];
        initial_tracked.vz = initial_tracked_array[5];
    }
    simulation_parameters parameters = (simulation_parameters) {
        .nb_fourier_modes = nb_fourier_modes,
        .sim_distrib      = sim_distrib,
        .ncx             = ncx,
        .ncy             = ncy,
        .ncz             = ncz,
        .nb_particles    = nb_particles,
        .num_iteration   = num_iteration,
        .diag_nb_outputs = diag_nb_outputs,
        .hdf5_nb_outputs = hdf5_nb_outputs,
        .delta_t       = delta_t,
        .thermal_speed = thermal_speed,
        .B_field       = B_field,
        .initial_tracked = initial_tracked,
        // ELECTRON_HOLES_2D3V only
        .vx_min    = vx_min,
        .vx_max    = vx_max,
        .ell       = ell,
        .delta_par = delta_par,
        .psi       = psi,
        .epsilon   = epsilon,
        .ky        = ky,
        // BI_MAXWELLIAN_2D3V only
        .vx_drift = vx_drift,
        // DRIFT_VELOCITIES_3D only
        .drift_velocity            = drift_velocity,
        .proportion_fast_particles = proportion_fast_particles,
        // LANDAU_3D_PROD_OF_ONE_PLUS_COS only
        .L = L,
        // LANDAU_XXX only
        .alpha   = alpha,
        .kmode_x = kmode_x,
        .kmode_y = kmode_y,
        .kmode_z = kmode_z,
    };
    memcpy(parameters.sim_distrib_name, sim_distrib_name, strlen(sim_distrib_name) + 1);
    if (nb_fourier_modes != INT_NOT_SET)
        for (int i = 0; i < nb_fourier_modes; i++)
            parameters.fourier_modes[i] = fourier_modes[i];
    return parameters;
}

