#ifndef PIC_VERT_PARAMETER_READER
#define PIC_VERT_PARAMETER_READER

// Helpers to read parameters from file
#ifndef PICVERT_MAX_NB_FOURIER_MODES
#    define PICVERT_MAX_NB_FOURIER_MODES 256
#endif
#ifndef PICVERT_MAX_LINE_LENGTH
#    define PICVERT_MAX_LINE_LENGTH 2048
#endif
#define DELIM "="
const char* STRING_NOT_SET = "NOT_SET";
const int INT_NOT_SET = -42;
const double DOUBLE_NOT_SET = -42.;

typedef struct couple {
    int value1, value2;
} couple;

typedef struct phase_space_position {
    double x, y, z, vx, vy, vz;
} phase_space_position;

typedef struct simulation_parameters {
    int nb_fourier_modes;
    couple fourier_modes[PICVERT_MAX_NB_FOURIER_MODES];
    char sim_distrib_name[PICVERT_MAX_LINE_LENGTH];
    unsigned char sim_distrib; // Physical test case.
    int ncx;                   // Number of grid points, x-axis
    int ncy;                   // Number of grid points, y-axis
    int ncz;                   // Number of grid points, z-axis
    long int nb_particles;     // Number of particles
    int num_iteration;         // Number of time steps
    int diag_nb_outputs;       // Number of diagnostics (energy, speed) outputs
    int hdf5_nb_outputs;       // Number of hdf5 outputs
    double delta_t;            // Time step
    double thermal_speed;      // Thermal speed
    double B_field;            // Constant magnetic field.
                               // The cyclotron frequency Omega_e has the same adimensionned value as B_field.
    phase_space_position initial_tracked; // Wanted initial position for the tracked particle.
    // ELECTRON_HOLES_2D3V only
    double vx_min;    // Minimum speed at initialization
    double vx_max;    // Maximum speed at initialization
    double ell;       // Middle of the physical domain (parallel to B_0 : in x)
    double delta_par; // Half-width of the electron hole in x (parallel to B_0 : in x)
    double psi;       // Allows to define the bounce frequency omega_b = sqrt(psi / delta_par**2)
    double epsilon;   // Measure of the perturbation
    double ky;        // Wave number (transverse to B_0 : in y)
    // BI_MAXWELLIAN_2D3V only
    double vx_drift;  // Drift of the second electron beam (first one is 0)
    // DRIFT_VELOCITIES_3D only
    double drift_velocity;            // Center of the second maxwellian
    double proportion_fast_particles; // Proportions of the particles in the second maxwellian
    // LANDAU_3D_PROD_OF_ONE_PLUS_COS only
    double L;         // Length of the physical space
    // LANDAU_XXX only
    double alpha;     // Landau perturbation amplitude
    double kmode_x;   // Landau perturbation mode, x-axis
    double kmode_y;   // Landau perturbation mode, y-axis
    double kmode_z;   // Landau perturbation mode, z-axis
} simulation_parameters;

simulation_parameters read_parameters_from_file(const char* filename, const char* sim_dimensions);

#endif // ifndef PIC_VERT_PARAMETER_READER
