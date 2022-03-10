#include "pic_demo.h"
// #include "pic_demo_aux.h"

#include <string.h>
#include "initial_distributions.h" // types speeds_generator_3d, distribution_function_3d,
  // max_distribution_function, variables speed_generators_3d, distribution_funs_3d,
  // distribution_maxs_3d
#include "parameters.h"  // constants PI, EPSILON, DBL_DECIMAL_DIG, FLT_DECIMAL_DIG, NB_PARTICLE
#include "poisson_solvers.h"
#include "random.h" // macros pic_vert_seed_double_RNG, pic_vert_free_RNG
#include "matrix_functions.h" // functions allocate_3d_array, deallocate_3d_array
#include "parameter_reader.h" // type simulation_parameters
                              // constants STRING_NOT_SET, INT_NOT_SET, DOUBLE_NOT_SET
                              // function  read_parameters_from_file

// --------- Local objects

// Object describing the grid (used by particle initialization and poisson solver)
cartesian_mesh_3d mesh;

// Object describing the configuration of the Poisson solver
poisson_3d_solver poisson;


// --------- Load parameters

void loadParameters(int argc, char** argv) {
  TRACE("Initialization starts, using chunk size %d\n", CHUNK_SIZE);

  // START OF "DO NOT CHANGE, THIS IS COPY PASTED FROM PIC-VERT"
  // Automatic values for the parameters.
  sim_distrib = INITIAL_DISTRIBUTION; // Physical test case (LANDAU_1D_PROJ3D, LANDAU_2D_PROJ3D, LANDAU_3D_SUM_OF_COS,
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
      if (parameters.seed != INT_NOT_SET)
          seed = parameters.seed;
      if (seed < 0) // if seed is not specified
        seed = seed_64bits(0); // then use a different seed at each run

  } else
      TRACE("No parameter file was passed through the command line. I will use the default parameters.\n");

  // Spatial parameters for initial density function.
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

  mesh = create_mesh_3d(ncx, ncy, ncz, x_min, x_max, y_min, y_max, z_min, z_max);
  poisson = new_poisson_3d_fft_solver(mesh);

  // END OF "DO NOT CHANGE, THIS IS COPY PASTED FROM PIC-VERT"

  // Convert PIC_VERT variables to verified_transfo variables.
  stepDuration = delta_t;
  nbSteps = num_iteration;
  nbParticles = nb_particles;
  gridX = ncx;
  gridY = ncy;
  gridZ = ncz;
  areaX = x_max - x_min;
  areaY = y_max - y_min;
  areaZ = z_max - z_min;
  const double q = -1.; // particle charge density
  const double m =  1.; // particle mass density
  averageChargeDensity = q;
  averageMassDensity = m;
}

// --------- Poisson solver

void allocateStructuresForPoissonSolver() {
  // Allocate and rho, Ex, Ey, Ez arrays used by the Poisson solver
  rho = allocate_3d_array(gridX, gridY, gridZ);
  Ex = allocate_3d_array(gridX, gridY, gridZ);
  Ey = allocate_3d_array(gridX, gridY, gridZ);
  Ez = allocate_3d_array(gridX, gridY, gridZ);
}

void deallocateStructuresForPoissonSolver() {
  deallocate_3d_array(rho, gridX, gridY, gridZ);
  deallocate_3d_array(Ex, gridX, gridY, gridZ);
  deallocate_3d_array(Ey, gridX, gridY, gridZ);
  deallocate_3d_array(Ez, gridX, gridY, gridZ);
}

void computeFieldFromRho() { // reads [double*** rho], writes [vect* field]
  // Execute Poisson Solver (the '0' deactivates a feature used in pic_barsmian.c)
  compute_E_from_rho_3d_fft(poisson, rho, Ex, Ey, Ez, 0);

  // Fill in the field array
  for (int i = 0; i < gridX; i++) {
    for (int j = 0; j < gridY; j++) {
      for (int k = 0; k < gridZ; k++) {
        vect e = { Ex[i][j][k], Ey[i][j][k], Ez[i][j][k] };
        field[cellOfCoord(i,j,k)] = e;
#ifdef DEBUG_FIELD
        printf("field<%d>[%d][%d][%d] = %g %g %g\n", cellOfCoord(i,j,k), i, j, k, e.x, e.y, e.z);
#endif
      }
    }
  }
}

// --------- Particle Creation

void createParticles() {
  // TRACE("Filling particles on %d cells\n", nbCells);

#ifdef PRINTPERF
  double timeInitStart = omp_get_wtime();
#endif

  // Creation of random particles and put them into bags.
  pic_vert_seed_double_RNG(seed);
  { // Adapted from PIC-VERT with amendments
    double x, y, z, vx, vy, vz;
    double control_point, evaluated_function;
    speeds_generator_3d speeds_generator = speed_generators_3d[sim_distrib];
    distribution_function_3d distrib_function = distribution_funs_3d[sim_distrib];
    max_distribution_function max_distrib_function = distribution_maxs_3d[sim_distrib];

    const double x_range = mesh.x_max - mesh.x_min;
    const double y_range = mesh.y_max - mesh.y_min;
    const double z_range = mesh.z_max - mesh.z_min;

    TRACE("Creating %ld particles\n", nbParticles);
    // Create particles and push them into the bags.
    for (int idParticle = 0; idParticle < nbParticles; idParticle++) {
        do {
            // x, y, z are offsets from mesh x/y/z/min
            x = x_range * pic_vert_next_random_double();
            y = y_range * pic_vert_next_random_double();
            z = z_range * pic_vert_next_random_double();
            control_point = (*max_distrib_function)(params) * pic_vert_next_random_double();
            evaluated_function = (*distrib_function)(params, x + mesh.x_min, y + mesh.y_min, z + mesh.z_min);
        } while (control_point > evaluated_function);
        (*speeds_generator)(speed_params, &vx, &vy, &vz);
        // The rest of the loop body is modified compared with pic-vert
#ifdef DEBUG_CREATION
        printf("Created = %d, %lf %lf %lf %lf %lf %lf \n", idParticle, x, y, z, vx, vy, vz);
#endif
        addParticle(CHECKER_ONLY_COMMA(idParticle) x, y, z, vx, vy, vz);
    }
  }

#ifdef PRINTPERF
  double timeInit = (double) (omp_get_wtime() - timeInitStart);
  printf("Creation time (including charge accumulation): %.3f sec\n", timeInit);
#endif
}

// --------- Reporting

void reportPerformance(double timeStart) {
#ifdef PRINTPERF
  double timeTotal = (double) (omp_get_wtime() - timeStart);
  printf("Exectime: %.3f sec\n", timeTotal);
  printf("Throughput: %.1f million particles/sec\n", nbParticles * nbSteps / timeTotal / 1000000);
#endif
}

void reportParticlesState() {
#ifdef CHECKER
  // printf("NbParticles: %d\n", nbParticles);
  FILE* f = fopen(CHECKER_FILENAME, "wb");
  fwrite(&nbParticles, sizeof(int), 1, f);
  fwrite(&areaX, sizeof(double), 1, f);
  fwrite(&areaY, sizeof(double), 1, f);
  fwrite(&areaZ, sizeof(double), 1, f);
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag* b = &bagsCur[idCell];
    bag_iter bag_it;
    for (particle* p = bag_iter_begin(&bag_it, b); p != NULL; p = bag_iter_next(&bag_it)) {
      fwrite(&(p->id), sizeof(int), 1, f);
      fwrite(&(p->pos.x), sizeof(double), 1, f);
      fwrite(&(p->pos.y), sizeof(double), 1, f);
      fwrite(&(p->pos.z), sizeof(double), 1, f);
      fwrite(&(p->speed.x), sizeof(double), 1, f);
      fwrite(&(p->speed.y), sizeof(double), 1, f);
      fwrite(&(p->speed.z), sizeof(double), 1, f);
#ifdef DEBUG_CHECKER
      printf("id=%d %lf %lf %lf %lf %lf %lf\n", p->id,
        p->pos.x, p->pos.y, p->pos.z,
        p->speed.x, p->speed.y, p->speed.z);
#endif
    }
  }
  fclose(f);
#endif
}


