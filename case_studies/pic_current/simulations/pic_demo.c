#include <omp.h>                                          // functions omp_get_wtime, omp_get_num_threads, omp_get_thread_num
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "parameter_reader.h"                             // type      simulation_parameters
                                                          // constants STRING_NOT_SET, INT_NOT_SET, DOUBLE_NOT_SET
                                                          // function  read_parameters_from_file

#define TRACE printf

// --------- Bags of particles

// Import the chunked sequence data structure, specialized to particles
// In OptiTrust, we want to actually inline that code.

// implicitly includes particle.h and optitrust.h
#include "particle_chunk.h"
#include "particle_chunk_alloc.h"

// --------- Parameters

// This code does not assume the cell size to be normalized,
// not the charge to be normalized; the normalization will
// be implemented in the transformations

//  physical parameter of the simulation
double areaX;
double areaY;
double areaZ;

double stepDuration;
double particleCharge;
double particleMass;

// Grid description
int gridX;
int gridY;
int gridZ;
int nbCells; // nbCells = gridX * gridY * gridZ;

// Derived grid parameters
double cellX;  // = areaX / gridX;
double cellY;  // = areaY / gridY;
double cellZ;  // = areaZ / gridZ;

// duration of the simulation
int nbSteps;

// --------- Grid coordinate functions

// from double to int
int int_of_double(double a) {
  return (int) a - (a < 0.);
}

int wrap(int gridSize, int a) {
  return (a % gridSize + gridSize) % gridSize;
}

/* Other possible implementations for wrap
  // assuming that a particle does not traverse the grid more than once in a timestep
   return (x % gridSize + gridSize) % gridSize
  // use of fmod possible
  // version without modulo but using if statements
    if (x < 0)
       return x + gridSize;
    if (x >= gridSize)
       return x - gridSize;
    return x;
*/

// --------- Grid Representation

// const int nbCorners = 8;
#define nbCorners 8

vect* fields;


int cellOfCoord(int i, int j, int k) {
  return MINDEX3(gridX,gridY,gridZ,i,j,k);
}

// idCellOfPos computes the id of the cell that contains a position.
int idCellOfPos(vect pos) {
  int iX = int_of_double(pos.x / cellX);
  int iY = int_of_double(pos.y / cellY);
  int iZ = int_of_double(pos.z / cellZ);
  return cellOfCoord(iX, iY, iZ);
}

double relativePosX(double x) {
  int iX = int_of_double(x / cellX);
  return (x - iX * cellX) / cellX;
}
double relativePosY(double y) {
  int iY = int_of_double(y / cellY);
  return (y - iY * cellY) / cellY;
}
double relativePosZ(double z) {
  int iZ = int_of_double(z / cellZ);
  return (z -  iZ * cellZ) / cellZ;
}

typedef struct {
  int iX;
  int iY;
  int iZ;
} coord;

coord coordOfCell(int idCell) {
  const int iZ = idCell % gridZ;
  const int iXY = idCell / gridZ;
  const int iY = iXY % gridY;
  const int iX = iXY / gridY;
  return (coord) { iX, iY, iZ };
}

typedef struct {
  int v[nbCorners];
} int_nbCorners;

typedef struct {
  double v[nbCorners];
} double_nbCorners;

typedef struct {
  vect v[nbCorners];
} vect_nbCorners;

int_nbCorners indicesOfCorners(int idCell) {
  const coord coord = coordOfCell(idCell);
  const int x = coord.iX;
  const int y = coord.iY;
  const int z = coord.iZ;
  const int x2 = wrap(gridX, x+1);
  const int y2 = wrap(gridY, y+1);
  const int z2 = wrap(gridZ, z+1);
  return (int_nbCorners) {
    cellOfCoord(x,y,z),
    cellOfCoord(x,y,z2),
    cellOfCoord(x,y2,z),
    cellOfCoord(x,y2,z2),
    cellOfCoord(x2,y,z),
    cellOfCoord(x2,y,z2),
    cellOfCoord(x2,y2,z),
    cellOfCoord(x2,y2,z2),
  };

}

vect_nbCorners getFieldAtCorners(int idCell, vect* field) {
  const int_nbCorners indices = indicesOfCorners(idCell);
  vect_nbCorners res;
  for (int k = 0; k < nbCorners; k++) {
    res.v[k] = field[indices.v[k]];
  }
  return res;

}

// Total charge of the particles already placed in the cell for the next time step
// charge are also accumulated in the corners of the cells

void accumulateChargeAtCorners(double* nextCharge, int idCell, double_nbCorners charges) {
  const int_nbCorners indices = indicesOfCorners(idCell);
  for (int k = 0; k < nbCorners; k++) {
    nextCharge[indices.v[k]] += charges.v[k];
  }
}

// --------- Interpolation operations

// given the relative position inside a cell, with coordinates in the range [0,1],
// compute the coefficient for interpolation at each corner;
// the value for one corner is proportional to the volume between the particle
// and the opposite corner.

double_nbCorners cornerInterpolationCoeff(vect pos) {
  const double rX = relativePosX(pos.x);
  const double rY = relativePosY(pos.y);
  const double rZ = relativePosZ(pos.z);
  const double cX = 1. + -1. * rX;
  const double cY = 1. + -1. * rY;
  const double cZ = 1. + -1. * rZ;
  double_nbCorners r;
  r.v[0] = cX * cY * cZ;
  r.v[1] = cX * cY * rZ;
  r.v[2] = cX * rY * cZ;
  r.v[3] = cX * rY * rZ;
  r.v[4] = rX * cY * cZ;
  r.v[5] = rX * cY * rZ;
  r.v[6] = rX * rY * cZ;
  r.v[7] = rX * rY * rZ;
  return r;
}

vect matrix_vect_mul(const double_nbCorners coeffs, const vect_nbCorners matrix) {
  vect res = { 0., 0., 0. };
  for (int k = 0; k < nbCorners; k++) {
    res = vect_add(res, vect_mul(coeffs.v[k], matrix.v[k]));
  }
  return res;
}

double_nbCorners vect8_mul(const double a, const double_nbCorners data) {
  double_nbCorners res;
  for (int k = 0; k < nbCorners; k++) {
    res.v[k] = a * data.v[k];
  }
  return res;
}


#include "poisson_solvers.h"
poisson_3d_solver poisson; // TODO: is this really passed by value to compute_E_from_rho_3d_fft? not by pointer?
// Arrays used during Poisson computations

double*** rho;
double*** Ex;
double*** Ey;
double*** Ez;
// nextCharge[idCell] corresponds to the cell in the front-top-left corner of that cell
double* nextCharge;
// Strength of the field that applies to each cell
// fields[idCell] corresponds to the field at the top-right corner of the cell idCell;
// The grid is treated with wrap-around
vect* field;


// Particles in each cell, at the current and the next time step
bag* bagsCur;
bag* bagsNext;

#include "parameters.h"                                   // constants PI, EPSILON, DBL_DECIMAL_DIG, FLT_DECIMAL_DIG, NB_PARTICLE
// TODO: it would be simpler if the Poisson module could take rho directly as an array indexed by idCell
void computeRhoForPoisson(double* nextCharge, double*** rho) {
  for (int i = 0; i < gridX; i++) {
    for (int j = 0; j < gridY; j++) {
      for (int k = 0; k < gridZ; k++) {
        rho[i][j][k] = nextCharge[cellOfCoord(i,j,k)];
      }
    }
  }
}

void resetIntArray(double* array) {
  for (int idCell = 0; idCell < nbCells; idCell++) {
    array[idCell] = 0;
  }
}

// updateFieldsUsingNextCharge in an operation that reads nextCharge,
// and updates the values in the fields array.
void updateFieldUsingNextCharge(double* nextCharge, vect* field) {

  // Compute Rho from nextCharge
  computeRhoForPoisson(nextCharge, rho);

  // Execute Poisson Solver (0 avoids the useless cell at borders which contains the same data)
  compute_E_from_rho_3d_fft(poisson, rho, Ex, Ey, Ez, 0);

  // Fill in the field array
  for (int i = 0; i < gridX; i++) {
    for (int j = 0; j < gridY; j++) {
      for (int k = 0; k < gridZ; k++) {
        field[cellOfCoord(i,j,k)] = (vect) { Ex[i][j][k], Ey[i][j][k], Ez[i][j][k] };
      }
    }
  }

  // Reset nextCharge
  resetIntArray(nextCharge);
}


#include "matrix_functions.h"                             // functions allocate_3d_array, deallocate_3d_array
#include "random.h"                                       // macros    pic_vert_seed_double_RNG, pic_vert_free_RNG
#include "initial_distributions.h"                        // types     speeds_generator_3d, distribution_function_3d, max_distribution_function
                                                          // variables speed_generators_3d, distribution_funs_3d, distribution_maxs_3d
void init(int argc, char** argv) {
    TRACE("Start init\n");
 /****************************
  * DO NOT CHANGE            *
  * COPY/PASTE FROM PIC-VERT *
  ****************************/
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
        TRACE("No parameter file was passed through the command line. I will use the default parameters.\n");

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
  poisson = new_poisson_3d_fft_solver(mesh);
 /*********************
  * END DO NOT CHANGE *
  *********************/

  // Convert PIC_VERT variables to verified_transfo variables.
  stepDuration = delta_t;
  double totalCharge = -1.;
    /* A "numerical particle" (we also say "macro particle") represents several
     * physical particles. The weight is the number of physical particles it
     * represents. The more particles we have in the simulation, the less this
     * weight will be. A numerical particle may represent a different number of
     * physical particles than another numerical particle, even though in this
     * simulation it's not the case.
     */
  particleCharge = totalCharge / nb_particles;
  particleMass =  1.;
  nbSteps = num_iteration;
  gridX = ncx;
  gridY = ncy;
  gridZ = ncz;
  areaX = x_max - x_min;
  areaY = y_max - y_min;
  areaZ = z_max - z_min;

  // New verified_transfo variables.
  nbCells = gridX * gridY * gridZ;
  cellX = areaX / gridX;
  cellY = areaY / gridY;
  cellZ = areaZ / gridZ;

  // initialize poisson solver, and rho, Ex, Ey, Ez arrays
  rho = allocate_3d_array(gridX, gridY, gridZ);
  Ex = allocate_3d_array(gridX, gridY, gridZ);
  Ey = allocate_3d_array(gridX, gridY, gridZ);
  Ez = allocate_3d_array(gridX, gridY, gridZ);
  nextCharge = (double*) malloc(nbCells * sizeof(double));
  // Reset nextCharge
  resetIntArray(nextCharge);
  // Not initializes in this function, only allocated
  field = (vect*) malloc(nbCells * sizeof(vect));

  // Later in optimizations: call an 'initialize' function in particle_chunk_alloc

  TRACE("Fill particles\n");
  // Initialize bagsNext and bagsCur with empty bags in every cell
  bagsCur = (bag*) malloc(nbCells * sizeof(bag));
  bagsNext = (bag*) malloc(nbCells * sizeof(bag));
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_init_initial(&bagsCur[idCell]);
    bag_init_initial(&bagsNext[idCell]);
  }

  int seed = 0;
  // If you want different random number at each run, type instead
  // seed = seed_64bits(0);
  pic_vert_seed_double_RNG(seed);

  // Creation of random particles and put them into bags.
  { // COPY PASTE FROM PIC-VERT WITH AMENDMENTS
    double x, y, z, vx, vy, vz;
    double control_point, evaluated_function;
    speeds_generator_3d speeds_generator = speed_generators_3d[sim_distrib];
    distribution_function_3d distrib_function = distribution_funs_3d[sim_distrib];
    max_distribution_function max_distrib_function = distribution_maxs_3d[sim_distrib];

    const double x_range = mesh.x_max - mesh.x_min;
    const double y_range = mesh.y_max - mesh.y_min;
    const double z_range = mesh.z_max - mesh.z_min;

    TRACE("Create particles %ld\n", nb_particles);
    // Create particles and push them into the bags.
    for (int idParticle = 0; idParticle < nb_particles; idParticle++) {
        do {
            // x, y, z are offsets from mesh x/y/z/min
            x = x_range * pic_vert_next_random_double();
            y = y_range * pic_vert_next_random_double();
            z = z_range * pic_vert_next_random_double();
            control_point = (*max_distrib_function)(params) * pic_vert_next_random_double();
            evaluated_function = (*distrib_function)(params, x + mesh.x_min, y + mesh.y_min, z + mesh.z_min);
        } while (control_point > evaluated_function);
        (*speeds_generator)(speed_params, &vx, &vy, &vz);
        // Modified from pic-vert
        const vect pos = { x, y, z };
        const vect speed = { vx, vy, vz };
        const particle particle = { pos, speed };
        const int idCell = idCellOfPos(pos);
        bag_push_initial(&bagsCur[idCell], particle);
    }
  }

  TRACE("First poisson\n");
  // Poisson solver to compute field at time zero, and reset nextCharge
  updateFieldUsingNextCharge(nextCharge, field);

  TRACE("Leap frog (chunksize=%d)\n", CHUNK_SIZE);
  // Computes speeds backwards for half a time-step (leap-frog method)
  double negHalfStepDuration = -0.5 * stepDuration;
  // For each cell from the grid
  for (int idCell = 0; idCell < nbCells; idCell++) {
    // Consider the bag of particles in that cell
    bag* b = &bagsCur[idCell];
    // TRACE("Leap frog cell %d, bag %p\n", idCell, b);
    bag_iter bag_it;
    // Compute fields at corners of the cell
    vect_nbCorners field_at_corners = getFieldAtCorners(idCell, field);
    // For each particle in that cell
    int k = 0;
    for (particle* p = bag_iter_begin(&bag_it, b); p != NULL; p = bag_iter_next(&bag_it)) {
        // TRACE("leap frog step %d\n", k++);
        // Interpolate the field based on the position relative to the corners of the cell
        double_nbCorners coeffs = cornerInterpolationCoeff(p->pos);
        // TRACE("LOOP2\n");
        vect fieldAtPos = matrix_vect_mul(coeffs, field_at_corners);
        // Compute the acceleration: F = m*a and F = q*E  gives a = q/m*E
        // TRACE("LOOP3\n");
        vect accel = vect_mul(particleCharge / particleMass, fieldAtPos);
        // Compute the new speed and position for the particle.
        // TRACE("LOOP4\n");
        p->speed = vect_add(p->speed, vect_mul(negHalfStepDuration, accel));
    }
  }
  TRACE("Init end\n");
    exit(0);
}

void finalize(bag* bagsCur, bag* bagsNext, vect* field) {
  TRACE("Finalize\n");
  // Later in optimizations: call a 'finalize' function in particle_chunk_alloc

  // Free the chunks
  for (int idCell = 0; idCell < nbCells; idCell++) {
    bag_free_initial(&bagsCur[idCell]);
    bag_free_initial(&bagsNext[idCell]);
  }

  // Free arrays
  free(bagsCur);
  free(bagsNext);
  free(field);
}


// --------- Module Simulation


int main(int argc, char** argv) {

  init(argc, argv);

  // Instrumentation of the code
  double time_start = omp_get_wtime();
  TRACE("Simulate\n");

  // Foreach time step
  for (int step = 0; step < nbSteps; step++) {
    TRACE("Step %d\n", step);
    // Update the new field based on the total charge accumulated in each cell,
    // and reset nextCharge.
    updateFieldUsingNextCharge(nextCharge, field);

    // For each cell from the grid
    for (int idCell = 0; idCell < nbCells; idCell++) {

      // Read the electric field that applies to the corners of the cell considered
      vect_nbCorners field_at_corners = getFieldAtCorners(idCell, field);

      // Consider the bag of particles in that cell
      bag* b = &bagsCur[idCell];

      bag_iter bag_it;
      for (particle* p = bag_iter_begin(&bag_it, b); p != NULL; p = bag_iter_next_destructive(&bag_it)) {

        // Interpolate the field based on the position relative to the corners of the cell
        double_nbCorners coeffs = cornerInterpolationCoeff(p->pos);
        vect fieldAtPos = matrix_vect_mul(coeffs, field_at_corners);

        // Compute the acceleration: F = m*a and F = q*E  gives a = q/m*E
        vect accel = vect_mul(particleCharge / particleMass, fieldAtPos);

        // Compute the new speed and position for the particle.
        vect speed2 = vect_add(p->speed, vect_mul(stepDuration, accel));
        vect pos2 = vect_add(p->pos, vect_mul(stepDuration, speed2));
        particle p2 = { pos2, speed2 };

        // Compute the location of the cell that now contains the particle
        int idCell2 = idCellOfPos(pos2);

        // Push the updated particle into the bag associated with its target cell
        bag_push(&bagsNext[idCell2], p2);

        // Deposit the charge of the particle at the corners of the target cell
        double_nbCorners coeffs2 = cornerInterpolationCoeff(pos2);
        double_nbCorners deltaChargeOnCorners = vect8_mul(particleCharge, coeffs2);
        accumulateChargeAtCorners(nextCharge, idCell2, deltaChargeOnCorners);
      }
      bag_init_initial(b);
    }

    // For the next time step, the contents of bagNext is moved into bagCur (which is empty)
    TRACE("Swap\n");
    for (int idCell = 0; idCell < nbCells; idCell++) {
      bag_swap(&bagsCur[idCell], &bagsNext[idCell]);
    }

    // Poisson solver and reset nextCharge
    TRACE("Poisson\n");
    updateFieldUsingNextCharge(nextCharge, field);
  }
  double time_simu = (double) (omp_get_wtime() - time_start);
  // TODO: TRACE

  finalize(bagsCur, bagsNext, field);
}


// LATER: When ClangML supports it, we'll use overloaded + and * operators on class vect
// LATER: When ClangML supports it, we'll use higher-order iteration with a local function
// LATER: When ClangML supports it, we'll use boost arrays for fixed size arrays


// TODO: rename "_nbCorners" to 8
// TODO: move particle p2 = just before the bag_push
// TODO: replace double cX = 1. + -1. * rX;   with   double cX = 1. - rX;   and use a transformation for this change
// TODO: int x =     make those uppercase in indicesOfCorners
