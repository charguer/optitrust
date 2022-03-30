#ifndef PIC_DEMO_H
#define PIC_DEMO_H

// --------- Includes

#include <omp.h> // functions omp_get_wtime, omp_get_num_threads, omp_get_thread_num
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

// --------- Macros for the correctness checker

#include "mymacros.h"
#include "particle.h"
#include "bag.h"

// --------- Simulation parameters // LATER: these parameters should be declared "extern"

// Physical space in which particles move
double areaX;
double areaY;
double areaZ;

// Description of the grid that is mapped onto the space
int gridX;
int gridY;
int gridZ;
int nbCells; // nbCells = gridX * gridY * gridZ;

// Derived grid parameters
double cellX;  // = areaX / gridX;
double cellY;  // = areaY / gridY;
double cellZ;  // = areaZ / gridZ;

// Description of the number of steps and the duration of each time step
int nbSteps;
double stepDuration;

// Description of the particles
double averageChargeDensity;
double averageMassDensity;
double particleCharge;
double particleMass;
int nbParticles;

// Parameters for randomized particle creation at t=0
unsigned char sim_distrib; // type of pattern for the distribution
double *params; // parameters for the distribution of positions
double *speed_params; // parameters for the distribution of speeds
int seed; // seed for random generator


// --------- Data structures

// 3D-arrays used during Poisson computations
double*** rho;
double*** Ex;
double*** Ey;
double*** Ez;

// Arrays used to deposit the electric field, and the contribution to the charge density
// - field[idCell] corresponds to the field at the top-right corner of the cell idCell;
// - deposit[idCell] corresponds to the cell in the front-top-left corner of that cell;
// The grids are, here again, treated with wrap-around
vect* field;
double* deposit;

// Bags used to store particles at the current and at the next time step.
bag* bagsCur;
bag* bagsNext;

// forward declaration for a function used by createParticles, which is defined in pic_demo_aux.c
void addParticle(CHECKER_ONLY_COMMA(int idParticle) double x, double y, double z, double vx, double vy, double vz);

// forward declaration for a function used by computeFieldFromRho, which is defined in pic_demo_aux.c
int cellOfCoord(int i, int j, int k);

void allocateStructures();
void allocateStructuresForPoissonSolver();
void deallocateStructures();
void deallocateStructuresForPoissonSolver();

#endif