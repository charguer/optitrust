#ifndef PIC_VERT_INITIAL_DISTRIBUTIONS
#define PIC_VERT_INITIAL_DISTRIBUTIONS

#include <math.h>           // functions cos, sin, log, sqrt
#include <stdlib.h>         // functions drand48 (random generator)
#include "parameters.h"     // constant  PI
#include "math_functions.h" // function  sqr

/*****************************************************************************
 *                           Random generators                               *
 *****************************************************************************/

/*
 * 0. Note : we could also use the Box-Muller algorithm for Maxwellian 2d.
 *
 *
 * I. Random generation of positions or speeds through the inversion of the
 * cumulative distribution function.
 * 
 * To generate numbers that follows a given density f, rather than the uniform law :
 *     1. compute the cumulative distribution function
 *        rep(x) = int(f(t),   t=-infinity..x); (cartesian)
 *        rep(x) = int(f(r)*r, r=0..x);         (polar)
 *     2. compute the invert of this function
 *        inv(x) = solve(x = rep(y), y);
 *     3. pick a random z in [0. ; 1[ (uniformly)
 *     4. return inv(z);
 *
 * N.B. : drand48() returns doubles uniformly distributed in the interval [0. ; 1.[.
 *
 *
 * II. Random generation of positions or speeds through acceptance / reject algorithm.
 *
 * To generate a number that follows a given distribution f, rather than
 * the uniform distribution :
 *     1. pick a random x in [x_min ; x_max[ (uniformly)
 *     2. pick a random z in [0. ; max(f)[ (uniformly)
 *     3. if (x, z) is below the curve of f, accept the value of x
 *        else, pick again.
 */

typedef double (*max_distribution_function)(double*);



typedef void (*speeds_generator_1d)(double*, double*);
typedef double (*distribution_function_1d)(double*, double);

/*
 * Below are available test distribution functions :
 * 0. PARAXIAL_1D for Paraxial 1d
 *    f_0(x, v) = 1/(sqrt(2 * pi) * v_{th}) * exp(−(vx^2) / (2 * v_{th}^2))
 */
#define PARAXIAL_1D     0
#define NB_TEST_CASE_1D 1

char distribution_names_1d[NB_TEST_CASE_1D][42];
speeds_generator_1d speed_generators_1d[NB_TEST_CASE_1D];
distribution_function_1d distribution_funs_1d[NB_TEST_CASE_1D];
max_distribution_function distribution_maxs_1d[NB_TEST_CASE_1D];



typedef void (*speeds_generator_2d)(double*, double*, double*);
typedef double (*distribution_function_2d)(double*, double, double);

/*
 * dens(r) = exp(-r**2 / 2) is the speed density,
 * in polar [ it is Maxwellian(v) ]
 * We compute the repartition function, which is int(dens(r)*r*dr, 0, x),
 * Maple gives :
 *    int(exp(-r**2 / 2) * r, r=0..x);
 * rep(x) = 1 - exp(-x**2 / 2)
 * We then have to find the invert of this function,
 * Maple gives :
 *    solve(y = 1 - exp(-x**2 / 2), x);
 * inv(x) = sqrt(-2 * ln(1 - x))
 *
 * @param[out] vx.
 * @param[out] vy.
 */
void maxwellian_2d(double* params, double* vx, double* vy);

/*
 * Below are available test distribution functions :
 * 0. LANDAU_1D_PROJ2D for Landau 1d
 *    f_0(x, v) = 1/(2 * pi * v_{th}^2) * (1 + alpha * cos(kx * x))
 *                * exp(−(vx^2 +vy^2) / (2 * v_{th}^2))
 * 1. LANDAU_2D for Landau 2d
 *    f_0(x, v) = 1/(2 * pi * v_{th}^2) * (1 + alpha * cos(kx * x) * cos(ky * y))
 *                * exp(−(vx^2 +vy^2) / (2 * v_{th}^2))
 * 2. KELVIN_HELMHOLTZ for Kelvin Helmholtz
 *    f_0(x, v) = 1/(2 * pi * v_{th}^2) * (1 + alpha * cos(kx * x) + alpha + sin(y))
 *                * exp(−(vx^2 +vy^2) / (2 * v_{th}^2))
 * 3. TWO_STREAM for Two Stream (Bernier)
 *    f_0(x, v) = 1/(2 * pi * v_{th}^2) * (1 + alpha *  (cos(mode1x * kx * x + mode1y * ky * y) +
 *                                                       cos(mode2x * kx * x + mode2y * ky * y)))
 *                * vx^2 * exp(−(vx^2 +vy^2) / (2 * v_{th}^2))
 * 4. TWO_BEAMS for Fijalkow Two Beams Instability
 *    f_0(x, v) = 7 / (4 * pi) * exp(-(vx^2 + 4 * vy^2) / 8) * (sin(vx / 3))^2 * (1 + alpha * cos(kx * x))
 */
#define LANDAU_1D_PROJ2D     0
#define LANDAU_2D            1
#define KELVIN_HELMHOLTZ     2
#define TWO_STREAM_BERNIER   3
#define TWO_BEAMS_FIJALKOW   4
#define TWO_STREAM_1D_PROJ2D 5
#define TWO_STREAM_2D        6
#define NB_TEST_CASE_2D      7

char distribution_names_2d[NB_TEST_CASE_2D][42];
speeds_generator_2d speed_generators_2d[NB_TEST_CASE_2D];
distribution_function_2d distribution_funs_2d[NB_TEST_CASE_2D];
max_distribution_function distribution_maxs_2d[NB_TEST_CASE_2D];



typedef void (*speeds_generator_3d)(double*, double*, double*, double*);
typedef double (*distribution_function_3d)(double*, double, double, double);

/*
 * Below are available test distribution functions :
 * 0. LANDAU_1D_PROJ3D for Landau 1d
 *    f_0(x, v) = 1/(2 * pi * v_{th}^2) * (1 + alpha * cos(kx * x))
 *                * exp(−(vx^2 +vy^2) / (2 * v_{th}^2))
 * 1. LANDAU_2D_PROJ3D for Landau 2d
 *    f_0(x, v) = 1/(2 * pi * v_{th}^2) * (1 + alpha * cos(kx * x) * cos(ky * y))
 *                * exp(−(vx^2 +vy^2) / (2 * v_{th}^2))
 */
#define LANDAU_1D_PROJ3D               0
#define LANDAU_2D_PROJ3D               1
#define LANDAU_3D_SUM_OF_COS           2
#define LANDAU_3D_PROD_OF_COS          3
#define LANDAU_3D_PROD_OF_ONE_PLUS_COS 4
#define NB_TEST_CASE_3D                5

char distribution_names_3d[NB_TEST_CASE_3D][42];
speeds_generator_3d speed_generators_3d[NB_TEST_CASE_3D];
distribution_function_3d distribution_funs_3d[NB_TEST_CASE_3D];
max_distribution_function distribution_maxs_3d[NB_TEST_CASE_3D];



/*
 * Below are available test distribution functions :
 * 0. ELECTRON_HOLES_2D3V for Electron holes in phase space 2d3v
 *
 * This specific 2d3v testcase links positions and speeds at initial time.
 * We have thus a specific code for this specific test case, different
 * from the other initializers, namely:
 *     "distribution_function_2d" initializes (x, vx) and not (x, y).
 *     y is uniform.
 *     "speeds_generator_2d" initializes (vy, vz) and not (vx, vy, vz).
 */
#define ELECTRON_HOLES_2D3V 0
#define NB_TEST_CASE_2D3V   1

char distribution_names_2d3v[NB_TEST_CASE_2D3V][42];
speeds_generator_2d speed_generators_2d3v[NB_TEST_CASE_2D3V];
distribution_function_2d distribution_funs_2d3v[NB_TEST_CASE_2D3V];
max_distribution_function distribution_maxs_2d3v[NB_TEST_CASE_2D3V];

#endif // ifndef PIC_VERT_INITIAL_DISTRIBUTIONS


