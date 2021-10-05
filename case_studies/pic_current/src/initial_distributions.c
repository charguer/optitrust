#include <stdio.h>                 // function  fprintf (output strings on a stream)
                                   // constant  stderr (standard error output stream)
#include <stdlib.h>                // functions exit (error handling)
                                   // constant  EXIT_FAILURE (error handling)
#include <math.h>                  // functions cos, sin, log, sqrt, pow
#include "parameters.h"            // constant  PI
#include "math_functions.h"        // function  sqr
#include "initial_distributions.h" // constants NB_TEST_CASE_1D, NB_TEST_CASE_2D, NB_TEST_CASE_3D
                                   // types     max_distribution_function, speeds_generator_1d, distribution_function_1d
                                   //           speeds_generator_2d, distribution_function_2d, speeds_generator_3d, distribution_function_3d
#include "random.h"                // function  pic_vert_next_random_double

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

double max_distrib_Uniform         (double* params) {
    return 1.;
}
double max_distrib_One_mode        (double* params) {
    double alpha = params[0];
    return 1. + alpha;
}
double max_distrib_Kelvin_Helmholtz(double* params) {
    double alpha = params[0];
    return 2. + 2. * alpha;
}
double max_distrib_Sum_of_two_modes(double* params) {
    double alpha = params[0];
    return 1. + 2. * alpha;
}
double max_distrib_Bath_of_modes_2d(double* params) {
    double alpha      = params[0];
    double mode_max_x = params[3];
    double mode_max_y = params[4];
    double number_of_modes = 0.;
    for (int i_mode_x = 1; i_mode_x < mode_max_x; i_mode_x++)
        for (int i_mode_y = 1; i_mode_y < mode_max_y; i_mode_y++)
            number_of_modes += 1.;
    return 1. + alpha * number_of_modes;
}
double max_distrib_Landau_3d_sum_of_cos          (double* params) {
    double alpha = params[0];
// Kormann & Sonnendrucker, "Sparse Grids for the Vlasov–Poisson Equation", 2014, p.183
    return 1. + 3 * alpha;
}
double max_distrib_Landau_3d_prod_of_cos         (double* params) {
    double alpha = params[0];
    return 1. + alpha;
}
double max_distrib_Landau_3d_prod_of_one_plus_cos(double* params) {
    double alpha = params[0];
// Ricketson & Cerfon, "Sparse Grid Techniques For Particle-in-Cell Schemes", 2016
    return pow(1. + alpha, 3.);
}



double distrib_Uniform_1d(double* params, double x) {
    return 1.;
}

/*
 * Uses Maxwellian_2d.
 *
 * @param[out] vx.
 */
void maxwellian_1d(double* params, double* vx) {
    double throw_away;
    maxwellian_2d(params, vx, &throw_away);
}

/*
 * Below are available test distribution functions :
 * 0. PARAXIAL_1D for Paraxial 1d
 *    f_0(x, v) = 1/(sqrt(2 * pi) * v_{th}) * exp(−(vx^2) / (2 * v_{th}^2))
 */

char distribution_names_1d[NB_TEST_CASE_1D][42] = {
    [PARAXIAL_1D] = "PARAXIAL_1D",
};
speeds_generator_1d speed_generators_1d[NB_TEST_CASE_1D] = {
    [PARAXIAL_1D] = &maxwellian_1d,
};
distribution_function_1d distribution_funs_1d[NB_TEST_CASE_1D] = {
    [PARAXIAL_1D] = &distrib_Uniform_1d,
};
max_distribution_function distribution_maxs_1d[NB_TEST_CASE_1D] = {
    [PARAXIAL_1D] = &max_distrib_Uniform,
};



double distrib_One_mode_proj2d  (double* params, double x, double y) {
    double alpha = params[0];
    double kx    = params[1];
    return 1. + alpha * cos(kx * x);
}
double distrib_Prod_of_two_modes(double* params, double x, double y) {
    double alpha = params[0];
    double kx    = params[1];
    double ky    = params[2];
    return 1. + alpha * cos(kx * x) * cos(ky * y);
}
double distrib_Kelvin_Helmholtz (double* params, double x, double y) {
    double alpha = params[0];
    double kx    = params[1];
    return 1. + alpha * cos(kx * x) + alpha + sin(y);
}
double distrib_Sum_of_two_modes (double* params, double x, double y) {
    double alpha  = params[0];
    double kx     = params[1];
    double ky     = params[2];
    double mode1x = params[3];
    double mode1y = params[4];
    double mode2x = params[5];
    double mode2y = params[6];
    return 1. + alpha * (cos(mode1x * kx * x + mode1y * ky * y) +
                         cos(mode2x * kx * x + mode2y * ky * y));
}
double distrib_Bath_of_modes_2d (double* params, double x, double y) {
    double alpha      = params[0];
    double kx         = params[1];
    double ky         = params[2];
    double mode_max_x = params[3];
    double mode_max_y = params[4];
    double sum_of_cos = 0.;
    for (int i_mode_x = 1; i_mode_x < mode_max_x; i_mode_x++)
        for (int i_mode_y = 1; i_mode_y < mode_max_y; i_mode_y++)
            sum_of_cos += cos(i_mode_x * kx * x + i_mode_y * ky * y);
    return 1. + alpha * sum_of_cos;
}
double distrib_Uniform_2d       (double* params, double x, double y) {
    return 1.;
}

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
void maxwellian_2d(double* params, double* vx, double* vy) {
    double thermal_speed = params[0];
    double rsq, v1, v2;
    
    do {
       v1 = pic_vert_next_random_double();
       v2 = pic_vert_next_random_double();
       v1 = 2. * v1 - 1.;
       v2 = 2. * v2 - 1.;
       rsq = sqr(v1) + sqr(v2);
    } while (rsq >= 1.);
    
    // In math.h, log means ln
    rsq = sqrt(-2. * log(rsq) / rsq);
    *vx = v1 * rsq * thermal_speed;
    *vy = v2 * rsq * thermal_speed;
}

/*
 * Speeds taken in [-6 ; 6).
 * TODO: Parameter !
 *
 * For the function f(x, y) = x**2 * exp(-(x**2 + y**2) / 2) / (2 * Pi)
 * Maple gives :
 * solve(diff(x**2 * exp(-(x**2 + y**2) / 2) / (2 * Pi), x) = 0, x);
 *       0 ; sqrt(2) ; -sqrt(2)
 * solve(diff(x**2 * exp(-(x**2 + y**2) / 2) / (2 * Pi), y) = 0, y);
 *       0
 * f(sqrt(2), 0) = 0.1170996630
 *
 * @param[out] vx.
 * @param[out] vy.
 */
void polynomial_times_maxwellian_2d(double* params, double* vx, double* vy) {
    double thermal_speed = params[0];
    double control_point, evaluated_function;
    do {
        *vx = 12. * pic_vert_next_random_double() - 6.0;
        *vy = 12. * pic_vert_next_random_double() - 6.0;
        control_point = 0.1171 * pic_vert_next_random_double();
        evaluated_function = sqr(*vx) * exp(-(sqr(*vx) + sqr(*vy)) / 2.) / (2. * PI);
    } while (control_point > evaluated_function);
    *vx = *vx * thermal_speed;
    *vy = *vy * thermal_speed;
}

/*
 * Speeds taken in [-3 * pi ; 3 * pi).
 * TODO: Parameter !
 *
 * For the function f(x, y) = 7 / (4 * Pi) * exp(-(x**2 + 4 * y**2) / 8) * (sin(x / 3))**2
 * Maple gives :
 * solve(diff(7 / (4 * Pi) * exp(-(x**2 + 4 * y**2) / 8) * (sin(x / 3))**2, x) = 0, x);
 *       3 RootOf(9 _Z tan(_Z) - 8) ; 0
 * solve(diff(7 / (4 * Pi) * exp(-(x**2 + 4 * y**2) / 8) * (sin(x / 3))**2, y) = 0, y);
 *       0
 * evalf(3 * RootOf(9 * x * tan(x) - 8));
 *       -2.470622195
 * eval(7 / (4 * 3.14159265358979323846264338327) * exp(-((-2.470622195)**2 + 4 * 0**2) / 8) * (sin(-2.470622195 / 3))**2);
 *       0.1397624146
 * f(3 RootOf(9 _Z tan(_Z) - 8), 0) = 0.1397624146
 *
 * @param[out] vx.
 * @param[out] vy.
 */
void two_beams_2d(double* params, double* vx, double* vy) {
    double thermal_speed = params[0];
    double control_point, evaluated_function;
    do {
        *vx = 6. * PI * pic_vert_next_random_double() - 3. * PI;
        *vy = 6. * PI * pic_vert_next_random_double() - 3. * PI;
        control_point = 0.1398 * pic_vert_next_random_double();
        evaluated_function = 7. / (4. * PI) * exp(-(sqr(*vx) + 4. * sqr(*vy)) / 8.) * sqr(sin(*vx / 3.));
    } while (control_point > evaluated_function);
    *vx = *vx * thermal_speed;
    *vy = *vy * thermal_speed;
}

/*
 * Uses Maxwellian_2d.
 *
 * f(vx, vy) = 0.5 * exp(-0.5 * (vx - v_drift)**2 / v_th**2) + 0.5 * exp(-0.5 * (vx + v_drift)**2 / v_th**2)
 * vy taken uniformly in [-5 ; 5).
 * TODO: Parameter !
 *
 * @param[out] vx.
 * @param[out] vy.
 */
void two_stream_1d_proj_2d(double* params, double* vx, double* vy) {
    double thermal_speed = params[0];
    double v_drift       = params[1];
    double throw_away;
    maxwellian_2d(params, vx, &throw_away);
    if (pic_vert_next_random_double() > 0.5)
        *vx += v_drift;
    else
        *vx -= v_drift;
    *vy = (10. * pic_vert_next_random_double() - 5.) * thermal_speed;
}

/*
 * Below are available test distribution functions :
 * 0. LANDAU_1D_PROJ2D for Landau 1d
 *    f_0(x, v) = 1/(2 * pi * v_{th}^2) * (1 + alpha * cos(kx * x))
 *                * exp(−(vx^2 +vy^2) / (2 * v_{th}^2))
 * 1. LANDAU_2D for Landau 2d (product of two 1d modes)
 *    f_0(x, v) = 1/(2 * pi * v_{th}^2) * (1 + alpha * cos(kx * x) * cos(ky * y))
 *                * exp(−(vx^2 +vy^2) / (2 * v_{th}^2))
 * 2. KELVIN_HELMHOLTZ for Kelvin Helmholtz
 *    f_0(x, v) = 1/(2 * pi * v_{th}^2) * (1 + alpha * cos(kx * x) + alpha + sin(y))
 *                * exp(−(vx^2 +vy^2) / (2 * v_{th}^2))
 * 3. TWO_STREAM_BERNIER for Two Stream (sum of two 2d modes)
 *    f_0(x, v) = 1/(2 * pi * v_{th}^2) * (1 + alpha *  (cos(mode1x * kx * x + mode1y * ky * y) +
 *                                                       cos(mode2x * kx * x + mode2y * ky * y)))
 *                * vx^2 * exp(−(vx^2 +vy^2) / (2 * v_{th}^2))
 *    (cf. Y. Barsamian, J. Bernier, S. A. Hirstoaga and M. Mehrenberger,
 *    "Verification of 2D × 2D and two-species Vlasov-Poisson solvers", 2018)
 * 4. TWO_BEAMS_FIJALKOW for Fijalkow Two Beams Instability
 *    f_0(x, v) = 7 / (4 * pi) * exp(-(vx^2 + 4 * vy^2) / 8) * (sin(vx / 3))^2 * (1 + alpha * cos(kx * x))
 *    (cf. slides from E. Deriaz, Numkin 2016)
 */

char distribution_names_2d[NB_TEST_CASE_2D][42] = {
    [LANDAU_1D_PROJ2D]     = "LANDAU_1D_PROJ2D",
    [LANDAU_2D]            = "LANDAU_2D",
    [KELVIN_HELMHOLTZ]     = "KELVIN_HELMHOLTZ",
    [TWO_STREAM_BERNIER]   = "TWO_STREAM_BERNIER",
    [TWO_BEAMS_FIJALKOW]   = "TWO_BEAMS_FIJALKOW",
    [TWO_STREAM_1D_PROJ2D] = "TWO_STREAM_1D_PROJ2D",
    [TWO_STREAM_2D]        = "TWO_STREAM_2D",
};
speeds_generator_2d speed_generators_2d[NB_TEST_CASE_2D] = {
    [LANDAU_1D_PROJ2D]     = &maxwellian_2d,
    [LANDAU_2D]            = &maxwellian_2d,
    [KELVIN_HELMHOLTZ]     = &maxwellian_2d,
    [TWO_STREAM_BERNIER]   = &polynomial_times_maxwellian_2d,
    [TWO_BEAMS_FIJALKOW]   = &two_beams_2d,
    [TWO_STREAM_1D_PROJ2D] = &two_stream_1d_proj_2d,
    [TWO_STREAM_2D]        = &polynomial_times_maxwellian_2d,
};
distribution_function_2d distribution_funs_2d[NB_TEST_CASE_2D] = {
    [LANDAU_1D_PROJ2D]     = &distrib_One_mode_proj2d,
    [LANDAU_2D]            = &distrib_Prod_of_two_modes,
    [KELVIN_HELMHOLTZ]     = &distrib_Kelvin_Helmholtz,
    [TWO_STREAM_BERNIER]   = &distrib_Sum_of_two_modes,
    [TWO_BEAMS_FIJALKOW]   = &distrib_One_mode_proj2d,
    [TWO_STREAM_1D_PROJ2D] = &distrib_Bath_of_modes_2d,
    [TWO_STREAM_2D]        = &distrib_Prod_of_two_modes,
};
max_distribution_function distribution_maxs_2d[NB_TEST_CASE_2D] = {
    [LANDAU_1D_PROJ2D]     = &max_distrib_One_mode,
    [LANDAU_2D]            = &max_distrib_One_mode,
    [KELVIN_HELMHOLTZ]     = &max_distrib_Kelvin_Helmholtz,
    [TWO_STREAM_BERNIER]   = &max_distrib_Sum_of_two_modes,
    [TWO_BEAMS_FIJALKOW]   = &max_distrib_One_mode,
    [TWO_STREAM_1D_PROJ2D] = &max_distrib_Bath_of_modes_2d,
    [TWO_STREAM_2D]        = &max_distrib_One_mode,
};



double distrib_One_mode_proj3d               (double* params, double x, double y, double z) {
    double alpha = params[0];
    double kx    = params[1];
    return 1. + alpha * cos(kx * x);
}
double distrib_Prod_of_two_modes_proj3d      (double* params, double x, double y, double z) {
    double alpha = params[0];
    double kx    = params[1];
    double ky    = params[2];
    return 1. + alpha * cos(kx * x) * cos(ky * y);
}
double distrib_Landau_3d_sum_of_cos          (double* params, double x, double y, double z) {
    double alpha = params[0];
    double kx    = params[1];
    double ky    = params[2];
    double kz    = params[3];
// Kormann & Sonnendrucker, "Sparse Grids for the Vlasov–Poisson Equation", 2014, p.183
    return 1. + alpha * (cos(kx * x) + cos(ky * y) + cos(kz * z));
}
double distrib_Landau_3d_prod_of_cos         (double* params, double x, double y, double z) {
    double alpha = params[0];
    double kx    = params[1];
    double ky    = params[2];
    double kz    = params[3];
    return 1. + alpha * cos(kx * x) * cos(ky * y) * cos(kz * z);
}
double distrib_Landau_3d_prod_of_one_plus_cos(double* params, double x, double y, double z) {
    double alpha = params[0];
    double kx    = params[1];
    double ky    = params[2];
    double kz    = params[3];
// Ricketson & Cerfon, "Sparse Grid Techniques For Particle-in-Cell Schemes", 2016
    return (1. + alpha * cos(kx * x)) * (1. + alpha * cos(ky * y)) * (1. + alpha * cos(kz * z));
}
double distrib_Uniform_3d                    (double* params, double x, double y, double z) {
    return 1.;
}

/*
 * Uses Maxwellian_2d.
 *
 * Speed in z taken uniformly in [-6 ; 6).
 * TODO: Parameter !
 *
 * @param[out] vx.
 * @param[out] vy.
 * @param[out] vz.
 */
void maxwellian_2d_proj3d(double* params, double* vx, double* vy, double* vz) {
    maxwellian_2d(params, vx, vy);
    *vz = 12. * pic_vert_next_random_double() - 6.0;
}
/*
 * Uses Maxwellian_2d.
 *
 * @param[out] vx.
 * @param[out] vy.
 * @param[out] vz.
 */
void maxwellian_3d(double* params, double* vx, double* vy, double* vz) {
    double throw_away;
    maxwellian_2d(params, vx, vy);
    maxwellian_2d(params, vz, &throw_away);
}
/*
 * Speeds taken uniformly in [-1 ; 1).
 * TODO: Parameter !
 *
 * @param[out] vx.
 * @param[out] vy.
 * @param[out] vz.
 */
void uniform_3d(double* params, double* vx, double* vy, double* vz) {
    *vx = 2. * pic_vert_next_random_double() - 1.0;
    *vy = 2. * pic_vert_next_random_double() - 1.0;
    *vz = 2. * pic_vert_next_random_double() - 1.0;
}

/*
 * Uses Maxwellian_2d.
 *
 * One Maxwellian is centered on v_drift, the other is centered on 0.
 * f(vx, vy, vz) = 0.5 * exp(-0.5 * (vx - v_drift)**2 / v_th**2) + 0.5 * exp(-0.5 * vx**2 / v_th**2)
 *
 * @param[out] vx.
 * @param[out] vy.
 * @param[out] vz.
 */
void bi_maxwellians_3d(double* params, double* vx, double* vy, double* vz) {
//    double thermal_speed = params[0];
    double v_drift       = params[1];
    double proba_drift   = params[2];
    double throw_away;
    maxwellian_2d(params, vx, &throw_away);
    maxwellian_2d(params, vy, vz);
    if (pic_vert_next_random_double() < proba_drift)
        *vx += v_drift;
    if (pic_vert_next_random_double() < proba_drift)
        *vy += v_drift;
    if (pic_vert_next_random_double() < proba_drift)
        *vz += v_drift;
}

/*
 * Below are available test distribution functions :
 * 0. LANDAU_1D_PROJ3D for Landau 1d
 *    f_0(x, v) = 1/(2 * pi * v_{th}^2) * (1 + alpha * cos(kx * x))
 *                * exp(−(vx^2 +vy^2) / (2 * v_{th}^2))
 * 1. LANDAU_2D_PROJ3D for Landau 2d
 *    f_0(x, v) = 1/(2 * pi * v_{th}^2) * (1 + alpha * cos(kx * x) * cos(ky * y))
 *                * exp(−(vx^2 +vy^2) / (2 * v_{th}^2))
 */

char distribution_names_3d[NB_TEST_CASE_3D][42] = {
    [LANDAU_1D_PROJ3D]               = "LANDAU_1D_PROJ3D",
    [LANDAU_2D_PROJ3D]               = "LANDAU_2D_PROJ3D",
    [LANDAU_3D_SUM_OF_COS]           = "LANDAU_3D_SUM_OF_COS",
    [LANDAU_3D_PROD_OF_COS]          = "LANDAU_3D_PROD_OF_COS",
    [LANDAU_3D_PROD_OF_ONE_PLUS_COS] = "LANDAU_3D_PROD_OF_ONE_PLUS_COS",
    [UNIFORM_3D]                     = "UNIFORM_3D",
    [DRIFT_VELOCITIES_3D]            = "DRIFT_VELOCITIES_3D",
};
speeds_generator_3d speed_generators_3d[NB_TEST_CASE_3D] = {
    [LANDAU_1D_PROJ3D]               = &maxwellian_2d_proj3d,
    [LANDAU_2D_PROJ3D]               = &maxwellian_2d_proj3d,
    [LANDAU_3D_SUM_OF_COS]           = &maxwellian_3d,
    [LANDAU_3D_PROD_OF_COS]          = &maxwellian_3d,
    [LANDAU_3D_PROD_OF_ONE_PLUS_COS] = &maxwellian_3d,
    [UNIFORM_3D]                     = &uniform_3d,
    [DRIFT_VELOCITIES_3D]            = &bi_maxwellians_3d,
};
distribution_function_3d distribution_funs_3d[NB_TEST_CASE_3D] = {
    [LANDAU_1D_PROJ3D]               = &distrib_One_mode_proj3d,
    [LANDAU_2D_PROJ3D]               = &distrib_Prod_of_two_modes_proj3d,
    [LANDAU_3D_SUM_OF_COS]           = &distrib_Landau_3d_sum_of_cos,
    [LANDAU_3D_PROD_OF_COS]          = &distrib_Landau_3d_prod_of_cos,
    [LANDAU_3D_PROD_OF_ONE_PLUS_COS] = &distrib_Landau_3d_prod_of_one_plus_cos,
    [UNIFORM_3D]                     = &distrib_Uniform_3d,
    [DRIFT_VELOCITIES_3D]            = &distrib_Uniform_3d,
};
max_distribution_function distribution_maxs_3d[NB_TEST_CASE_3D] = {
    [LANDAU_1D_PROJ3D]               = &max_distrib_One_mode,
    [LANDAU_2D_PROJ3D]               = &max_distrib_One_mode,
    [LANDAU_3D_SUM_OF_COS]           = &max_distrib_Landau_3d_sum_of_cos,
    [LANDAU_3D_PROD_OF_COS]          = &max_distrib_Landau_3d_prod_of_cos,
    [LANDAU_3D_PROD_OF_ONE_PLUS_COS] = &max_distrib_Landau_3d_prod_of_one_plus_cos,
    [UNIFORM_3D]                     = &max_distrib_Uniform,
    [DRIFT_VELOCITIES_3D]            = &max_distrib_Uniform,
};



/*
 * Below are available test distribution functions :
 * 0. ELECTRON_HOLES_2D3V for Electron holes in phase space 2d3v
 *     (cf. L. Muschietti, I. Roth, C. W. Carlson, and R. E. Ergun,
 *     "Transverse Instability of Magnetized Electron Holes", 2000)
 *
 * For the function f(w) = 6 * sqrt(2) / Pi / (8 + w**3)
 * Maple gives :
 * solve(diff(6 * sqrt(2) / Pi / (8 + w**3), w) = 0, w);
 *       0 ; 0
 * evalf(6 * sqrt(2) / Pi / (8 + 0**3));
 * f(0) = 0.3376186184
 *
 * For the function g(w) = sqrt(-w) * (1 + 2 * log(-0.5 * 0.8 / w)) / (Pi * 2**2) + (6 + (sqrt(2) + sqrt(-w)) * (1 - w) * sqrt(-w)) / (Pi * (sqrt(2) + sqrt(-w)) * (4 - 2 * w + w**2))
 * Maple gives :
 * solve(diff(sqrt(-w) * (1 + 2 * log(-0.5 * 0.8 / w)) / (Pi * 2**2) + (6 + (sqrt(2) + sqrt(-w)) * (1 - w) * sqrt(-w)) / (Pi * (sqrt(2) + sqrt(-w)) * (4 - 2 * w + w**2)), w) = 0, w);
 *       -0.03363522784
 * evalf(sqrt(0.03363522784) * (1 + 2 * log(0.5 * 0.8 / 0.03363522784)) / (Pi * 2**2) + (6 + (sqrt(2) + sqrt(0.03363522784)) * (1 + 0.03363522784) * sqrt(0.03363522784)) / (Pi * (sqrt(2) + sqrt(0.03363522784)) * (4 + 2 * 0.03363522784 + 0.03363522784**2)));
 * f(-0.03363522784) = 0.3955312929
 *
 * This specific 2d3v testcase links positions and speeds at initial time.
 * We have thus a specific code for this specific test case, different
 * from the other initializers, namely:
 *     "distribution_function_3d" initializes (x, y, vx) and not (x, y, z).
 *     "speeds_generator_2d" initializes (vy, vz) and not (vx, vy, vz).
 *
 * 2. BI_MAXWELLIAN_2D3V
 *     (cf. Q. M. Lu, B. Lembege, J. B. Tao, and S. Wang,
 *     "Perpendicular electric field in two-dimensional electron phase-holes: A parameter study", 2008)
 *
 * This test case uses usual initializers.
 */
double max_distrib_Electron_holes_x_y_vx(double* params) {
    return 0.3956;
}
double distrib_Electron_holes_x_y_vx(double* params, double x, double y, double vx) {
    double ell       = params[0]; // Middle of the physical domain (parallel to B_0 : in x)
    double delta_par = params[1]; // Half-width of the electron hole in x (parallel to B_0 : in x)
    double psi       = params[2]; // Allows to define the bounce frequency omega_b = sqrt(psi / delta_par**2)
    double epsilon   = params[3]; // Measure of the perturbation
    double ky        = params[4]; // Wave number (transverse to B_0 : in y)
    double w = sqr(vx) - 2. * psi * exp(-0.5 * sqr((x - ell) / delta_par - epsilon * cos(ky * y)));
    if (w > 0.) {
        return 6. * sqrt(2.) / PI / (8. + pow(w, 3));
    } else if (w < (-2. * psi)) {
        fprintf(stderr, "Negative energy, problem !\n");
        exit(EXIT_FAILURE);
    } else {
        return sqrt(-w) * (1. + 2. * log(-0.5 * psi / w)) / (PI * sqr(delta_par)) +
               (6. + (sqrt(2.) + sqrt(-w)) * (1. - w) * sqrt(-w)) /
               (PI * (sqrt(2.) + sqrt(-w)) * (4. - 2. * w + sqr(w)));
    }
}

/*
 * Uses Maxwellian_2d.
 *
 * One Maxwellian is centered on v_drift, the other is centered on 0.
 * f(vx, vy, vz) = 0.5 * exp(-0.5 * (vx - v_drift)**2 / v_th**2) + 0.5 * exp(-0.5 * vx**2 / v_th**2)
 *
 * @param[out] vx.
 * @param[out] vy.
 * @param[out] vz.
 */
void bi_maxwellian_x_3d(double* params, double* vx, double* vy, double* vz) {
//    double thermal_speed = params[0];
    double v_drift_x     = params[1];
    double throw_away;
    maxwellian_2d(params, vx, &throw_away);
    if (pic_vert_next_random_double() > 0.5)
        *vx += v_drift_x;
    maxwellian_2d(params, vy, vz);
}

char distribution_names_2d3v[NB_TEST_CASE_2D3V][42] = {
    [ELECTRON_HOLES_2D3V]        = "ELECTRON_HOLES_2D3V",
    [INITIALIZE_VX_WITH_X_AND_Y] = "THIS_IS_NOT_A_TESTCASE",
    [BI_MAXWELLIAN_2D3V]         = "BI_MAXWELLIAN_2D3V",
    [LANDAU_1D_PROJ2D3V]         = "LANDAU_1D_PROJ2D3V",
};
speeds_generator_2d speed_generators_2d3v_vyvz[NB_TEST_CASE_2D3V] = {
    [ELECTRON_HOLES_2D3V]        = &maxwellian_2d,
    [INITIALIZE_VX_WITH_X_AND_Y] = (void*)0,
    [BI_MAXWELLIAN_2D3V]         = (void*)0,
    [LANDAU_1D_PROJ2D3V]         = (void*)0,
};
distribution_function_3d distribution_funs_2d3v_xyvx[NB_TEST_CASE_2D3V] = {
    [ELECTRON_HOLES_2D3V]        = &distrib_Electron_holes_x_y_vx,
    [INITIALIZE_VX_WITH_X_AND_Y] = (void*)0,
    [BI_MAXWELLIAN_2D3V]         = (void*)0,
    [LANDAU_1D_PROJ2D3V]         = (void*)0,
};
speeds_generator_3d speed_generators_2d3v_vxvyvz[NB_TEST_CASE_2D3V] = {
    [ELECTRON_HOLES_2D3V]        = (void*)0,
    [INITIALIZE_VX_WITH_X_AND_Y] = (void*)0,
    [BI_MAXWELLIAN_2D3V]         = &bi_maxwellian_x_3d,
    [LANDAU_1D_PROJ2D3V]         = &maxwellian_3d,
};
distribution_function_2d distribution_funs_2d3v_xy[NB_TEST_CASE_2D3V] = {
    [ELECTRON_HOLES_2D3V]        = (void*)0,
    [INITIALIZE_VX_WITH_X_AND_Y] = (void*)0,
    [BI_MAXWELLIAN_2D3V]         = &distrib_Uniform_2d,
    [LANDAU_1D_PROJ2D3V]         = &distrib_One_mode_proj2d,
};
max_distribution_function distribution_maxs_2d3v[NB_TEST_CASE_2D3V] = {
    [ELECTRON_HOLES_2D3V]        = &max_distrib_Electron_holes_x_y_vx,
    [INITIALIZE_VX_WITH_X_AND_Y] = (void*)0,
    [BI_MAXWELLIAN_2D3V]         = &max_distrib_Uniform,
    [LANDAU_1D_PROJ2D3V]         = &max_distrib_One_mode,
};

