#ifndef PIC_VERT_INITIAL_DISTRIBUTIONS
#define PIC_VERT_INITIAL_DISTRIBUTIONS

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

typedef double (*max_distribution_function)(double*);



typedef void (*speeds_generator_1d)(double*, double*);
typedef double (*distribution_function_1d)(double*, double);

/*
 * Below are available test distribution functions :
 * 0. PARAXIAL_1D for Paraxial 1d
 *    f_0(x, v) = 1/(sqrt(2 * pi) * v_{th}) * exp(−(vx^2) / (2 * v_{th}^2))
 */
enum TEST_CASES_1D {
    PARAXIAL_1D,
    NB_TEST_CASE_1D // Always has to be last if you update this enum !
};

extern char distribution_names_1d[NB_TEST_CASE_1D][42];
extern speeds_generator_1d speed_generators_1d[NB_TEST_CASE_1D];
extern distribution_function_1d distribution_funs_1d[NB_TEST_CASE_1D];
extern max_distribution_function distribution_maxs_1d[NB_TEST_CASE_1D];



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
enum TEST_CASES_2D {
    LANDAU_1D_PROJ2D,
    LANDAU_2D,
    KELVIN_HELMHOLTZ,
    TWO_STREAM_BERNIER,
    TWO_BEAMS_FIJALKOW,
    TWO_STREAM_1D_PROJ2D,
    TWO_STREAM_2D,
    NB_TEST_CASE_2D // Always has to be last if you update this enum !
};

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
enum TEST_CASES_3D {
    LANDAU_1D_PROJ3D,
    LANDAU_2D_PROJ3D,
    LANDAU_3D_SUM_OF_COS,
    LANDAU_3D_PROD_OF_COS,
    LANDAU_3D_PROD_OF_ONE_PLUS_COS,
    UNIFORM_3D,
    DRIFT_VELOCITIES_3D,
    NB_TEST_CASE_3D // Always has to be last if you update this enum !
};

extern char distribution_names_3d[NB_TEST_CASE_3D][42];
extern speeds_generator_3d speed_generators_3d[NB_TEST_CASE_3D];
extern distribution_function_3d distribution_funs_3d[NB_TEST_CASE_3D];
extern max_distribution_function distribution_maxs_3d[NB_TEST_CASE_3D];



/*
 * Below are available test distribution functions :
 * 0. ELECTRON_HOLES_2D3V for Electron holes in phase space 2d3v
 *     (cf. L. Muschietti, I. Roth, C. W. Carlson, and R. E. Ergun,
 *     "Transverse Instability of Magnetized Electron Holes", 2000)
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
enum TEST_CASES_2D3V {
    ELECTRON_HOLES_2D3V,
    INITIALIZE_VX_WITH_X_AND_Y, // Put all the test cases that update vx together with x and y before ; put usual test cases after.
    BI_MAXWELLIAN_2D3V,
    LANDAU_1D_PROJ2D3V,
    NB_TEST_CASE_2D3V           // Always has to be last if you update this enum !
};

char distribution_names_2d3v[NB_TEST_CASE_2D3V][42];
speeds_generator_2d speed_generators_2d3v_vyvz[NB_TEST_CASE_2D3V];
distribution_function_3d distribution_funs_2d3v_xyvx[NB_TEST_CASE_2D3V];
speeds_generator_3d speed_generators_2d3v_vxvyvz[NB_TEST_CASE_2D3V];
distribution_function_2d distribution_funs_2d3v_xy[NB_TEST_CASE_2D3V];
max_distribution_function distribution_maxs_2d3v[NB_TEST_CASE_2D3V];

#endif // ifndef PIC_VERT_INITIAL_DISTRIBUTIONS


