#include <stdio.h>          // functions printf, fprintf (output strings on a stream)
                            // constant  stderr (standard error output stream)
#include <stdlib.h>         // function  exit (error handling)
                            // constant  EXIT_FAILURE (error handling)
#include <math.h>           // functions cos, sin, pow, fabs
#include <mpi.h>            // constants MPI_COMM_WORLD
                            // functions MPI_Comm_rank
#include "diagnostics.h"    // types     damping_values, hashmap
#include "math_functions.h" // function  sqr
#include "meshes.h"         // types     cartesian_mesh_1d, cartesian_mesh_2d, cartesian_mesh_3d
#include "parameters.h"     // constants PI, EPSILON, DBL_DECIMAL_DIG

/*****************************************************************************
 *                        Theoretical Damping Values                         *
 *****************************************************************************/

hashmap* hashtable[MAP_SIZE] = { (void*)0 };

/*
 * Form hash value in [|0 ; MAP_SIZE|[ for a given kmode.
 *
 * @param  kmode
 * @return the hash value of kmode
 */
unsigned int hash(double kmode) {
    return ((int)(kmode * 100)) % MAP_SIZE;
}

/*
 * Look for a given kmode in the hashmap
 *
 * @param  kmode
 * @return the damping values when found, null if not found.
 */
damping_values* lookup(double kmode) {
    hashmap* map_p;
    for (map_p = hashtable[hash(kmode)]; map_p; map_p = map_p->next)
        if (fabs(kmode - map_p->kmode) < EPSILON)
            return &(map_p->values);
    return (void*)0;
}

/*
 * If the kmode is already in the hashtable, do nothing.
 * Else, add it at its hash position, first in the list.
 *
 * @param kmode
 * @param values
 */
void add_pair(double kmode, damping_values values) {
    if (!lookup(kmode)) {
        hashmap* map = malloc(sizeof(hashmap));
        unsigned int hashval = hash(kmode);
        map->next = hashtable[hashval];
        map->kmode = kmode;
        map->values = values;
        hashtable[hashval] = map;
    }
}

/*
 * Put the theoretical values inside the hashtable.
 *
 * er, psi values for 0.2, 0.3, 0.4 & 0.5 from
 *   Sonnendrücker, "Numerical Methods for the Vlasov-Maxwell equations", 2016
 *   4.4.2. Landau damping, p. 54
 *
 * omega_real, omega_imag for 2 * PI / 22 and sqrt(2) * 0.5 from Zeal / Maple.
 *
 * omega_real, omega_imag for 0.25 to 2.0 by 0.05 & 2.3088 from
 *   Canosa, "Numerical Solution of Landau's Dispersion Equation", 1973
 *   p. 159
 */
void init_damping_table() {
/*
    add_pair(, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = ,
        .omega_imag =  });
*/
    add_pair(0.2, (damping_values){
        .er         = 1.129664,
        .psi        = 0.00127377,
        .omega_real = 1.0640,
        .omega_imag = -0.00005510 });
    add_pair(0.25, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 1.1056,
        .omega_imag = -0.0021693 });
    add_pair(0.28559933214452665, (damping_values){ // 2 * PI / 22
        .er         = 25., // experimental
        .psi        = 0.1, // experimental
        .omega_real = 1.1432989086184062498,
        .omega_imag = -0.0084664151303061653770 });
    add_pair(0.3, (damping_values){
        .er         = 0.63678,
        .psi        = 0.114267,
        .omega_real = 1.1598,
        .omega_imag = -0.012623 });
    add_pair(0.35, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 1.2209,
        .omega_imag = -0.034324 });
    add_pair(0.4, (damping_values){
        .er         = 0.424666,
        .psi        = 0.3357725,
        .omega_real = 1.2850,
        .omega_imag = -0.066133 });
    add_pair(0.45, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 1.3502,
        .omega_imag = -0.10629 });
    add_pair(0.5, (damping_values){
        .er         = 0.3677,
        .psi        = 0.536245,
        .omega_real = 1.4156,
        .omega_imag = -0.15336 });
    add_pair(0.55, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 1.4809,
        .omega_imag = -0.20624 });
    add_pair(0.6, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 1.5457,
        .omega_imag = -0.26411 });
    add_pair(0.65, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 1.6100,
        .omega_imag = -0.32633 });
    add_pair(0.7, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 1.6739,
        .omega_imag = -0.39240 });
    add_pair(0.707106781187, (damping_values){ // sqrt(2) * 0.5
        .er         = 0.72, // experimental
        .psi        = 0.93, // experimental
        .omega_real = 1.68289327433,
        .omega_imag = -0.402080551005 });
    add_pair(0.75, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 1.7371,
        .omega_imag = -0.46192 });
    add_pair(0.8, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 1.7999,
        .omega_imag = -0.53455 });
    add_pair(0.85, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 1.8621,
        .omega_imag = -0.61003 });
    add_pair(0.866025403784, (damping_values){ // sqrt(3) * 0.5
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 1.8819720651567779579,
        .omega_imag = -0.63477897100726209258 });
    add_pair(0.9, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 1.9239,
        .omega_imag = -0.68811 });
    add_pair(0.95, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 1.9851,
        .omega_imag = -0.76860 });
    add_pair(1.0, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 2.0459,
        .omega_imag = -0.85134 });
    add_pair(1.05, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 2.1062,
        .omega_imag = -0.93615 });
    add_pair(1.1, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 2.1662,
        .omega_imag = -1.0229 });
    add_pair(1.15, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 2.2257,
        .omega_imag = -1.1115 });
    add_pair(1.2, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 2.2848,
        .omega_imag = -1.2019 });
    add_pair(1.25, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 2.3436,
        .omega_imag = -1.2939 });
    add_pair(1.3, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 2.4020,
        .omega_imag = -1.3874 });
    add_pair(1.35, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 2.4600,
        .omega_imag = -1.4824 });
    add_pair(1.4, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 2.5178,
        .omega_imag = -1.5789 });
    add_pair(1.45, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 2.5752,
        .omega_imag = -1.6766 });
    add_pair(1.5, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 2.6323,
        .omega_imag = -1.7757 });
    add_pair(1.55, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 2.6892,
        .omega_imag = -1.8760 });
    add_pair(1.6, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 2.7457,
        .omega_imag = -1.9775 });
    add_pair(1.65, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 2.8020,
        .omega_imag = -2.0801 });
    add_pair(1.7, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 2.8580,
        .omega_imag = -2.1839 });
    add_pair(1.75, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 2.9138,
        .omega_imag = -2.2886 });
    add_pair(1.8, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 2.9693,
        .omega_imag = -2.3944 });
    add_pair(1.85, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 3.0246,
        .omega_imag = -2.5012 });
    add_pair(1.9, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 3.0797,
        .omega_imag = -2.6090 });
    add_pair(1.95, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 3.1345,
        .omega_imag = -2.7176 });
    add_pair(2.0, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 3.1891,
        .omega_imag = -2.8272 });
    add_pair(2.3088, (damping_values){
        .er         = 0.,
        .psi        = 0.,
        .omega_real = 3.5222,
        .omega_imag = -3.5522 });
}

/*
 * Look for a given kmode in the hashmap.
 * If not found, applies a formula.
 *
 * @param  kmode
 * @param  compare_values set to non-0 if you want to print the comparison of formulae. Defaults to 0.
 * @return the damping values.
 */
damping_values* get_damping_values(double kmode, int compare_values) {
    int mpi_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    init_damping_table();
    damping_values* landau_values = lookup(kmode);
    if (mpi_rank == 0 && compare_values != 0) {
        printf("#Comparison of formulae.\n");
        if (landau_values) {
            printf("#kmode = %.*g (Numerical zero search)\n", DBL_DECIMAL_DIG, kmode);
            printf("#  omega_real = %.*g\n", DBL_DECIMAL_DIG, landau_values->omega_real);
            printf("#  omega_imag = %.*g\n", DBL_DECIMAL_DIG, landau_values->omega_imag);
        }
        // Textbook formulae
        printf("#kmode = %.*g (Textbook formulae for kmode >= 0.3)\n", DBL_DECIMAL_DIG, kmode);
        printf("#  omega_real = %.*g\n", DBL_DECIMAL_DIG, 1. + 3. / 2. * sqr(kmode));
        printf("#  omega_imag = %.*g\n", DBL_DECIMAL_DIG, -0.5 * sqrt(PI / 2) * 1. / pow(kmode, 3) * exp(-1. / (2. * sqr(kmode)) - 3. / 2.));
        // McKinstrie, Giacone & Startsev, "Accurate formulas for the Landau damping rates of electrostatic waves", 1999
        // p. 464, formulae 22-24 (more accurate when kmode < 0.3)
        printf("#kmode = %.*g (McKinstrie-Giacone-Startsev formulae for kmode < 0.3)\n", DBL_DECIMAL_DIG, kmode);
        printf("#  omega_real = %.*g\n", DBL_DECIMAL_DIG, 1. + 3. / 2. * sqr(kmode) + 15. / 8. * pow(kmode, 4) + 147. / 16. * pow(kmode, 6));
        printf("#  omega_imag = %.*g\n", DBL_DECIMAL_DIG, -0.5 * sqrt(PI / 2) * (1. / pow(kmode, 3) - 6. * kmode) * exp(-1. / (2. * sqr(kmode)) - 3. / 2. - 3. * sqr(kmode) - 12. * pow(kmode, 4)));
        // Shalaby, Broderick, Chang, Pfrommer, Lamberts & Puchwein, "SHARP: A spatially higher-order, relativistic Particle-in-Cell code", 2017
        // p. 9, formula 53 (even more accurate when kmode < 0.6)
        printf("#kmode = %.*g (Shalaby-Broderick-Chang-Pfrommer-Lamberts-Puchwein formulae for kmode < 0.6)\n", DBL_DECIMAL_DIG, kmode);
        printf("#  omega_real = %.*g\n", DBL_DECIMAL_DIG, 1. + 3. / 2. * sqr(kmode) + 15. / 8. * pow(kmode, 4) + 147. / 16. * pow(kmode, 6) + 736.437 * pow(kmode, 8) - 14729.3 * pow(kmode, 10) + 105429 * pow(kmode, 12) - 370151 * pow(kmode, 14) + 645538 * pow(kmode, 16) - 448190 * pow(kmode, 18));
        printf("#  omega_imag = %.*g\n", DBL_DECIMAL_DIG, -0.5 * sqrt(PI / 2) * (1. / pow(kmode, 3) - 6. * kmode - 40.7173 * pow(kmode, 3) + 3900.23 * pow(kmode, 5) - 2462.25 * pow(kmode, 7) - 274.99 * pow(kmode, 9)) * exp(-1. / (2. * sqr(kmode)) - 3. / 2. - 3. * sqr(kmode) - 12. * pow(kmode, 4) - 575.516 * pow(kmode, 6) + 3790.16 * pow(kmode, 8) - 8827.54 * pow(kmode, 10) + 7266.87 * pow(kmode, 12)));
    }
    if (!landau_values) {
        landau_values = malloc(sizeof(damping_values));
        landau_values->er  = 0.;
        landau_values->psi = 0.;
        if (kmode <= 0.6) {
            landau_values->omega_real = 1. + 3. / 2. * sqr(kmode) + 15. / 8. * pow(kmode, 4) + 147. / 16. * pow(kmode, 6) + 736.437 * pow(kmode, 8) - 14729.3 * pow(kmode, 10) + 105429 * pow(kmode, 12) - 370151 * pow(kmode, 14) + 645538 * pow(kmode, 16) - 448190 * pow(kmode, 18);
            landau_values->omega_imag = -0.5 * sqrt(PI / 2) * (1. / pow(kmode, 3) - 6. * kmode - 40.7173 * pow(kmode, 3) + 3900.23 * pow(kmode, 5) - 2462.25 * pow(kmode, 7) - 274.99 * pow(kmode, 9)) * exp(-1. / (2. * sqr(kmode)) - 3. / 2. - 3. * sqr(kmode) - 12. * pow(kmode, 4) - 575.516 * pow(kmode, 6) + 3790.16 * pow(kmode, 8) - 8827.54 * pow(kmode, 10) + 7266.87 * pow(kmode, 12));
        } else {
            landau_values->omega_real = 0.;
            landau_values->omega_imag = 0.;
        }
    }
    return landau_values;
}



/*****************************************************************************
 *                       Integral of squared fields                          *
 *       (the L2-Norm of the field is the square root of this integral)      *
 *****************************************************************************/

/*
 * Integral of a squared field in 1d. Uses Kahan Summation Formula, cf.
 * Kahan, "Further remarks on reducing truncation errors", 1965
 * https://en.wikipedia.org/wiki/Kahan_summation_algorithm
 *
 * @param[in] mesh, the mesh on which we're working.
 * @param[in] e[ncx+1] the electric field on any axis.
 * @return    the electric energy of e.
 */
double integral_of_squared_field_1d(cartesian_mesh_1d mesh1d, double* e) {
    int i;
    double sum, compensation, sum2, term_plus_compensation;
    
    sum = 0.;
    compensation = 0.;
    for (i = 0; i < mesh1d.num_cell_x; i++) {
        term_plus_compensation = sqr(e[i]) - compensation;
        sum2 = sum + term_plus_compensation;
        compensation = (sum2 - sum) - term_plus_compensation;
        sum = sum2;
    }
    return sum * mesh1d.delta_x;
}

/*
 * Integral of a squared field in 2d. Uses Kahan Summation Formula, cf.
 * Kahan, "Further remarks on reducing truncation errors", 1965
 * https://en.wikipedia.org/wiki/Kahan_summation_algorithm
 *
 * @param[in] mesh, the mesh on which we're working.
 * @param[in] e[ncx+1][ncy+1] the electric field on any axis.
 * @return    the electric energy of e.
 */
double integral_of_squared_field_2d(cartesian_mesh_2d mesh2d, double** e) {
    int i, j;
    double sum, compensation, sum2, term_plus_compensation;
    
    sum = 0.;
    compensation = 0.;
    for (i = 0; i < mesh2d.num_cell_x; i++) {
        for (j = 0; j < mesh2d.num_cell_y; j++) {
            term_plus_compensation = sqr(e[i][j]) - compensation;
            sum2 = sum + term_plus_compensation;
            compensation = (sum2 - sum) - term_plus_compensation;
            sum = sum2;
        }
    }
    return sum * mesh2d.delta_x * mesh2d.delta_y;
}

/*
 * Integral of a squared field in 3d. Uses Kahan Summation Formula, cf.
 * Kahan, "Further remarks on reducing truncation errors", 1965
 * https://en.wikipedia.org/wiki/Kahan_summation_algorithm
 *
 * @param[in] mesh, the mesh on which we're working.
 * @param[in] e[ncx+1][ncy+1][ncz+1] the electric field on any axis.
 * @return    the electric energy of e.
 */
double integral_of_squared_field_3d(cartesian_mesh_3d mesh3d, double*** e) {
    int i, j, k;
    double sum, compensation, sum2, term_plus_compensation;
    
    sum = 0.;
    compensation = 0.;
    for (i = 0; i < mesh3d.num_cell_x; i++) {
        for (j = 0; j < mesh3d.num_cell_y; j++) {
            for (k = 0; k < mesh3d.num_cell_z; k++) {
                term_plus_compensation = sqr(e[i][j][k]) - compensation;
                sum2 = sum + term_plus_compensation;
                compensation = (sum2 - sum) - term_plus_compensation;
                sum = sum2;
            }
        }
    }
    return sum * mesh3d.delta_x * mesh3d.delta_y * mesh3d.delta_z;
}



/*****************************************************************************
 *                        Fourier modes computation                          *
 *****************************************************************************/

/*
 * Computes a Fourier mode of the electric field, in 1d.
 * Computes A_n = sqrt(a_n**2 + b_n**2), where n = mode_x.
 *
 * @param[in] mesh2d the spatial mesh.
 * @param[in] mode_x the mode to be computed.
 * @param[in] ex the electric field on the x-axis.
 * @return    the Fourier mode of the electric field
 */
double fourier_mode_1d(cartesian_mesh_2d mesh2d, int mode_x, double** ex) {
/*
 * The whole function is equivalent to the following code :
 *    double** ey = allocate_matrix(mesh2d.num_cell_x, mesh2d.num_cell_y);
 *    for (int i = 0; i < mesh2d.num_cells1; i++) 
 *        for (int j = 0; j < mesh2d.num_cells2; j++)
 *            ey[i][j] = 0.;
 *    double tmp = fourier_mode_2d(mesh2d, mode_x, 0, ex, ey, { 1., 0. });
 *    deallocate_matrix(ey, mesh2d.num_cells1, mesh2d.num_cells2);
 *    return tmp;
 */
    int i, j;
    double term1, term2;
    int ncx = mesh2d.num_cell_x;
    int ncy = mesh2d.num_cell_y;
    double dx = mesh2d.delta_x;
    double dy = mesh2d.delta_y;
    
    term1 = 0.;
    for (i = 0; i < ncx; i++)
        for (j = 0; j < ncy; j++)
            term1 += ex[i][j] * cos((double)(mode_x * i) * 2. * PI / ((double) ncx));
    term1 = sqr(term1);
    term2 = 0.;
    for (i = 0; i < ncx; i++)
        for (j = 0; j < ncy; j++)
            term2 += ex[i][j] * sin((double)(mode_x * i) * 2. * PI / ((double) ncx));
    term2 = sqr(term2);
    return sqrt(2. * dx * dy * (term1 + term2) / ((double)(ncx * ncy)));
}

/*
 * Computes the energy of the Fourier mode of the electric field, in 1d.
 * Computes A_n**2, where n = mode_x.
 * N.B.: Thanks to Parseval, sum_n(A_n**2) = int_x(E_x**2 dx).
 *
 * @param[in] mesh2d the spatial mesh.
 * @param[in] mode_x the mode to be computed.
 * @param[in] ex the electric field on the x-axis.
 * @return    the energy of the Fourier mode of the electric fields.
 */
double energy_fourier_mode_1d(cartesian_mesh_2d mesh2d, int mode_x, double** ex) {
    return sqr(fourier_mode_1d(mesh2d, mode_x, ex));
}

/*
 * Computes a Fourier mode of the electric fields, in 2d.
 * Computes A_n = sqrt(a_n**2 + b_n**2), where n = mode_x.
 * 
 * @param[in] mesh2d the spatial mesh.
 * @param[in] mode_x the mode to be computed.
 * @param[in] mode_y the mode to be computed.
 * @param[in] ex the electric field on the x-axis.
 * @param[in] ey the electric field on the y-axis.
 * @param[in] d the direction in which to compute the Fourier mode.
 * @return    the Fourier mode of the electric fields.
 */
double fourier_mode_2d(cartesian_mesh_2d mesh2d, int mode_x, int mode_y, double** ex, double** ey, double d[2]) {
    int i, j;
    
    double sqr_norm_d = sqr(d[0]) + sqr(d[1]);
    if (sqr_norm_d < EPSILON) {
        fprintf(stderr, "d is not a proper direction.\n");
        exit(EXIT_FAILURE);
    }
    
    int ncx = mesh2d.num_cell_x;
    int ncy = mesh2d.num_cell_y;
    double dx = mesh2d.delta_x;
    double dy = mesh2d.delta_y;
    
    double term1 = 0.;
    for (i = 0; i < ncx; i++)
        for (j = 0; j < ncy; j++)
            term1 = term1 + (d[0] * ex[i][j] + d[1] * ey[i][j]) *
                cos((double)(mode_x * i) * 2. * PI / ((double)ncx) +
                    (double)(mode_y * j) * 2. * PI / ((double)ncy));
    term1 = sqr(term1);
    double term2 = 0.;
    for (i = 0; i < ncx; i++)
        for (j = 0; j < ncy; j++)
            term2 = term2 + (d[0] * ex[i][j] + d[1] * ey[i][j]) *
                sin((double)(mode_x * i) * 2. * PI / ((double)ncx) +
                    (double)(mode_y * j) * 2. * PI / ((double)ncy));
    term2 = sqr(term2);
    return sqrt(2. * dx * dy * (term1 + term2) / (sqr_norm_d * (double)(ncx * ncy)));
}

/*
 * Computes the energy of the Fourier mode of the electric fields, in 2d.
 * 
 * @param[in] mesh2d the spatial mesh.
 * @param[in] mode_x the mode to be computed.
 * @param[in] mode_y the mode to be computed.
 * @param[in] ex the electric field on the x-axis.
 * @param[in] ey the electric field on the y-axis.
 * @return    the energy of the Fourier mode of the electric fields.
 */
double energy_fourier_mode_2d(cartesian_mesh_2d mesh2d, int mode_x, int mode_y, double** ex, double** ey) {
    double d_10[2] = { 1., 0. };
    double d_01[2] = { 0., 1. };
    return sqr(fourier_mode_2d(mesh2d, mode_x, mode_y, ex, ey, d_10)) +
           sqr(fourier_mode_2d(mesh2d, mode_x, mode_y, ex, ey, d_01));
}

