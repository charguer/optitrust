#ifndef PIC_VERT_POISSON_SOLVERS
#define PIC_VERT_POISSON_SOLVERS

#include <fftw3.h>  // types fftw_plan, fftw_complex
#include "meshes.h" // types cartesian_mesh_1d, cartesian_mesh_2d, cartesian_mesh_3d

/*****************************************************************************
 *                            Poisson solver 1d                              *
 *            Free Boundary Conditions, Trapezoidal Integration              *
 *                                                                           *
 * Code translated from Fortran from Sever's code, in the following file :   *
 * poisson_solver.f90                                                        *
 *****************************************************************************/

/*
 * Solve laplacian(phi(x)) = -rho(x) with free boundary conditions,
 * then set E(x) = -grad(phi(x)) which means
 *     Ex(x) = -partial_x phi(x)
 *
 * The algorithm uses the fact that the resulting electric field will be odd.
 * 
 * @param[in]  mesh, the mesh on which we're working.
 * @param[in]  rho[ncx+1] the charge density.
 * @param[out] e_x[ncx+1] the electric field on the x-axis.
 */
void compute_E_from_rho_1d_trapezoidal(cartesian_mesh_1d mesh, double* rho, double* e_x);



/*****************************************************************************
 *                            Poisson solver 1d                              *
 *                 Periodic Boundary Conditions, FFT Algorithm               *
 *                                                                           *
 * Code translated from Fortran from Selalib, in the following files :       *
 * selalib/src/field_solvers/poisson_solvers/sll_m_poisson_1d_periodic.F90   *
 *****************************************************************************/

typedef struct poisson_1d_solver {
    int ncx;               // cells number in x
    double* kx;            // wave number in x
    double* k2;            // \f[ kx^2 \f]
    fftw_complex* tmp_rho; // rho is given as (ncx+1) double* array
                           // tmp_rho takes the ncx part, as a fftw_complex* array
    fftw_complex* tmp_e_x; // e_x is given as (ncx+1) double* array
                           // tmp_e_x takes the ncx part, as a fftw_complex* array
    fftw_complex* rho_hat; // fft(rho)
    fftw_complex* e_x_hat; // fft(e_x)
    fftw_plan fw;          //  forward fftw plan : rho_hat =  fft(rho)
    fftw_plan bwx;         // backward fftw plan : tmp_e_x = ifft(e_x_hat)
} poisson_1d_solver;

/*
 * @param[in] mesh, the mesh on which we're working.
 * @return    a poisson solver that uses the library FFTW.
 */
poisson_1d_solver new_poisson_1d_fft_solver(cartesian_mesh_1d mesh);

/*
 * Solve laplacian(phi(x)) = -rho(x) with periodic boundary conditions,
 * then set E(x) = -grad(phi(x)) which means
 *     Ex(x) = -partial_x phi(x)
 *
 * @param[in]  p the poisson solver (has to be initialized before the call).
 * @param[in]  rho[ncx+1] the charge density.
 * @param[out] e_x[ncx+1] the electric field on the x-axis.
 */
void compute_E_from_rho_1d_fft(poisson_1d_solver p, double* rho, double* e_x);

void free_poisson_1d(poisson_1d_solver* p);



/*****************************************************************************
 *                            Poisson solver 2d                              *
 *                 Periodic Boundary Conditions, FFT Algorithm               *
 *                                                                           *
 * Code translated from Fortran from Selalib, in the following files :       *
 * selalib/src/field_solvers/poisson_solvers/sll_m_poisson_2d_fft.F90        *
 * selalib/src/field_solvers/poisson_solvers/                                *
 *     sll_m_poisson_2d_periodic_fftw.F90                                    *
 *****************************************************************************/

typedef struct poisson_2d_solver {
    int ncx;           // cells number in x
    int ncy;           // cells number in y
    double** kx;       // wave number in x
    double** ky;       // wave number in y
    double** k2;       // \f[ kx^2 + ky^2 \f]
    double* tmp_rho;   // rho is given as (ncx+1)*(ncy+1) double** array
                       // tmp_rho takes the ncx*ncy part, as a double* array
    double* tmp_e_x;   // e_x is given as (ncx+1)*(ncy+1) double** array
                       // tmp_e_x takes the ncx*ncy part, as a double* array
    double* tmp_e_y;   // e_y is given as (ncx+1)*(ncy+1) double** array
                       // tmp_e_y takes the ncx*ncy part, as a double* array
    fftw_complex* rht; // fft(rho)
    fftw_complex* ext; // fft(ex)
    fftw_complex* eyt; // fft(ey)
    fftw_plan fw;      //  forward fftw plan : rht = fft(rho)
    fftw_plan bwx;     // backward fftw plan : tmp_e_x = ifft(ext)
    fftw_plan bwy;     // backward fftw plan : tmp_e_y = ifft(eyt)
} poisson_2d_solver;

/*
 * @param[in] mesh, the mesh on which we're working.
 * @return    a poisson solver that uses the library FFTW.
 */
poisson_2d_solver new_poisson_2d_fft_solver(cartesian_mesh_2d mesh);

/*
 * Solve laplacian(phi(x, y)) = -rho(x, y) with periodic boundary conditions.
 * 
 * @param[in]  p the poisson solver (has to be initialized before the call).
 * @param[in]  rho[ncx+1][ncy+1] the charge density.
 * @param[out] phi[ncx+1][ncy+1] the electric potential.
 */
void compute_phi_from_rho_2d_fft(poisson_2d_solver p, double** rho, double** phi);

/*
 * Solve laplacian(phi(x, y)) = -rho(x, y) with periodic boundary conditions,
 * then set E(x, y) = -grad(phi(x, y)) which means
 *     Ex(x, y) = -partial_x phi(x, y)
 *     Ey(x, y) = -partial_y phi(x, y)
 * 
 * @param[in]  p the poisson solver (has to be initialized before the call).
 * @param[in]  rho[ncx+1][ncy+1] the charge density.
 * @param[out] e_x[ncx+1][ncy+1] the electric field on the x-axis.
 * @param[out] e_y[ncx+1][ncy+1] the electric field on the y-axis.
 */
void compute_E_from_rho_2d_fft(poisson_2d_solver p, double** rho, double** e_x, double** e_y);

void free_poisson_2d(poisson_2d_solver* p);



/*****************************************************************************
 *                            Poisson solver 3d                              *
 *                 Periodic Boundary Conditions, FFT Algorithm               *
 *                                                                           *
 * Code translated from C++ from Ksander's code, in the following file :     *
 * ksander_bsl_6d.cpp                                                        *
 *****************************************************************************/

typedef struct poisson_3d_solver {
    int ncx;                          // cells number in x
    int ncy;                          // cells number in y
    int ncz;                          // cells number in z
    double* kx;                       // wave number in x
    double* ky;                       // wave number in y
    double* kz;                       // wave number in z
    double*** k2;                     // \f[ kx^2 + ky^2 + kz^2 \f]
    fftw_complex*** hat_rho;          // fft(rho)
    fftw_complex** in;                // temporary arrays for fft plans (one per thread)
    fftw_complex** out;               // temporary arrays for fft plans (one per thread)
    fftw_plan* px;                    //  forward fft in x plans (one per thread)
    fftw_plan* py;                    //  forward fft in y plans (one per thread)
    fftw_plan* pz;                    //  forward fft in z plans (one per thread)
    fftw_plan* px_inv;                // backward fft in z plans (one per thread)
    fftw_plan* py_inv;                // backward fft in z plans (one per thread)
    fftw_plan* pz_inv;                // backward fft in z plans (one per thread)
} poisson_3d_solver;

/*
 * @param[in] mesh, the mesh on which we're working.
 * @return    a poisson solver that uses the library FFTW.
 */
poisson_3d_solver new_poisson_3d_fft_solver(cartesian_mesh_3d mesh);

/*
 * Solve laplacian(phi(x, y, z)) = -rho(x, y, z) with periodic boundary conditions,
 * then set E(x, y, z) = -grad(phi(x, y, z)) which means
 *     Ex(x, y, z) = -partial_x phi(x, y, z)
 *     Ey(x, y, z) = -partial_y phi(x, y, z)
 *     Ez(x, y, z) = -partial_z phi(x, y, z)
 * 
 * @param[in]  p the poisson solver (has to be initialized before the call).
 * @param[in]  rho[ncx+1][ncy+1][ncz+1] the charge density.
 * @param[out] e_x[ncx+1][ncy+1][ncz+1] the electric field on the x-axis.
 * @param[out] e_y[ncx+1][ncy+1][ncz+1] the electric field on the y-axis.
 * @param[out] e_z[ncx+1][ncy+1][ncz+1] the electric field on the y-axis.
 */
void compute_E_from_rho_3d_fft(poisson_3d_solver p, double*** rho, double*** e_x, double*** e_y, double*** e_z);

void free_poisson_3d(poisson_3d_solver* p);

#endif // ifndef PIC_VERT_POISSON_SOLVERS

