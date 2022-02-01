#include <stdio.h>            // function  fprintf (output strings on a stream)
                              // constant  stderr (standard error output stream)
#include <stdlib.h>           // functions malloc, free ((de)allocate memory)
                              //           exit (error handling)
                              // constant  EXIT_FAILURE (error handling)
                              // type      size_t
#include <complex.h>          // type      double complex
                              // constant  I (I^2 = -1)
#include <fftw3.h>            // functions fftw_plan_dft_r2c_2d, fftw_execute_dft_c2r, fftw_execute_dft_r2c,
                              //           fftw_plan_dft_1d, fftw_execute, fftw_destroy_plan,
                              //           fftw_malloc, fftw_alloc_complex, fftw_free
                              // constant  FFTW_FORWARD, FFTW_BACKWARD, FFTW_ESTIMATE, FFTW_PATIENT
                              // types     fftw_plan, fftw_complex
#include <omp.h>              // function  omp_get_thread_num
#include "parameters.h"       // constant  PI
#include "meshes.h"           // types     cartesian_mesh_1d, cartesian_mesh_2d, cartesian_mesh_3d
#include "math_functions.h"   // functions max, sqr
#include "matrix_functions.h" // functions allocate_matrix, allocateMatrix, deallocate_matrix, deallocateMatrix
                              //           allocate_3d_array, deallocate_3d_array, allocate_array, deallocate_array
#include "poisson_solvers.h"  // types     poisson_1d_solver, poisson_2d_solver, poisson_3d_solver

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
void compute_E_from_rho_1d_trapezoidal(cartesian_mesh_1d mesh, double* rho, double* e_x) {
    int j, ncx;
    double dx, x_min;

    ncx = mesh.num_cell_x;
    dx = mesh.delta_x;
    x_min = mesh.x_min;
    e_x[ncx / 2] = 0.;
    for (j = ncx / 2 + 1; j <= ncx; j++) {
       e_x[j] = (0.5 * dx * rho[j - 1] + e_x[j - 1]) * (x_min + (double)(j-1) * dx) / (x_min + (double)j * dx) + 0.5 * dx * rho[j];
       e_x[ncx - j] = -e_x[j];
    }
}



/*****************************************************************************
 *                            Poisson solver 1d                              *
 *                 Periodic Boundary Conditions, FFT Algorithm               *
 *                                                                           *
 * Code translated from Fortran from Selalib, in the following files :       *
 * selalib/src/field_solvers/poisson_solvers/sll_m_poisson_1d_periodic.F90   *
 *****************************************************************************/

/*
 * @param[in] mesh, the mesh on which we're working.
 * @return    a poisson solver that uses the library FFTW.
 */
poisson_1d_solver new_poisson_1d_fft_solver(cartesian_mesh_1d mesh) {
    // Warning : don't put i as size_t, because with size_t values,
    // i - p.ncx will take really big values instead of negative ones.
    int i;
    poisson_1d_solver p;

    p.ncx = mesh.num_cell_x;
    p.kx = malloc(p.ncx * sizeof(double));
    p.k2 = malloc(p.ncx * sizeof(double));
    p.tmp_rho = fftw_alloc_complex(p.ncx);
    p.tmp_e_x = fftw_alloc_complex(p.ncx);
    p.rho_hat = fftw_alloc_complex(p.ncx);
    p.e_x_hat = fftw_alloc_complex(p.ncx);
    p.fw  = fftw_plan_dft_1d(p.ncx, p.tmp_rho, p.rho_hat, FFTW_FORWARD,  FFTW_PATIENT);
    p.bwx = fftw_plan_dft_1d(p.ncx, p.e_x_hat, p.tmp_e_x, FFTW_BACKWARD, FFTW_PATIENT);

    // Computes the wave numbers.
    double kx0 = 2. * PI / (mesh.x_max - mesh.x_min);
    for (i = 0; i < p.ncx / 2; i++)
        p.kx[i] = i * kx0;
    for (i = p.ncx / 2; i < p.ncx; i++)
        p.kx[i] = (i - p.ncx) * kx0;

    // Computes k2 = kx^2 then normalizes kx.
    // Starts at i = 1 because kx[0] = 0.
    for (i = 1; i < p.ncx; i++) {
        p.k2[i] = sqr(p.kx[i]);
        p.kx[i] /= p.k2[i];
    }

    return p;
}

/*
 * Solve laplacian(phi(x)) = -rho(x) with periodic boundary conditions,
 * then set E(x) = -grad(phi(x)) which means
 *     Ex(x) = -partial_x phi(x)
 *
 * @param[in]  p the poisson solver (has to be initialized before the call).
 * @param[in]  rho[ncx+1] the charge density.
 * @param[out] e_x[ncx+1] the electric field on the x-axis.
 */
void compute_E_from_rho_1d_fft(poisson_1d_solver p, double* rho, double* e_x) {
    int i;

    for (i = 0; i < p.ncx; i++)
        p.tmp_rho[i] = rho[i];

    // Computes rho_hat = FFT(tmp_rho).
    fftw_execute(p.fw);

    // Computes tmp_e_x = IFFT(-I*kx*rho_hat) = IFFT(-I*kx*FFT(tmp_rho)).
    for (i = 0; i < p.ncx; i++)
        p.e_x_hat[i] = (-p.kx[i] * I) * p.rho_hat[i];
    fftw_execute(p.bwx);

    // Puts into double* array and normalize.
    for (i = 0; i < p.ncx; i++)
        e_x[i] = creal(p.tmp_e_x[i] / p.ncx);
    // Periodicity
    e_x[p.ncx] = e_x[0];
}

void free_poisson_1d(poisson_1d_solver* p) {
    free(p->kx);
    free(p->k2);
    free(p->tmp_rho);
    free(p->tmp_e_x);
    fftw_free(p->rho_hat);
    fftw_free(p->e_x_hat);
    fftw_destroy_plan(p->fw);
    fftw_destroy_plan(p->bwx);
}



/*****************************************************************************
 *                            Poisson solver 2d                              *
 *                 Periodic Boundary Conditions, FFT Algorithm               *
 *                                                                           *
 * Code translated from Fortran from Selalib, in the following files :       *
 * selalib/src/field_solvers/poisson_solvers/sll_m_poisson_2d_fft.F90        *
 * selalib/src/field_solvers/poisson_solvers/                                *
 *     sll_m_poisson_2d_periodic_fftw.F90                                    *
 *****************************************************************************/

/*
 * @param[in] mesh, the mesh on which we're working.
 * @return    a poisson solver that uses the library FFTW.
 */
poisson_2d_solver new_poisson_2d_fft_solver(cartesian_mesh_2d mesh) {
    // Warning : don't put i and j as size_t, because with size_t values,
    // i - p.ncx will take really big values instead of negative ones.
    int i, j;
    poisson_2d_solver p;
    size_t sz_fft = mesh.num_cell_x * (mesh.num_cell_y / 2 + 1);

    p.ncx = mesh.num_cell_x;
    p.ncy = mesh.num_cell_y;
    p.kx = allocate_matrix(p.ncx, p.ncy / 2 + 1);
    p.ky = allocate_matrix(p.ncx, p.ncy / 2 + 1);
    p.k2 = allocate_matrix(p.ncx, p.ncy / 2 + 1);
    p.tmp_rho = allocateMatrix(p.ncx, p.ncy);
    p.tmp_e_x = allocateMatrix(p.ncx, p.ncy);
    p.tmp_e_y = allocateMatrix(p.ncx, p.ncy);
    p.rht = fftw_alloc_complex(sz_fft);
    p.ext = fftw_alloc_complex(sz_fft);
    p.eyt = fftw_alloc_complex(sz_fft);
    p.fw  = fftw_plan_dft_r2c_2d(p.ncx, p.ncy, p.tmp_rho, p.rht, FFTW_PATIENT);
    p.bwx = fftw_plan_dft_c2r_2d(p.ncx, p.ncy, p.ext, p.tmp_e_x, FFTW_PATIENT);
    p.bwy = fftw_plan_dft_c2r_2d(p.ncx, p.ncy, p.eyt, p.tmp_e_y, FFTW_PATIENT);

    // Computes the wave numbers.
    double kx0 = 2. * PI / (mesh.x_max - mesh.x_min);
    double ky0 = 2. * PI / (mesh.y_max - mesh.y_min);
    for (i = 0; i < p.ncx / 2; i++) {
        for (j = 0; j < p.ncy / 2; j++) {
            p.kx[i][j] = i * kx0;
            p.ky[i][j] = j * ky0;
        }
        for (j = p.ncy / 2; j < p.ncy / 2 + 1; j++) {
            p.kx[i][j] = i * kx0;
            p.ky[i][j] = (j - p.ncy) * ky0;
        }
    }
    for (i = p.ncx / 2; i < p.ncx; i++) {
        for (j = 0; j < p.ncy / 2; j++) {
            p.kx[i][j] = (i - p.ncx) * kx0;
            p.ky[i][j] = j * ky0;
        }
        for (j = p.ncy / 2; j < p.ncy / 2 + 1; j++) {
            p.kx[i][j] = (i - p.ncx) * kx0;
            p.ky[i][j] = (j - p.ncy) * ky0;
        }
    }

    // Sets kx[0][0] to a non-zero value because we have kx[0][0] = ky[0][0] = 0.
    // which would lead to k2[0][0] = 0. then divide by zero error (1. is arbitrary).
    p.kx[0][0] = 1.;

    // Computes k2 = kx^2 + ky^2 then normalizes kx and ky.
    for (i = 0; i < p.ncx; i++) {
        for (j = 0; j < p.ncy / 2 + 1; j++) {
            p.k2[i][j] = sqr(p.kx[i][j]) + sqr(p.ky[i][j]);
            p.kx[i][j] /= p.k2[i][j];
            p.ky[i][j] /= p.k2[i][j];
        }
    }

    // Puts back kx[0][0] to its 0. value.
    p.kx[0][0] = 0.;

    return p;
}

/*
 * Solve laplacian(phi(x, y)) = -rho(x, y) with periodic boundary conditions.
 *
 * @param[in]  p the poisson solver (has to be initialized before the call).
 * @param[in]  rho[ncx+1][ncy+1] the charge density.
 * @param[out] phi[ncx+1][ncy+1] the electric potential.
 */
void compute_phi_from_rho_2d_fft(poisson_2d_solver p, double** rho, double** phi) {
    int i, j;

    for (i = 0; i < p.ncx; i++)
        for (j = 0; j < p.ncy; j++)
            p.tmp_rho[i * p.ncy + j] = rho[i][j];

    // Computes rht = FFT(tmp_rho).
    fftw_execute(p.fw);
    p.rht[0] = 0;

    // Computes tmp_e_x = IFFT(rht/k2) = IFFT(FFT(tmp_rho)/k2).
    for (i = 0; i < p.ncx; i++)
        for (j = 0; j < p.ncy / 2 + 1; j++)
            p.ext[i * (p.ncy / 2 + 1) + j] = p.rht[i * (p.ncy / 2 + 1) + j] / p.k2[i][j];
    fftw_execute(p.bwx);

    // Puts into double** array and normalize.
    for (i = 0; i < p.ncx; i++)
        for (j = 0; j < p.ncy; j++)
            phi[i][j] = p.tmp_e_x[i * p.ncy + j] / (p.ncx * p.ncy);

    // Periodicity
    for (i = 0; i < p.ncx; i++) {
        phi[i][p.ncy] = phi[i][0];
    }
    for (j = 0; j < p.ncy; j++) {
        phi[p.ncx][j] = phi[0][j];
    }
    phi[p.ncx][p.ncy] = phi[0][0];
}

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
void compute_E_from_rho_2d_fft(poisson_2d_solver p, double** rho, double** e_x, double** e_y) {
    int i, j;

    for (i = 0; i < p.ncx; i++)
        for (j = 0; j < p.ncy; j++)
            p.tmp_rho[i * p.ncy + j] = rho[i][j];

    // Computes rht = FFT(tmp_rho).
    fftw_execute(p.fw);

    // Computes tmp_e_x = IFFT(-I*kx*rht) = IFFT(-I*kx*FFT(tmp_rho)).
    for (i = 0; i < p.ncx; i++)
        for (j = 0; j < p.ncy / 2 + 1; j++)
            p.ext[i * (p.ncy / 2 + 1) + j] = (-p.kx[i][j] * I) * p.rht[i * (p.ncy / 2 + 1) + j];
    fftw_execute(p.bwx);

    // Computes tmp_e_y = IFFT(-I*ky*rht) = IFFT(-I*ky*FFT(tmp_rho)).
    for (i = 0; i < p.ncx; i++)
        for (j = 0; j < p.ncy / 2 + 1; j++)
            p.eyt[i * (p.ncy / 2 + 1) + j] = (-p.ky[i][j] * I) * p.rht[i * (p.ncy / 2 + 1) + j];
    fftw_execute(p.bwy);

    // Puts into double** array and normalize.
    for (i = 0; i < p.ncx; i++)
        for (j = 0; j < p.ncy; j++)
            e_x[i][j] = p.tmp_e_x[i * p.ncy + j] / (p.ncx * p.ncy);
    for (i = 0; i < p.ncx; i++)
        for (j = 0; j < p.ncy; j++)
            e_y[i][j] = p.tmp_e_y[i * p.ncy + j] / (p.ncx * p.ncy);

    // Periodicity
    for (i = 0; i < p.ncx; i++) {
        e_x[i][p.ncy] = e_x[i][0];
        e_y[i][p.ncy] = e_y[i][0];
    }
    for (j = 0; j < p.ncy; j++) {
        e_x[p.ncx][j] = e_x[0][j];
        e_y[p.ncx][j] = e_y[0][j];
    }
    e_x[p.ncx][p.ncy] = e_x[0][0];
    e_y[p.ncx][p.ncy] = e_y[0][0];
}

void free_poisson_2d(poisson_2d_solver* p) {
    deallocate_matrix(p->kx, p->ncx, p->ncy / 2 + 1);
    deallocate_matrix(p->ky, p->ncx, p->ncy / 2 + 1);
    deallocate_matrix(p->k2, p->ncx, p->ncy / 2 + 1);
    deallocateMatrix(p->tmp_rho, p->ncx, p->ncy);
    deallocateMatrix(p->tmp_e_x, p->ncx, p->ncy);
    deallocateMatrix(p->tmp_e_y, p->ncx, p->ncy);
    fftw_free(p->rht);
    fftw_free(p->ext);
    fftw_free(p->eyt);
    fftw_destroy_plan(p->fw);
    fftw_destroy_plan(p->bwx);
    fftw_destroy_plan(p->bwy);
}



/*****************************************************************************
 *                            Poisson solver 3d                              *
 *                 Periodic Boundary Conditions, FFT Algorithm               *
 *                                                                           *
 * Code translated from C++ from Ksander's code, in the following file :     *
 * ksander_bsl_6d.cpp                                                        *
 *****************************************************************************/

/*
 * @param[in] mesh, the mesh on which we're working.
 * @return    a poisson solver that uses the library FFTW.
 */
poisson_3d_solver new_poisson_3d_fft_solver(cartesian_mesh_3d mesh) {
    // Warning : don't put i and j as size_t, because with size_t values,
    // i - p.ncx will take really big values instead of negative ones.
    int i, j, k;
    int num_threads;
    poisson_3d_solver p;

    #pragma omp parallel
    num_threads = omp_get_num_threads();
    p.ncx = mesh.num_cell_x;
    p.ncy = mesh.num_cell_y;
    p.ncz = mesh.num_cell_z;

    int N = max(p.ncx, max(p.ncy, p.ncz));
    p.in  = malloc(num_threads * sizeof(fftw_complex*));
    p.out = malloc(num_threads * sizeof(fftw_complex*));
    p.px     = malloc(num_threads * sizeof(fftw_plan));
    p.py     = malloc(num_threads * sizeof(fftw_plan));
    p.pz     = malloc(num_threads * sizeof(fftw_plan));
    p.px_inv = malloc(num_threads * sizeof(fftw_plan));
    p.py_inv = malloc(num_threads * sizeof(fftw_plan));
    p.pz_inv = malloc(num_threads * sizeof(fftw_plan));
    for (i = 0; i < num_threads; i++) {
        p.in[i]  = fftw_alloc_complex(N);
        p.out[i] = fftw_alloc_complex(N);
        p.px[i]     = fftw_plan_dft_1d(p.ncx, p.in[i], p.out[i], FFTW_FORWARD,  FFTW_ESTIMATE);
        p.py[i]     = fftw_plan_dft_1d(p.ncy, p.in[i], p.out[i], FFTW_FORWARD,  FFTW_ESTIMATE);
        p.pz[i]     = fftw_plan_dft_1d(p.ncz, p.in[i], p.out[i], FFTW_FORWARD,  FFTW_ESTIMATE);
        p.px_inv[i] = fftw_plan_dft_1d(p.ncx, p.in[i], p.out[i], FFTW_BACKWARD, FFTW_ESTIMATE);
        p.py_inv[i] = fftw_plan_dft_1d(p.ncy, p.in[i], p.out[i], FFTW_BACKWARD, FFTW_ESTIMATE);
        p.pz_inv[i] = fftw_plan_dft_1d(p.ncz, p.in[i], p.out[i], FFTW_BACKWARD, FFTW_ESTIMATE);
    }

    p.kx = allocate_array(p.ncx);
    p.ky = allocate_array(p.ncy);
    p.kz = allocate_array(p.ncz);

    p.k2 = allocate_3d_array(p.ncx, p.ncy, p.ncz);

    p.hat_rho = malloc(p.ncx * sizeof(fftw_complex**));
    if (!p.hat_rho) {
        fprintf(stderr, "allocate_3d_array(%d, %d, %d) : malloc error.\n", p.ncx, p.ncy, p.ncz);
        exit(EXIT_FAILURE);
    }
    for (i = 0; i < p.ncx; i++) {
        p.hat_rho[i] = malloc(p.ncy * sizeof(fftw_complex*));
        if (!p.hat_rho[i]) {
            fprintf(stderr, "allocate_3d_array(%d, %d, %d) : malloc error.\n", p.ncx, p.ncy, p.ncz);
            exit(EXIT_FAILURE);
        }
        for (j = 0; j < p.ncy; j++)
            p.hat_rho[i][j] = fftw_alloc_complex(p.ncz);
    }

    // Computes the wave numbers.
    double kx0 = 2. * PI / (mesh.x_max - mesh.x_min);
    for (i = 0; i < p.ncx / 2; i++)
        p.kx[i] = (double)i * kx0;
    for (i = p.ncx / 2; i < p.ncx; i++)
        p.kx[i] = (double)(i - p.ncx) * kx0;

    double ky0 = 2. * PI / (mesh.y_max - mesh.y_min);
    for (j = 0; j < p.ncy / 2; j++)
        p.ky[j] = (double)j * ky0;
    for (j = p.ncy / 2; j < p.ncy; j++)
        p.ky[j] = (double)(j - p.ncy) * ky0;

    double kz0 = 2. * PI / (mesh.z_max - mesh.z_min);
    for (k = 0; k < p.ncz / 2; k++)
        p.kz[k] = (double)k * kz0;
    for (k = p.ncz / 2; k < p.ncz; k++)
        p.kz[k] = (double)(k - p.ncz) * kz0;

    // Computes k2 = kx^2 + ky^2 + kz^2.
    // Warning : Do not divide by k2[0][0][0] = 0 in the solve Poisson code.
    for (i = 0; i < p.ncx; i++)
        for (j = 0; j < p.ncy; j++)
            for (k = 0; k < p.ncz; k++)
                p.k2[i][j][k] = sqr(p.kx[i]) + sqr(p.ky[j]) + sqr(p.kz[k]);

    return p;
}

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
void compute_E_from_rho_3d_fft(poisson_3d_solver p, double*** rho, double*** e_x, double*** e_y, double*** e_z) {
    int i, j, k, thread_id;
    const int ncx = p.ncx;
    const int ncy = p.ncy;
    const int ncz = p.ncz;

    #pragma omp parallel private(i, j, k, thread_id) firstprivate(ncx, ncy, ncz)
    {
        thread_id = omp_get_thread_num();
        // FFT in x
        #pragma omp for collapse(2)
        for (j = 0; j < ncy; j++) {
            for (k = 0; k < ncz; k++) {
                for (i = 0; i < ncx; i++)
                    p.in[thread_id][i] = rho[i][j][k];
                fftw_execute(p.px[thread_id]);
                for (i = 0; i < ncx; i++)
                    p.hat_rho[i][j][k] = p.out[thread_id][i];
            }
        }
        // FFT in y
        #pragma omp for collapse(2)
        for (i = 0; i < ncx; i++) {
            for (k = 0; k < ncz; k++) {
                for (j = 0; j < ncy; j++)
                    p.in[thread_id][j] = p.hat_rho[i][j][k];
                fftw_execute(p.py[thread_id]);
                for (j = 0; j < ncy; j++)
                    p.hat_rho[i][j][k] = p.out[thread_id][j];
            }
        }
        // FFT in z
        #pragma omp for collapse(2)
        for (i = 0; i < ncx; i++) {
            for (j = 0; j < ncy; j++) {
                for (k = 0; k < ncz; k++)
                    p.in[thread_id][k] = p.hat_rho[i][j][k];
                fftw_execute(p.pz[thread_id]);
                for (k = 0; k < ncz; k++) {
                    //and normalization
                    p.hat_rho[i][j][k] = p.out[thread_id][k] / ((double)ncx * (double)ncy * (double)ncz);
                }
            }
        }

        // Compute hat_phi (in hat_rho)
        #pragma omp for collapse(3)
        for (i = 0; i < ncx; i++)
            for (j = 0; j < ncy; j++)
                for (k = 0; k < ncz; k++)
                    if (i == 0 && j == 0 && k == 0)
                        p.hat_rho[0][0][0] = 0.;
                    else
                        p.hat_rho[i][j][k] /= p.k2[i][j][k];

        // FFT_inv in z
        #pragma omp for collapse(2)
        for (i = 0; i < ncx; i++) {
            for (j = 0; j < ncy; j++) {
                for (k = 0; k < ncz; k++)
                    p.in[thread_id][k] = p.hat_rho[i][j][k];
                fftw_execute(p.pz_inv[thread_id]);
                for (k = 0; k < ncz; k++)
                    p.hat_rho[i][j][k] = p.out[thread_id][k];
            }
        }
        // FFT_inv in y
        #pragma omp for collapse(2)
        for (i = 0; i < ncx; i++) {
            for (k = 0; k < ncz; k++) {
                for (j = 0; j < ncy; j++)
                    p.in[thread_id][j] = p.hat_rho[i][j][k];
                fftw_execute(p.py_inv[thread_id]);
                for (j = 0; j < ncy; j++)
                    p.hat_rho[i][j][k] = p.out[thread_id][j];
            }
        }
        // FFT_inv in x
        #pragma omp for collapse(2)
        for (j = 0; j < ncy; j++) {
            for (k = 0; k < ncz; k++) {
                for (i = 0; i < ncx; i++)
                    p.in[thread_id][i] = p.hat_rho[i][j][k];
                fftw_execute(p.px_inv[thread_id]);
                for (i = 0; i < ncx; i++)
                    p.hat_rho[i][j][k] = p.out[thread_id][i];
            }
        }

        // Compute E: FFT + derive + FFT inv + Assign in E_field
        // Compute Ex
        #pragma omp for collapse(2)
        for (k = 0; k < ncz; k++) {
            for (j = 0; j < ncy; j++) {
                for (i = 0; i < ncx; i++)
                    p.in[thread_id][i] = p.hat_rho[i][j][k];
                fftw_execute(p.px[thread_id]);
                for (i = 0; i < ncx; i++)
                    p.in[thread_id][i] = -I * p.kx[i] * p.out[thread_id][i];
                fftw_execute(p.px_inv[thread_id]);
                for (i = 0; i < ncx; i++)
                    e_x[i][j][k] = creal(p.out[thread_id][i]) / ((double)ncx);
            }
        }

        // Compute Ey
        #pragma omp for collapse(2)
        for (k = 0; k < ncz; k++) {
            for (i = 0; i < ncx; i++) {
                for (j = 0; j < ncy; j++)
                    p.in[thread_id][j] = p.hat_rho[i][j][k];
                fftw_execute(p.py[thread_id]);
                for (j = 0; j < ncy; j++)
                    p.in[thread_id][j] = -I * p.ky[j] * p.out[thread_id][j];
                fftw_execute(p.py_inv[thread_id]);
                for (j = 0; j < ncy; j++)
                    e_y[i][j][k] = creal(p.out[thread_id][j]) / ((double)ncy);
            }
        }

        // Compute Ez
        #pragma omp for collapse(2)
        for (i = 0; i < ncx; i++) {
            for (j = 0; j < ncy; j++) {
                for (k = 0; k < ncz; k++)
                    p.in[thread_id][k] = p.hat_rho[i][j][k];
                fftw_execute(p.pz[thread_id]);
                for (k = 0; k < ncz; k++)
                    p.in[thread_id][k] = -I * p.kz[k] * p.out[thread_id][k];
                fftw_execute(p.pz_inv[thread_id]);
                for (k = 0; k < ncz; k++)
                    e_z[i][j][k] = creal(p.out[thread_id][k]) / ((double)ncz);
            }
        }

        // Periodicity
        #pragma omp for collapse(2)
        for (i = 0; i < ncx; i++) {
            for (k = 0; k < ncz; k++) {
                e_x[i][ncy][k] = e_x[i][0][k];
                e_y[i][ncy][k] = e_y[i][0][k];
                e_z[i][ncy][k] = e_z[i][0][k];
            }
        }
        #pragma omp for collapse(2)
        for (j = 0; j < ncy; j++) {
            for (k = 0; k < ncz; k++) {
                e_x[ncx][j][k] = e_x[0][j][k];
                e_y[ncx][j][k] = e_y[0][j][k];
                e_z[ncx][j][k] = e_z[0][j][k];
            }
        }
        #pragma omp for collapse(2)
        for (i = 0; i < ncx; i++) {
            for (j = 0; j < ncy; j++) {
                e_x[i][j][ncz] = e_x[i][j][0];
                e_y[i][j][ncz] = e_y[i][j][0];
                e_z[i][j][ncz] = e_z[i][j][0];
            }
        }
        #pragma omp for
        for (i = 0; i < ncx; i++) {
            e_x[i][ncy][ncz] = e_x[i][0][0];
            e_y[i][ncy][ncz] = e_y[i][0][0];
            e_z[i][ncy][ncz] = e_z[i][0][0];
        }
        #pragma omp for
        for (j = 0; j < ncy; j++) {
            e_x[ncx][j][ncz] = e_x[0][j][0];
            e_y[ncx][j][ncz] = e_y[0][j][0];
            e_z[ncx][j][ncz] = e_z[0][j][0];
        }
        #pragma omp for
        for (k = 0; k < ncz; k++) {
            e_x[ncx][ncy][k] = e_x[0][0][k];
            e_y[ncx][ncy][k] = e_y[0][0][k];
            e_z[ncx][ncy][k] = e_z[0][0][k];
        }
    } // End parallel region
    e_x[ncx][ncy][ncz] = e_x[0][0][0];
    e_y[ncx][ncy][ncz] = e_y[0][0][0];
    e_z[ncx][ncy][ncz] = e_z[0][0][0];
}

void free_poisson_3d(poisson_3d_solver* p) {
    int num_threads;
    size_t i, j;

    #pragma omp parallel
    num_threads = omp_get_num_threads();
    for (i = 0; i < num_threads; i++) {
        fftw_destroy_plan(p->px[i]);
        fftw_destroy_plan(p->py[i]);
        fftw_destroy_plan(p->pz[i]);
        fftw_destroy_plan(p->px_inv[i]);
        fftw_destroy_plan(p->py_inv[i]);
        fftw_destroy_plan(p->pz_inv[i]);
        fftw_free(p->in[i]);
        fftw_free(p->out[i]);
    }
    free(p->px);
    free(p->py);
    free(p->pz);
    free(p->px_inv);
    free(p->py_inv);
    free(p->pz_inv);
    free(p->in);
    free(p->out);
    deallocate_array(p->kx, p->ncx);
    deallocate_array(p->ky, p->ncy);
    deallocate_array(p->kz, p->ncz);
    deallocate_3d_array(p->k2, p->ncx, p->ncy, p->ncz);
    for (i = 0; i < p->ncx; i++) {
        for (j = 0; j < p->ncy; j++) {
            fftw_free(p->hat_rho[i][j]);
        }
        free(p->hat_rho[i]);
    }
    free(p->hat_rho);
}

