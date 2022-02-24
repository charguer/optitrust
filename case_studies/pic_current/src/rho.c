#include <stdio.h>                 // function  printf (output strings on standard output)
#include <stdlib.h>                // function  exit (error handling)
                                   // constant  EXIT_FAILURE (error handling)
#include <mpi.h>                   // constants MPI_COMM_WORLD, MPI_DOUBLE_PRECISION, MPI_SUM
                                   // functions MPI_Allreduce, MPI_Iallreduce
                                   // types     MPI_Request, MPI_Status
#include <stdlib.h>                // type      size_t
#include "compiler_test.h"         // constant  PIC_VERT_MPI_3_0
#include "matrix_functions.h"      // functions allocate_aligned_double_matrix, deallocate_matrix
#include "rho.h"                   // constants NB_CORNERS_2D, NB_CORNERS_3D
#include "space_filling_curves.h"  // macros    COMPUTE_I_CELL_2D, COMPUTE_I_CELL_3D, I_CELL_PARAM_2D, I_CELL_PARAM1_3D, I_CELL_PARAM2_3D

/*****************************************************************************
 *                   Volume charge density handling 1d                       *
 *****************************************************************************/

/*
 * Conversion from local rho to global rho, with periodicity.
 * @param[in]      mpi_world_size the number of MPI processors.
 * @param[in, out] send_buf[ncx], recv_buf[ncx] the MPI buffers.
 * @param[in]      ncx mesh size on the x-axis.
 * @param[in, out] rho[ncx+1], the particle density.
 */
void mpi_reduce_rho_1d(int mpi_world_size, double* send_buf, double* recv_buf,
        int ncx, double* rho) {
    if (mpi_world_size > 1) {
        size_t i;
        for (i = 0; i < ncx; i++) {
            send_buf[i] = rho[i];
            recv_buf[i] = 0.;
        }
        MPI_Allreduce(send_buf, recv_buf, ncx, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD);
        for (i = 0; i < ncx; i++)
            rho[i] = recv_buf[i];
        // Periodicity
        rho[ncx] = rho[0];
    }
}

/*
 * x-axis : left -> right
 *
 * (0) : LEFT
 * (1) : RIGHT
 *
 *      dx
 *     <———>
 *      +———|——————+
 * LEFT |   O      | RIGHT
 *      +———|——————+
 *
 */

/*
 * Resets a charge accumulator.
 * @param[in]  ncx mesh size on the x-axis.
 * @param[out] charge_accu[ncx] the charge accumulator with 0s in every cell.
 */
void reset_charge_1d_accumulator(int ncx,
        int num_threads, double* charge_accu) {
    for (size_t i = 0; i < ncx * NB_CORNERS_1D * num_threads; i++)
        charge_accu[i] = 0.;
}

/*
 * Conversion from charge accumulator to rho, with periodicity.
 * @param[in]  charge_accu[ncx] the charge MASS accumulator.
 * @param[in]  ncx mesh size on the x-axis.
 * @param[in]  factor is a number by which the whole array is multiplicated
 *             (done in this function to gain performance).
 * @param[out] rho[ncx+1], the charge density.
 */
void convert_charge_to_rho_1d_per_per(double* charge_accu,
        int num_threads, int ncx, double factor, double* rho) {
    int i, i_thread, offset;

    for (i = 0; i < ncx + 1; i++)
        rho[i] = 0.;

  for (i_thread = 0; i_thread < num_threads; i_thread++) {
    offset = i_thread * NB_CORNERS_1D * ncx;
    for (i = 1; i < ncx; i++) {
        // general i
        rho[i] += (charge_accu[offset + NB_CORNERS_1D*(i    ) + LEFT] +
                   charge_accu[offset + NB_CORNERS_1D*(i - 1) + RIGHT]);
    }

    // i = 0
    rho[ 0 ] += charge_accu[offset + NB_CORNERS_1D*(0      ) + LEFT];
    // i = ncx
    rho[ncx] += charge_accu[offset + NB_CORNERS_1D*(ncx - 1) + RIGHT];
  }

    // Periodicity
    rho[ 0 ] += rho[ncx];
    rho[ncx]  = rho[ 0 ];

    // Normalization
    for (i = 0; i < ncx + 1; i++)
        rho[i] *= factor;
}



/*****************************************************************************
 *                   Volume charge density handling 2d                       *
 *                                                                           *
 * Code translated from Fortran from Selalib, in the following files :       *
 * selalib/src/particle_methods/pic_2d_standard/pic_utilities/               *
 *     sll_m_charge_to_density.F90                                           *
 * selalib/src/particle_methods/pic_2d_standard/pic_accumulators/            *
 *     sll_m_accumulators.F90                                                *
 *****************************************************************************/

/*
 * Conversion from local rho to global rho, with periodicity.
 *
 * @param[in]      mpi_world_size the number of MPI processors.
 * @param[in, out] send_buf[ncx*ncy], recv_buf[ncx*ncy] the MPI buffers.
 * @param[in]      ncx mesh size on the x-axis.
 * @param[in]      ncy mesh size on the y-axis.
 * @param[in, out] rho[ncx+1][ncy+1], the particle density
 *                    in  : as computed locally
 *                    out : reduced from all the local ones
 */
void mpi_reduce_rho_2d(int mpi_world_size, double* send_buf, double* recv_buf,
        int ncx, int ncy, double** rho) {
    if (mpi_world_size > 1) {
        size_t i, j;
        for (i = 0; i < ncx; i++)
            for (j = 0; j < ncy; j++)
                send_buf[i * ncy + j] = rho[i][j];
        MPI_Allreduce(send_buf, recv_buf, ncx * ncy, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD);
        for (i = 0; i < ncx; i++)
            for (j = 0; j < ncy; j++)
                rho[i][j] = recv_buf[i * ncy + j];
        // Periodicity
        for (i = 0; i < ncx + 1; i++)
            rho[i][ncy] = rho[i][0];
        for (j = 0; j < ncy + 1; j++)
            rho[ncx][j] = rho[0][j];
    }
}

/*
 * Conversion from local rho to global rho, with periodicity.
 *
 * @param[in]      mpi_world_size the number of MPI processors.
 * @param[in, out] send_buf[ncx*ncy], recv_buf[ncx*ncy] the MPI buffers.
 * @param[in]      ncx mesh size on the x-axis.
 * @param[in]      ncy mesh size on the y-axis.
 * @param[in]      rho[ncx+1][ncy+1], the particle density, as computed locally.
 * @param[out]     request_all_reduce
 */
void launch_non_blocking_reduce_rho_2d(int mpi_world_size, double* send_buf, double* recv_buf,
        int ncx, int ncy, double** rho, MPI_Request* request_all_reduce) {
#if defined(PIC_VERT_MPI_3_0)
    if (mpi_world_size > 1) {
        for (size_t i = 0; i < ncx; i++)
            for (size_t j = 0; j < ncy; j++)
                send_buf[i * ncy + j] = rho[i][j];
        MPI_Iallreduce(send_buf, recv_buf, ncx * ncy, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, request_all_reduce);
    }
#else
    printf("MPI_Iallreduce needs MPI 3.0 support. Please update your MPI wrapper.\n");
    exit(EXIT_FAILURE);
#endif
}

/*
 * Conversion from local rho to global rho, with periodicity.
 *
 * @param[in]  mpi_world_size the number of MPI processors.
 * @param[in]  recv_buf[ncx*ncy] the MPI receive buffer.
 * @param[in]  ncx mesh size on the x-axis.
 * @param[in]  ncy mesh size on the y-axis.
 * @param[out] rho[ncx+1][ncy+1], the particle density, reduced from all the local ones.
 * @param[in]  request_all_reduce
 */
void wait_non_blocking_reduce_rho_2d(int mpi_world_size, double* recv_buf,
        int ncx, int ncy, double** rho, MPI_Request* request_all_reduce) {
    if (mpi_world_size > 1) {
        size_t i, j;
        MPI_Status status_all_reduce;
        MPI_Wait(request_all_reduce, &status_all_reduce);
        for (i = 0; i < ncx; i++)
            for (j = 0; j < ncy; j++)
                rho[i][j] = recv_buf[i * ncy + j];
        // Periodicity
        for (i = 0; i < ncx + 1; i++)
            rho[i][ncy] = rho[i][0];
        for (j = 0; j < ncy + 1; j++)
            rho[ncx][j] = rho[0][j];
    }
}

/*
 * x-axis : west  -> east
 * y-axis : south -> north
 *
 * (0, 0) : South, West : SW
 * (0, 1) : North, West : NW
 * (1, 0) : South, East : SE
 * (1, 1) : North, East : NE
 *
 *    dx
 *   <———>
 * NW +——————————+ NE
 *    |   |      |
 *    |———O——————|     ^
 *    |   |      |     |
 *    |   |      |     |dy
 * SW +——————————+ SE  v
 *
 */

/*
 * Resets a charge accumulator.
 * @param[in]  ncx mesh size on the x-axis.
 * @param[in]  ncy mesh size on the y-axis.
 * @param[in]  num_threads number of threads.
 * @param[out] charge_accu[ncx*ncy*NB_CORNERS_2D*num_threads] the charge accumulator with 0s in every cell.
 */
void reset_charge_2d_accumulator(int ncx, int ncy,
        int num_threads, double* charge_accu) {
    for (size_t i = 0; i < ncx * ncy * NB_CORNERS_2D * num_threads; i++)
        charge_accu[i] = 0.;
}

/*
 * Conversion from charge accumulator to rho, with periodicity.
 * @param[in]  charge_accu[ncx*ncy*NB_CORNERS_2D*num_threads] the charge MASS accumulator.
 * @param[in]  ncx mesh size on the x-axis.
 * @param[in]  ncy mesh size on the y-axis.
 * @param[in]  factor is a number by which the whole array is multiplicated
 *             (done in this function to gain performance).
 * @param[out] rho[ncx+1][ncy+1], the charge density.
 */
void convert_charge_to_rho_2d_per_per(double* charge_accu,
        int num_threads, int ncx, int ncy, double factor, double** rho) {
    int i, j, corner, offset;
    const int ncxminusone = ncx - 1;
    const int ncyminusone = ncy - 1;
    const int icell_param = I_CELL_PARAM_2D(ncx, ncy);

    int num_cells_2d = ncx * ncy;
    double** reduced_charge_accu = allocate_aligned_double_matrix(num_cells_2d, NB_CORNERS_2D);

    #pragma omp parallel private(i, j, corner, offset) firstprivate(icell_param, ncx, ncy, ncxminusone, ncyminusone, factor)
    {
        #pragma omp for
        for (j = 0; j < num_cells_2d; j++) {
#ifdef PIC_VERT_OPENMP_4_0
            #pragma omp simd
#endif
            for (corner = 0; corner < NB_CORNERS_2D; corner++) {
                reduced_charge_accu[j][corner] = charge_accu[NB_CORNERS_2D * j + corner];
            }
            for (i = 1; i < num_threads; i++) {
                offset = i * NB_CORNERS_2D * num_cells_2d;
#ifdef PIC_VERT_OPENMP_4_0
                #pragma omp simd
#endif
                for (corner = 0; corner < NB_CORNERS_2D; corner++)
                    reduced_charge_accu[j][corner] += charge_accu[offset + NB_CORNERS_2D * j + corner];
            }
        }

        #pragma omp for collapse(2)
        for (i = 0; i < ncx + 1; i++)
            for (j = 0; j < ncy + 1; j++)
                rho[i][j] = factor * (
                    reduced_charge_accu[COMPUTE_I_CELL_2D(icell_param,  i   &ncxminusone,  j   &ncyminusone)][SOUTH_WEST] +
                    reduced_charge_accu[COMPUTE_I_CELL_2D(icell_param, (i-1)&ncxminusone,  j   &ncyminusone)][SOUTH_EAST] +
                    reduced_charge_accu[COMPUTE_I_CELL_2D(icell_param, (i-1)&ncxminusone, (j-1)&ncyminusone)][NORTH_EAST] +
                    reduced_charge_accu[COMPUTE_I_CELL_2D(icell_param,  i   &ncxminusone, (j-1)&ncyminusone)][NORTH_WEST]);
    }

    deallocate_matrix(reduced_charge_accu, num_cells_2d, NB_CORNERS_2D);
}



/*****************************************************************************
 *                   Volume charge density handling 3d                       *
 *****************************************************************************/

/*
 * Conversion from local rho to global rho, with periodicity.
 * @param[in]      mpi_world_size the number of MPI processors.
 * @param[in, out] send_buf[ncx*ncy*ncz], recv_buf[ncx*ncy*ncz] the MPI buffers.
 * @param[in]      ncx mesh size on the x-axis.
 * @param[in]      ncy mesh size on the y-axis.
 * @param[in]      ncz mesh size on the z-axis.
 * @param[in, out] rho[ncx+1][ncy+1][ncz+1], the particle density.
 */
void mpi_reduce_rho_3d(int mpi_world_size, double* send_buf, double* recv_buf,
        int ncx, int ncy, int ncz, double*** rho) {
    if (mpi_world_size > 1) {
        size_t i, j, k;
        for (i = 0; i < ncx; i++)
            for (j = 0; j < ncy; j++)
                for (k = 0; k < ncz; k++)
                    send_buf[(i * ncy + j) * ncz + k] = rho[i][j][k];
        MPI_Allreduce(send_buf, recv_buf, ncx * ncy * ncz, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD);
        for (i = 0; i < ncx; i++)
            for (j = 0; j < ncy; j++)
                for (k = 0; k < ncz; k++)
                    rho[i][j][k] = recv_buf[(i * ncy + j) * ncz + k];
        // Periodicity
        for (i = 0; i < ncx + 1; i++)
            for (k = 0; k < ncz + 1; k++)
                rho[i][ncy][k] = rho[i][0][k];
        for (j = 0; j < ncy + 1; j++)
            for (k = 0; k < ncz + 1; k++)
                rho[ncx][j][k] = rho[0][j][k];
        for (i = 0; i < ncx + 1; i++)
            for (j = 0; j < ncy + 1; j++)
                rho[i][j][ncz] = rho[i][j][0];
    }
}

/*
 * Conversion from local rho to global rho, with periodicity.
 *
 * @param[in]      mpi_world_size the number of MPI processors.
 * @param[in, out] send_buf[ncx*ncy*ncz], recv_buf[ncx*ncy*ncz] the MPI buffers.
 * @param[in]      ncx mesh size on the x-axis.
 * @param[in]      ncy mesh size on the y-axis.
 * @param[in]      ncz mesh size on the z-axis.
 * @param[in]      rho[ncx+1][ncy+1][ncz+1], the particle density, as computed locally.
 * @param[out]     request_all_reduce
 */
void launch_non_blocking_reduce_rho_3d(int mpi_world_size, double* send_buf, double* recv_buf,
        int ncx, int ncy, int ncz, double*** rho, MPI_Request* request_all_reduce) {
#if defined(PIC_VERT_MPI_3_0)
    if (mpi_world_size > 1) {
        for (size_t i = 0; i < ncx; i++)
            for (size_t j = 0; j < ncy; j++)
                for (size_t k = 0; k < ncz; k++)
                    send_buf[(i * ncy + j) * ncz + k] = rho[i][j][k];
        MPI_Iallreduce(send_buf, recv_buf, ncx * ncy * ncz, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, request_all_reduce);
    }
#else
    printf("MPI_Iallreduce needs MPI 3.0 support. Please update your MPI wrapper.\n");
    exit(EXIT_FAILURE);
#endif
}

/*
 * Conversion from local rho to global rho, with periodicity.
 *
 * @param[in]  mpi_world_size the number of MPI processors.
 * @param[in]  recv_buf[ncx*ncy*ncz] the MPI receive buffer.
 * @param[in]  ncx mesh size on the x-axis.
 * @param[in]  ncy mesh size on the y-axis.
 * @param[in]  ncz mesh size on the z-axis.
 * @param[out] rho[ncx+1][ncy+1][ncz+1], the particle density, reduced from all the local ones.
 * @param[in]  request_all_reduce
 */
void wait_non_blocking_reduce_rho_3d(int mpi_world_size, double* recv_buf,
        int ncx, int ncy, int ncz, double*** rho, MPI_Request* request_all_reduce) {
    if (mpi_world_size > 1) {
        size_t i, j, k;
        MPI_Status status_all_reduce;
        MPI_Wait(request_all_reduce, &status_all_reduce);
        for (i = 0; i < ncx; i++)
            for (j = 0; j < ncy; j++)
                for (k = 0; k < ncz; k++)
                    rho[i][j][k] = recv_buf[(i * ncy + j) * ncz + k];
        // Periodicity
        for (i = 0; i < ncx + 1; i++)
            for (k = 0; k < ncz + 1; k++)
                rho[i][ncy][k] = rho[i][0][k];
        for (j = 0; j < ncy + 1; j++)
            for (k = 0; k < ncz + 1; k++)
                rho[ncx][j][k] = rho[0][j][k];
        for (i = 0; i < ncx + 1; i++)
            for (j = 0; j < ncy + 1; j++)
                rho[i][j][ncz] = rho[i][j][0];
    }
}

/*
 * x-axis : left  -> right
 * y-axis : front -> back
 * z-axis : down  -> top
 *
 * (0, 0, 0) : Left,  Front, Down : LFD
 * (0, 0, 1) : Left,  Front, Top  : LFT
 * (0, 1, 0) : Left,  Back,  Down : LBD
 * (0, 1, 1) : Left,  Back,  Top  : LBT
 * (1, 0, 0) : Right, Front, Down : RFD
 * (1, 0, 1) : Right, Front, Top  : RFT
 * (1, 1, 0) : Right, Back,  Down : RBD
 * (1, 1, 1) : Right, Back,  Top  : RBT
 *
 * (0.5, 0.5, 0.5) : LGBT (-:
 *
 *
 *    LBT +————————+ RBT
 *       /'       /|
 *      / '   RFT/ |
 * LFT +————————+  |
 *     |  '     |  |
 *     |  +-----|--+ RBD
 *     | /LBD   | /
 *     |/       |/
 * LFD +————————+ RFD
 *
 */

/*
 * Resets a charge accumulator.
 * @param[in]  ncx mesh size on the x-axis.
 * @param[in]  ncy mesh size on the y-axis.
 * @param[in]  ncz mesh size on the z-axis.
 * @param[in]  num_threads number of threads.
 * @param[out] charge_accu[ncx*ncy*ncz*NB_CORNERS_3D*num_threads] the charge accumulator with 0s in every cell.
 */
void reset_charge_3d_accumulator(int ncx, int ncy, int ncz,
        int num_threads, double* charge_accu) {
    for (size_t i = 0; i < ncx * ncy * ncz * NB_CORNERS_3D * num_threads; i++)
        charge_accu[i] = 0.;
}

/*
 * Conversion from charge accumulator to rho, with periodicity.
 * @param[in]  charge_accu[ncx*ncy*ncz*NB_CORNERS_3D] the charge MASS accumulator.
 * @param[in]  ncx mesh size on the x-axis.
 * @param[in]  ncy mesh size on the y-axis.
 * @param[in]  ncz mesh size on the y-axis.
 * @param[in]  factor is a number by which the whole array is multiplicated
 *             (done in this function to gain performance).
 * @param[out] rho[ncx+1][ncy+1][ncz+1], the charge density.
 */
void convert_charge_to_rho_3d_per_per(double* charge_accu,
        int ncx, int ncy, int ncz, double factor, double*** rho) {
    int i, j, k;
    const int ncxminusone = ncx - 1;
    const int ncyminusone = ncy - 1;
    const int nczminusone = ncz - 1;
    const int param1 = I_CELL_PARAM1_3D(ncx, ncy, ncz);
    const int param2 = I_CELL_PARAM2_3D(ncx, ncy, ncz);

    #pragma omp parallel for private(i, j, k) firstprivate(param1, param2, ncx, ncy, ncz, ncxminusone, ncyminusone, nczminusone) collapse(3)
    for (i = 0; i < ncx + 1; i++) {
        for (j = 0; j < ncy + 1; j++) {
            for (k = 0; k < ncz + 1; k++) {
                rho[i][j][k] = factor * (
                    charge_accu[NB_CORNERS_3D*COMPUTE_I_CELL_3D(param1, param2,  i   &ncxminusone,  j   &ncyminusone,  k   &nczminusone) + LEFT_FRONT_DOWN] +
                    charge_accu[NB_CORNERS_3D*COMPUTE_I_CELL_3D(param1, param2, (i-1)&ncxminusone,  j   &ncyminusone,  k   &nczminusone) + RIGHT_FRONT_DOWN] +
                    charge_accu[NB_CORNERS_3D*COMPUTE_I_CELL_3D(param1, param2, (i-1)&ncxminusone, (j-1)&ncyminusone,  k   &nczminusone) + RIGHT_BACK_DOWN] +
                    charge_accu[NB_CORNERS_3D*COMPUTE_I_CELL_3D(param1, param2,  i   &ncxminusone, (j-1)&ncyminusone,  k   &nczminusone) + LEFT_BACK_DOWN] +
                    charge_accu[NB_CORNERS_3D*COMPUTE_I_CELL_3D(param1, param2,  i   &ncxminusone,  j   &ncyminusone, (k-1)&nczminusone) + LEFT_FRONT_TOP] +
                    charge_accu[NB_CORNERS_3D*COMPUTE_I_CELL_3D(param1, param2, (i-1)&ncxminusone,  j   &ncyminusone, (k-1)&nczminusone) + RIGHT_FRONT_TOP] +
                    charge_accu[NB_CORNERS_3D*COMPUTE_I_CELL_3D(param1, param2, (i-1)&ncxminusone, (j-1)&ncyminusone, (k-1)&nczminusone) + RIGHT_BACK_TOP] +
                    charge_accu[NB_CORNERS_3D*COMPUTE_I_CELL_3D(param1, param2,  i   &ncxminusone, (j-1)&ncyminusone, (k-1)&nczminusone) + LEFT_BACK_TOP]);
#ifdef DEBUG_CHARGE
                // printf("rho[%d][%d][%d] = %lf\n", i, j, k, rho[i][j][k]);
                if (i < ncx && j < ncy && k < ncz)
                   printf("rho[%d][%d][%d] / factor = %lf\n", i, j, k, rho[i][j][k] / factor);
#endif
            }
        }
    }
}

