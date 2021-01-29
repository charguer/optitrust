#ifndef PIC_VERT_RHO
#define PIC_VERT_RHO

#include <mpi.h> // type MPI_Request

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
        int ncx, double* rho);

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

enum CORNERS_1D {
    LEFT,
    RIGHT,
    NB_CORNERS_1D // Always has to be last if you update this enum !
};

/*
 * Resets a charge accumulator.
 * @param[in]  ncx mesh size on the x-axis.
 * @param[out] charge_accu[ncx] the charge accumulator with 0s in every cell.
 */
void reset_charge_1d_accumulator(int ncx,
        int num_threads, double* charge_accu);

/*
 * Conversion from charge accumulator to rho, with periodicity.
 * @param[in]  charge_accu[ncx] the charge MASS accumulator.
 * @param[in]  ncx mesh size on the x-axis.
 * @param[in]  factor is a number by which the whole array is multiplicated
 *             (done in this function to gain performance).
 * @param[out] rho[ncx+1], the charge density.
 */
void convert_charge_to_rho_1d_per_per(double* charge_accu,
        int num_threads, int ncx, double factor, double* rho);



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
 * @param[in]      mpi_world_size the number of MPI processors.
 * @param[in, out] send_buf[ncx*ncy], recv_buf[ncx*ncy] the MPI buffers.
 * @param[in]      ncx mesh size on the x-axis.
 * @param[in]      ncy mesh size on the y-axis.
 * @param[in, out] rho[ncx+1][ncy+1], the particle density.
 */
void mpi_reduce_rho_2d(int mpi_world_size, double* send_buf, double* recv_buf,
        int ncx, int ncy, double** rho);

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
        int ncx, int ncy, double** rho, MPI_Request* request_all_reduce);

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
        int ncx, int ncy, double** rho, MPI_Request* request_all_reduce);

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

enum CORNERS_2D {
    SOUTH_WEST,
    NORTH_WEST,
    SOUTH_EAST,
    NORTH_EAST,
    NB_CORNERS_2D // Always has to be last if you update this enum !
};

/*
 * Resets a charge accumulator.
 * @param[in]  ncx mesh size on the x-axis.
 * @param[in]  ncy mesh size on the y-axis.
 * @param[in]  num_threads number of threads.
 * @param[out] charge_accu[ncx*ncy*NB_CORNERS_2D*num_threads] the charge accumulator with 0s in every cell.
 */
void reset_charge_2d_accumulator(int ncx, int ncy,
        int num_threads, double* charge_accu);

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
        int num_threads, int ncx, int ncy, double factor, double** rho);




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
        int ncx, int ncy, int ncz, double*** rho);

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
        int ncx, int ncy, int ncz, double*** rho, MPI_Request* request_all_reduce);

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
        int ncx, int ncy, int ncz, double*** rho, MPI_Request* request_all_reduce);

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

enum CORNERS_3D {
    LEFT_FRONT_DOWN,
    LEFT_FRONT_TOP,
    LEFT_BACK_DOWN,
    LEFT_BACK_TOP,
    RIGHT_FRONT_DOWN,
    RIGHT_FRONT_TOP,
    RIGHT_BACK_DOWN,
    RIGHT_BACK_TOP,
    NB_CORNERS_3D // Always has to be last if you update this enum !
};

/*
 * Resets a charge accumulator.
 * @param[in]  ncx mesh size on the x-axis.
 * @param[in]  ncy mesh size on the y-axis.
 * @param[in]  ncz mesh size on the z-axis.
 * @param[in]  num_threads number of threads.
 * @param[out] charge_accu[ncx*ncy*ncz*NB_CORNERS_3D*num_threads] the charge accumulator with 0s in every cell.
 */
void reset_charge_3d_accumulator(int ncx, int ncy, int ncz,
        int num_threads, double* charge_accu);

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
        int ncx, int ncy, int ncz, double factor, double*** rho);

/*
 * Resets a charge accumulator.
 * @param[in]  ncx mesh size on the x-axis.
 * @param[in]  ncy mesh size on the y-axis.
 * @param[in]  ncz mesh size on the z-axis.
 * @param[in]  num_threads number of threads.
 * @param[out] charge_accu[num_threads][ncx*ncy*ncz][NB_CORNERS_3D] the charge accumulator with 0s in every cell.
 */
void reset_charge_3d_accumulator_bis(int ncx, int ncy, int ncz,
        int num_threads, double*** charge_accu);

/*
 * Conversion from charge accumulator to rho, with periodicity.
 * @param[in]  ncx mesh size on the x-axis.
 * @param[in]  ncy mesh size on the y-axis.
 * @param[in]  ncz mesh size on the y-axis.
 * @param[in]  charge_accu[ncx*ncy*ncz][NB_CORNERS_3D] the charge MASS accumulator.
 * @param[in]  factor is a number by which the whole array is multiplicated
 *             (done in this function to gain performance).
 * @param[out] rho[ncx+1][ncy+1][ncz+1], the charge density.
 */
void convert_charge_to_rho_3d_bis(int ncx, int ncy, int ncz,
        double** charge_accu, double factor, double*** rho);

#endif // ifndef PIC_VERT_RHO

