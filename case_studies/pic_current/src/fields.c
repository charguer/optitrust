#include <stdlib.h>               // functions malloc, free ((de)allocate memory)
#include "space_filling_curves.h" // macros    COMPUTE_I_CELL_2D, COMPUTE_I_CELL_3D, I_CELL_PARAM_2D, I_CELL_PARAM1_3D, I_CELL_PARAM2_3D
#include "fields.h"               // types     field_2d, field_2d_opt, field_3d, field_3d_opt

/*****************************************************************************
 *                           Electric field 2d                               *
 *                                                                           *
 * Code translated from Fortran from Selalib, in the following files :       *
 * selalib/src/particle_methods/pic_2d_standard/pic_utilities/               *
 *     sll_m_charge_to_density.F90                                           *
 * selalib/src/particle_methods/pic_2d_standard/pic_accumulators/            *
 *     sll_m_accumulators.F90                                                *
 *****************************************************************************/

/*
 * Creates a field accumulator.
 * @param[in] ncx mesh size on the x-axis.
 * @param[in] ncy mesh size on the y-axis.
 * @return    a [ncx*ncy] newly allocated field accumulator.
 */
field_2d create_field_2d(int ncx, int ncy) {
    field_2d f2d = malloc(ncx * ncy * sizeof(field_2d_cell));
    return f2d;
}

/*
 * Deallocates a field accumulator.
 * @param[in, out] f2d is the field accumulator to deallocate.
 */
void free_field_2d(field_2d f2d) {
    free(f2d);
}

/*
 * Sets the values of the field accumulator according to Ex and Ey.
 * @param[in]  Ex[ncx+1][ncy+1] the electric field on the x-axis.
 * @param[in]  Ey[ncx+1][ncy+1] the electric field on the y-axis.
 * @param[in]  ncx mesh size on the x-axis.
 * @param[in]  ncy mesh size on the y-axis.
 * @param[out] f2d the field accumulator updated.
 */
void accumulate_field_2d(double** Ex, double** Ey,
        int ncx, int ncy, double x_factor, double y_factor, field_2d f2d) {
    int i, j, i_cell;
    const int icell_param = I_CELL_PARAM_2D(ncx, ncy);

    // This loop doesn't cause arrayOutOfBoundsException because
    // Ex and Ey are (ncx+1) * (ncy+1) arrays.
    #pragma omp parallel for private(i, j, i_cell) firstprivate(icell_param, ncx, ncy, x_factor, y_factor) collapse(2)
    for (i = 0; i < ncx; i++) {
        for (j = 0; j < ncy; j++) {
            i_cell = COMPUTE_I_CELL_2D(icell_param, i, j);
            f2d[i_cell].field_x.south_west = Ex[i  ][j  ] * x_factor;
            f2d[i_cell].field_x.south_east = Ex[i+1][j  ] * x_factor;
            f2d[i_cell].field_x.north_west = Ex[i  ][j+1] * x_factor;
            f2d[i_cell].field_x.north_east = Ex[i+1][j+1] * x_factor;

            f2d[i_cell].field_y.south_west = Ey[i  ][j  ] * y_factor;
            f2d[i_cell].field_y.south_east = Ey[i+1][j  ] * y_factor;
            f2d[i_cell].field_y.north_west = Ey[i  ][j+1] * y_factor;
            f2d[i_cell].field_y.north_east = Ey[i+1][j+1] * y_factor;
        }
    }
}


/*
 * Creates a field accumulator.
 * @param[in] ncx mesh size on the x-axis.
 * @param[in] ncy mesh size on the y-axis.
 * @return    a [ncx*ncy] newly allocated field accumulator.
 */
field_2d_opt create_field_2d_opt(int ncx, int ncy) {
    field_2d_opt f2d = malloc(ncx * ncy * sizeof(field_2d_cell_opt));
    return f2d;
}

/*
 * Deallocates a field accumulator.
 * @param[in, out] f2d is the field accumulator to deallocate.
 */
void free_field_2d_opt(field_2d_opt f2d) {
    free(f2d);
}

/*
 * Sets the values of the field accumulator according to Ex and Ey.
 * @param[in]  Ex[ncx+1][ncy+1] the electric field on the x-axis.
 * @param[in]  Ey[ncx+1][ncy+1] the electric field on the y-axis.
 * @param[in]  ncx mesh size on the x-axis.
 * @param[in]  ncy mesh size on the y-axis.
 * @param[out] f2d the field accumulator updated.
 */
void accumulate_field_2d_opt(double** Ex, double** Ey,
        int ncx, int ncy, double x_factor, double y_factor, field_2d_opt f2d) {
    int i, j, i_cell;
    const int icell_param = I_CELL_PARAM_2D(ncx, ncy);

    // This loop doesn't cause arrayOutOfBoundsException because
    // Ex and Ey are (ncx+1) * (ncy+1) arrays.
    #pragma omp parallel for private(i, j, i_cell) firstprivate(icell_param, ncx, ncy, x_factor, y_factor) collapse(2)
    for (i = 0; i < ncx; i++) {
        for (j = 0; j < ncy; j++) {
            i_cell = COMPUTE_I_CELL_2D(icell_param, i, j);
            f2d[i_cell].field_x.constant_coeff =  Ex[i  ][j  ]                                       * x_factor;
            f2d[i_cell].field_x.dx_coeff       = (Ex[i+1][j  ] - Ex[i][j]                          ) * x_factor;
            f2d[i_cell].field_x.dy_coeff       = (Ex[i  ][j+1] - Ex[i][j]                          ) * x_factor;
            f2d[i_cell].field_x.dx_dy_coeff    = (Ex[i+1][j+1] + Ex[i][j] - Ex[i][j+1] - Ex[i+1][j]) * x_factor;

            f2d[i_cell].field_y.constant_coeff =  Ey[i  ][j  ]                                       * y_factor;
            f2d[i_cell].field_y.dx_coeff       = (Ey[i+1][j  ] - Ey[i][j]                          ) * y_factor;
            f2d[i_cell].field_y.dy_coeff       = (Ey[i  ][j+1] - Ey[i][j]                          ) * y_factor;
            f2d[i_cell].field_y.dx_dy_coeff    = (Ey[i+1][j+1] + Ey[i][j] - Ey[i][j+1] - Ey[i+1][j]) * y_factor;
        }
    }
}





/*
 * Creates a field accumulator.
 * @param[in] ncx mesh size on the x-axis.
 * @param[in] ncy mesh size on the y-axis.
 * @param[in] ncz mesh size on the z-axis.
 * @return    a [ncx*ncy*ncz] newly allocated field accumulator.
 */
field_3d create_field_3d(int ncx, int ncy, int ncz) {
    field_3d f3d = malloc(ncx * ncy * ncz * sizeof(field_3d_cell));
    return f3d;
}

/*
 * Deallocates a field accumulator.
 * @param[in, out] f3d is the field accumulator to deallocate.
 */
void free_field_3d(field_3d f3d) {
    free(f3d);
}

/*
 * Sets the values of the field accumulator according to Ex and Ey.
 * @param[in]  Ex[ncx+1][ncy+1][ncz+1] the electric field on the x-axis.
 * @param[in]  Ey[ncx+1][ncy+1][ncz+1] the electric field on the y-axis.
 * @param[in]  Ez[ncx+1][ncy+1][ncz+1] the electric field on the z-axis.
 * @param[in]  ncx mesh size on the x-axis.
 * @param[in]  ncy mesh size on the y-axis.
 * @param[in]  ncz mesh size on the z-axis.
 * @param[out] f3d the field accumulator updated.
 */
void accumulate_field_3d(double*** Ex, double*** Ey, double*** Ez,
        int ncx, int ncy, int ncz,
        double x_factor, double y_factor, double z_factor, field_3d f3d) {
    int i, j, k, i_cell;
    const int icell_param1 = I_CELL_PARAM1_3D(ncx, ncy, ncz);
    const int icell_param2 = I_CELL_PARAM2_3D(ncx, ncy, ncz);

    // This loop doesn't cause arrayOutOfBoundsException because
    // Ex, Ey and Ez are (ncx+1) * (ncy+1) * (ncz+1) arrays.
    #pragma omp parallel for private(i, j, k, i_cell) firstprivate(icell_param1, icell_param2, ncx, ncy, ncz, x_factor, y_factor, z_factor) collapse(3)
    for (i = 0; i < ncx; i++) {
        for (j = 0; j < ncy; j++) {
            for (k = 0; k < ncz; k++) {
                i_cell = COMPUTE_I_CELL_3D(icell_param1, icell_param2, i, j, k);
                f3d[i_cell].field_x.left_front_down  = Ex[i  ][j  ][k  ] * x_factor;
                f3d[i_cell].field_x.left_front_top   = Ex[i  ][j  ][k+1] * x_factor;
                f3d[i_cell].field_x.left_back_down   = Ex[i  ][j+1][k  ] * x_factor;
                f3d[i_cell].field_x.left_back_top    = Ex[i  ][j+1][k+1] * x_factor;
                f3d[i_cell].field_x.right_front_down = Ex[i+1][j  ][k  ] * x_factor;
                f3d[i_cell].field_x.right_front_top  = Ex[i+1][j  ][k+1] * x_factor;
                f3d[i_cell].field_x.right_back_down  = Ex[i+1][j+1][k  ] * x_factor;
                f3d[i_cell].field_x.right_back_top   = Ex[i+1][j+1][k+1] * x_factor;

                f3d[i_cell].field_y.left_front_down  = Ey[i  ][j  ][k  ] * y_factor;
                f3d[i_cell].field_y.left_front_top   = Ey[i  ][j  ][k+1] * y_factor;
                f3d[i_cell].field_y.left_back_down   = Ey[i  ][j+1][k  ] * y_factor;
                f3d[i_cell].field_y.left_back_top    = Ey[i  ][j+1][k+1] * y_factor;
                f3d[i_cell].field_y.right_front_down = Ey[i+1][j  ][k  ] * y_factor;
                f3d[i_cell].field_y.right_front_top  = Ey[i+1][j  ][k+1] * y_factor;
                f3d[i_cell].field_y.right_back_down  = Ey[i+1][j+1][k  ] * y_factor;
                f3d[i_cell].field_y.right_back_top   = Ey[i+1][j+1][k+1] * y_factor;

                f3d[i_cell].field_z.left_front_down  = Ez[i  ][j  ][k  ] * z_factor;
                f3d[i_cell].field_z.left_front_top   = Ez[i  ][j  ][k+1] * z_factor;
                f3d[i_cell].field_z.left_back_down   = Ez[i  ][j+1][k  ] * z_factor;
                f3d[i_cell].field_z.left_back_top    = Ez[i  ][j+1][k+1] * z_factor;
                f3d[i_cell].field_z.right_front_down = Ez[i+1][j  ][k  ] * z_factor;
                f3d[i_cell].field_z.right_front_top  = Ez[i+1][j  ][k+1] * z_factor;
                f3d[i_cell].field_z.right_back_down  = Ez[i+1][j+1][k  ] * z_factor;
                f3d[i_cell].field_z.right_back_top   = Ez[i+1][j+1][k+1] * z_factor;
            }
        }
    }
}


/*
 * Creates a field accumulator.
 * @param[in] ncx mesh size on the x-axis.
 * @param[in] ncy mesh size on the y-axis.
 * @param[in] ncz mesh size on the z-axis.
 * @return    a [ncx*ncy*ncz] newly allocated field accumulator.
 */
field_3d_opt create_field_3d_opt(int ncx, int ncy, int ncz) {
    field_3d_opt f3d = malloc(ncx * ncy * ncz * sizeof(field_3d_cell_opt));
    return f3d;
}

/*
 * Deallocates a field accumulator.
 * @param[in, out] f3d is the field accumulator to deallocate.
 */
void free_field_3d_opt(field_3d_opt f3d) {
    free(f3d);
}

/*
 * Sets the values of the field accumulator according to Ex and Ey.
 * @param[in]  Ex[ncx+1][ncy+1][ncz+1] the electric field on the x-axis.
 * @param[in]  Ey[ncx+1][ncy+1][ncz+1] the electric field on the y-axis.
 * @param[in]  Ez[ncx+1][ncy+1][ncz+1] the electric field on the z-axis.
 * @param[in]  ncx mesh size on the x-axis.
 * @param[in]  ncy mesh size on the y-axis.
 * @param[in]  ncz mesh size on the z-axis.
 * @param[out] f3d the field accumulator updated.
 */
void accumulate_field_3d_opt(double*** Ex, double*** Ey, double*** Ez,
        int ncx, int ncy, int ncz,
        double x_factor, double y_factor, double z_factor, field_3d_opt f3d) {
    int i, j, k, i_cell;
    const int icell_param1 = I_CELL_PARAM1_3D(ncx, ncy, ncz);
    const int icell_param2 = I_CELL_PARAM2_3D(ncx, ncy, ncz);

    // This loop doesn't cause arrayOutOfBoundsException because
    // Ex, Ey and Ez are (ncx+1) * (ncy+1) * (ncz+1) arrays.
    #pragma omp parallel for private(i, j, k, i_cell) firstprivate(icell_param1, icell_param2, ncx, ncy, ncz, x_factor, y_factor, z_factor) collapse(3)
    for (i = 0; i < ncx; i++) {
        for (j = 0; j < ncy; j++) {
            for (k = 0; k < ncz; k++) {
                i_cell = COMPUTE_I_CELL_3D(icell_param1, icell_param2, i, j, k);
                f3d[i_cell].field_x.constant_coeff =
                    Ex[i  ][j  ][k  ] * x_factor;
                f3d[i_cell].field_x.dx_coeff =
                    (Ex[i+1][j  ][k  ] - Ex[i  ][j  ][k  ]) * x_factor;
                f3d[i_cell].field_x.dy_coeff =
                    (Ex[i  ][j+1][k  ] - Ex[i  ][j  ][k  ]) * x_factor;
                f3d[i_cell].field_x.dz_coeff =
                    (Ex[i  ][j  ][k+1] - Ex[i  ][j  ][k  ]) * x_factor;
                f3d[i_cell].field_x.dx_dy_coeff =
                    (Ex[i+1][j+1][k  ] + Ex[i  ][j  ][k  ] - Ex[i  ][j+1][k  ] - Ex[i+1][j  ][k  ]) * x_factor;
                f3d[i_cell].field_x.dx_dz_coeff =
                    (Ex[i+1][j  ][k+1] + Ex[i  ][j  ][k  ] - Ex[i  ][j  ][k+1] - Ex[i+1][j  ][k  ]) * x_factor;
                f3d[i_cell].field_x.dy_dz_coeff =
                    (Ex[i  ][j+1][k+1] + Ex[i  ][j  ][k  ] - Ex[i  ][j  ][k+1] - Ex[i  ][j+1][k  ]) * x_factor;
                f3d[i_cell].field_x.dx_dy_dz_coeff =
                    (Ex[i+1][j+1][k+1] + Ex[i  ][j+1][k  ] + Ex[i  ][j  ][k+1] + Ex[i+1][j  ][k  ]
                   - Ex[i  ][j+1][k+1] - Ex[i+1][j  ][k+1] - Ex[i+1][j+1][k  ] - Ex[i  ][j  ][k  ]) * x_factor;

                f3d[i_cell].field_y.constant_coeff =
                    Ey[i  ][j  ][k  ] * y_factor;
                f3d[i_cell].field_y.dx_coeff =
                    (Ey[i+1][j  ][k  ] - Ey[i  ][j  ][k  ]) * y_factor;
                f3d[i_cell].field_y.dy_coeff =
                    (Ey[i  ][j+1][k  ] - Ey[i  ][j  ][k  ]) * y_factor;
                f3d[i_cell].field_y.dz_coeff =
                    (Ey[i  ][j  ][k+1] - Ey[i  ][j  ][k  ]) * y_factor;
                f3d[i_cell].field_y.dx_dy_coeff =
                    (Ey[i+1][j+1][k  ] + Ey[i  ][j  ][k  ] - Ey[i  ][j+1][k  ] - Ey[i+1][j  ][k  ]) * y_factor;
                f3d[i_cell].field_y.dx_dz_coeff =
                    (Ey[i+1][j  ][k+1] + Ey[i  ][j  ][k  ] - Ey[i  ][j  ][k+1] - Ey[i+1][j  ][k  ]) * y_factor;
                f3d[i_cell].field_y.dy_dz_coeff =
                    (Ey[i  ][j+1][k+1] + Ey[i  ][j  ][k  ] - Ey[i  ][j  ][k+1] - Ey[i  ][j+1][k  ]) * y_factor;
                f3d[i_cell].field_y.dx_dy_dz_coeff =
                    (Ey[i+1][j+1][k+1] + Ey[i  ][j+1][k  ] + Ey[i  ][j  ][k+1] + Ey[i+1][j  ][k  ]
                   - Ey[i  ][j+1][k+1] - Ey[i+1][j  ][k+1] - Ey[i+1][j+1][k  ] - Ey[i  ][j  ][k  ]) * y_factor;

                f3d[i_cell].field_z.constant_coeff =
                    Ez[i  ][j  ][k  ] * z_factor;
                f3d[i_cell].field_z.dx_coeff =
                    (Ez[i+1][j  ][k  ] - Ez[i  ][j  ][k  ]) * z_factor;
                f3d[i_cell].field_z.dy_coeff =
                    (Ez[i  ][j+1][k  ] - Ez[i  ][j  ][k  ]) * z_factor;
                f3d[i_cell].field_z.dz_coeff =
                    (Ez[i  ][j  ][k+1] - Ez[i  ][j  ][k  ]) * z_factor;
                f3d[i_cell].field_z.dx_dy_coeff =
                    (Ez[i+1][j+1][k  ] + Ez[i  ][j  ][k  ] - Ez[i  ][j+1][k  ] - Ez[i+1][j  ][k  ]) * z_factor;
                f3d[i_cell].field_z.dx_dz_coeff =
                    (Ez[i+1][j  ][k+1] + Ez[i  ][j  ][k  ] - Ez[i  ][j  ][k+1] - Ez[i+1][j  ][k  ]) * z_factor;
                f3d[i_cell].field_z.dy_dz_coeff =
                    (Ez[i  ][j+1][k+1] + Ez[i  ][j  ][k  ] - Ez[i  ][j  ][k+1] - Ez[i  ][j+1][k  ]) * z_factor;
                f3d[i_cell].field_z.dx_dy_dz_coeff =
                    (Ez[i+1][j+1][k+1] + Ez[i  ][j+1][k  ] + Ez[i  ][j  ][k+1] + Ez[i+1][j  ][k  ]
                   - Ez[i  ][j+1][k+1] - Ez[i+1][j  ][k+1] - Ez[i+1][j+1][k  ] - Ez[i  ][j  ][k  ]) * z_factor;
            }
        }
    }
}

