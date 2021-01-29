#ifndef PIC_VERT_FIELDS
#define PIC_VERT_FIELDS

/*****************************************************************************
 *                           Electric field 2d                               *
 *                                                                           *
 * Code translated from Fortran from Selalib, in the following files :       *
 * selalib/src/particle_methods/pic_2d_standard/pic_utilities/               *
 *     sll_m_charge_to_density.F90                                           *
 * selalib/src/particle_methods/pic_2d_standard/pic_accumulators/            *
 *     sll_m_accumulators.F90                                                *
 *****************************************************************************/

typedef struct field_2d_4corners_cell {
    double south_west;
    double south_east;
    double north_west;
    double north_east;
} field_2d_4corners_cell;

typedef struct field_2d_cell {
    field_2d_4corners_cell field_x;
    field_2d_4corners_cell field_y;
} field_2d_cell;

typedef field_2d_cell* field_2d;

/*
 * Creates a field accumulator.
 * @param[in] ncx mesh size on the x-axis.
 * @param[in] ncy mesh size on the y-axis.
 * @return    a [ncx*ncy] newly allocated field accumulator.
 */
field_2d create_field_2d(int ncx, int ncy);

/*
 * Deallocates a field accumulator.
 * @param[in, out] f2d is the field accumulator to deallocate.
 */
void free_field_2d(field_2d f2d);

/*
 * Sets the values of the field accumulator according to Ex and Ey.
 * @param[in]  Ex[ncx+1][ncy+1] the electric field on the x-axis.
 * @param[in]  Ey[ncx+1][ncy+1] the electric field on the y-axis.
 * @param[in]  ncx mesh size on the x-axis.
 * @param[in]  ncy mesh size on the y-axis.
 * @param[out] f2d the field accumulator updated.
 */
void accumulate_field_2d(double** Ex, double** Ey,
        int ncx, int ncy, double x_factor, double y_factor, field_2d f2d);


typedef struct field_2d_4corners_cell_opt {
    double constant_coeff;
    double dx_coeff;
    double dy_coeff;
    double dx_dy_coeff;
} field_2d_4corners_cell_opt;

typedef struct field_2d_cell_opt {
    field_2d_4corners_cell_opt field_x;
    field_2d_4corners_cell_opt field_y;
} field_2d_cell_opt;

typedef field_2d_cell_opt* field_2d_opt;

/*
 * Creates a field accumulator.
 * @param[in] ncx mesh size on the x-axis.
 * @param[in] ncy mesh size on the y-axis.
 * @return    a [ncx*ncy] newly allocated field accumulator.
 */
field_2d_opt create_field_2d_opt(int ncx, int ncy);

/*
 * Deallocates a field accumulator.
 * @param[in, out] f2d is the field accumulator to deallocate.
 */
void free_field_2d_opt(field_2d_opt f2d);

/*
 * Sets the values of the field accumulator according to Ex and Ey.
 * @param[in]  Ex[ncx+1][ncy+1] the electric field on the x-axis.
 * @param[in]  Ey[ncx+1][ncy+1] the electric field on the y-axis.
 * @param[in]  ncx mesh size on the x-axis.
 * @param[in]  ncy mesh size on the y-axis.
 * @param[out] f2d the field accumulator updated.
 */
void accumulate_field_2d_opt(double** Ex, double** Ey,
        int ncx, int ncy, double x_factor, double y_factor, field_2d_opt f2d);





typedef struct field_3d_8corners_cell {
    double left_front_down;
    double left_front_top;
    double left_back_down;
    double left_back_top;
    double right_front_down;
    double right_front_top;
    double right_back_down;
    double right_back_top;
} field_3d_8corners_cell;

typedef struct field_3d_cell {
    field_3d_8corners_cell field_x;
    field_3d_8corners_cell field_y;
    field_3d_8corners_cell field_z;
} field_3d_cell;

typedef field_3d_cell* field_3d;

/*
 * Creates a field accumulator.
 * @param[in] ncx mesh size on the x-axis.
 * @param[in] ncy mesh size on the y-axis.
 * @param[in] ncz mesh size on the z-axis.
 * @return    a [ncx*ncy*ncz] newly allocated field accumulator.
 */
field_3d create_field_3d(int ncx, int ncy, int ncz);

/*
 * Deallocates a field accumulator.
 * @param[in, out] f3d is the field accumulator to deallocate.
 */
void free_field_3d(field_3d f3d);

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
        double x_factor, double y_factor, double z_factor, field_3d f3d);


typedef struct field_3d_8corners_cell_opt {
    double constant_coeff;
    double dx_coeff;
    double dy_coeff;
    double dz_coeff;
    double dx_dy_coeff;
    double dx_dz_coeff;
    double dy_dz_coeff;
    double dx_dy_dz_coeff;
} field_3d_8corners_cell_opt;

typedef struct field_3d_cell_opt {
    field_3d_8corners_cell_opt field_x;
    field_3d_8corners_cell_opt field_y;
    field_3d_8corners_cell_opt field_z;
} field_3d_cell_opt;

typedef field_3d_cell_opt* field_3d_opt;

/*
 * Creates a field accumulator.
 * @param[in] ncx mesh size on the x-axis.
 * @param[in] ncy mesh size on the y-axis.
 * @param[in] ncz mesh size on the z-axis.
 * @return    a [ncx*ncy*ncz] newly allocated field accumulator.
 */
field_3d_opt create_field_3d_opt(int ncx, int ncy, int ncz);

/*
 * Deallocates a field accumulator.
 * @param[in, out] f3d is the field accumulator to deallocate.
 */
void free_field_3d_opt(field_3d_opt f3d);

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
        double x_factor, double y_factor, double z_factor, field_3d_opt f3d);

#endif // ifndef PIC_VERT_FIELDS

