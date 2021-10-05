#ifndef PIC_VERT_MESHES
#define PIC_VERT_MESHES

/*****************************************************************************
 *                            Spatial meshes                                 *
 *****************************************************************************/

typedef struct cartesian_mesh_1d cartesian_mesh_1d;
struct cartesian_mesh_1d {
    int num_cell_x; // number of cells on the x-axis
    double x_min;   // minimum value of x
    double x_max;   // maximum value of x
    double delta_x; // cell spacing on the x-axis
                    // (short for (x_max - x_min) / num_cell_x)
};

/*
 * @param[in] num_cell_x the number of cells on the x-axis
 * @param[in] x_min the minimum value of x
 * @param[in] x_max the maximum value of x
 * @return    a mesh that has these fields.
 */
cartesian_mesh_1d create_mesh_1d(int num_cell_x, double x_min, double x_max);

typedef struct cartesian_mesh_2d cartesian_mesh_2d;
struct cartesian_mesh_2d {
    int num_cell_x; // number of cells on the x-axis
    int num_cell_y; // number of cells on the y-axis
    double x_min;   // minimum value of x
    double x_max;   // maximum value of x
    double y_min;   // minimum value of y
    double y_max;   // maximum value of y
    double delta_x; // cell spacing on the x-axis
                    // (short for (x_max - x_min) / num_cell_x)
    double delta_y; // cell spacing on the y-axis
                    // (short for (y_max - y_min) / num_cell_y)
};

/*
 * @param[in] num_cell_x the number of cells on the x-axis
 * @param[in] num_cell_y the number of cells on the y-axis
 * @param[in] x_min the minimum value of x
 * @param[in] x_max the maximum value of x
 * @param[in] y_min the minimum value of y
 * @param[in] y_max the maximum value of y
 * @return    a mesh that has these fields.
 */
cartesian_mesh_2d create_mesh_2d(int num_cell_x, int num_cell_y,
        double x_min, double x_max, double y_min, double y_max);

typedef struct tiled_cartesian_mesh_2d tiled_cartesian_mesh_2d;
struct tiled_cartesian_mesh_2d {
    int tile_size_x;           // size of the tiles on the x-axis
    int tile_size_y;           // size of the tiles on the y-axis
    int num_tile_x;            // number of tiles on the x-axis
    int num_tile_y;            // number of tiles on the y-axis
    cartesian_mesh_2d mesh_2d; // the mesh to be tiled
};

/*
 * @param[in] tile_size_x the size of the tiles on the x-axis
 * @param[in] tile_size_y the size of the tiles on the y-axis
 * @param[in] mesh_2d a 2d mesh.
 * @return    a tiled mesh which is derived from mesh_2d.
 */
tiled_cartesian_mesh_2d create_tiled_mesh_2d(int tile_size_x, int tile_size_y,
        cartesian_mesh_2d mesh_2d);

typedef struct cartesian_mesh_3d cartesian_mesh_3d;
struct cartesian_mesh_3d {
    int num_cell_x; // number of cells on the x-axis
    int num_cell_y; // number of cells on the y-axis
    int num_cell_z; // number of cells on the y-axis
    double x_min;   // minimum value of x
    double x_max;   // maximum value of x
    double y_min;   // minimum value of y
    double y_max;   // maximum value of y
    double z_min;   // minimum value of z
    double z_max;   // maximum value of z
    double delta_x; // cell spacing on the x-axis
                    // (short for (x_max - x_min) / num_cell_x)
    double delta_y; // cell spacing on the y-axis
                    // (short for (y_max - y_min) / num_cell_y)
    double delta_z; // cell spacing on the z-axis
                    // (short for (z_max - z_min) / num_cell_z)
};

/*
 * @param[in] num_cell_x the number of cells on the x-axis
 * @param[in] num_cell_y the number of cells on the y-axis
 * @param[in] num_cell_z the number of cells on the z-axis
 * @param[in] x_min the minimum value of x
 * @param[in] x_max the maximum value of x
 * @param[in] y_min the minimum value of y
 * @param[in] y_max the maximum value of y
 * @param[in] z_min the minimum value of z
 * @param[in] z_max the maximum value of z
 * @return    a mesh that has these fields.
 */
cartesian_mesh_3d create_mesh_3d(int num_cell_x, int num_cell_y, int num_cell_z,
        double x_min, double x_max, double y_min, double y_max, double z_min, double z_max);

#endif // ifndef PIC_VERT_MESHES

