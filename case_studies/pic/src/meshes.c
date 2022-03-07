#include <stdio.h>  // function  fprintf (output strings on a stream)
                    // constant  stderr (standard error output stream)
#include <stdlib.h> // functions exit (error handling)
                    // constant  EXIT_FAILURE (error handling)
#include "meshes.h" // types cartesian_mesh_1d, cartesian_mesh_2d, cartesian_mesh_3d

/*****************************************************************************
 *                            Spatial meshes                                 *
 *****************************************************************************/

/*
 * @param[in] num_cell_x the number of cells on the x-axis
 * @param[in] x_min the minimum value of x
 * @param[in] x_max the maximum value of x
 * @return    a mesh that has these fields.
 */
cartesian_mesh_1d create_mesh_1d(int num_cell_x, double x_min, double x_max) {
    cartesian_mesh_1d m;
    
    m.num_cell_x = num_cell_x;
    m.x_min      = x_min;
    m.x_max      = x_max;
    m.delta_x    = (x_max - x_min) / num_cell_x;
    return m;
}

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
        double x_min, double x_max, double y_min, double y_max) {
    cartesian_mesh_2d m;
    
    m.num_cell_x = num_cell_x;
    m.num_cell_y = num_cell_y;
    m.x_min      = x_min;
    m.x_max      = x_max;
    m.y_min      = y_min;
    m.y_max      = y_max;
    m.delta_x    = (x_max - x_min) / num_cell_x;
    m.delta_y    = (y_max - y_min) / num_cell_y;
    return m;
}

/*
 * @param[in] tile_size_x the size of the tiles on the x-axis
 * @param[in] tile_size_y the size of the tiles on the y-axis
 * @param[in] mesh_2d a 2d mesh.
 * @return    a tiled mesh which is derived from mesh_2d.
 */
tiled_cartesian_mesh_2d create_tiled_mesh_2d(int tile_size_x, int tile_size_y,
        cartesian_mesh_2d mesh_2d) {
    tiled_cartesian_mesh_2d m;
    
    if (tile_size_x < 1 || tile_size_y < 1) {
        fprintf(stderr, "tile_size_x and tile_size_y must be greater or equal to 1.\n");
        exit(EXIT_FAILURE);
    }
    m.tile_size_x = tile_size_x;
    m.tile_size_y = tile_size_y;
    m.num_tile_x  = (mesh_2d.num_cell_x + tile_size_x - 1) / tile_size_x;
    m.num_tile_y  = (mesh_2d.num_cell_y + tile_size_y - 1) / tile_size_y;
    m.mesh_2d     = mesh_2d;
    return m;
}

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
        double x_min, double x_max, double y_min, double y_max, double z_min, double z_max) {
    cartesian_mesh_3d m;
    
    m.num_cell_x = num_cell_x;
    m.num_cell_y = num_cell_y;
    m.num_cell_z = num_cell_z;
    m.x_min      = x_min;
    m.x_max      = x_max;
    m.y_min      = y_min;
    m.y_max      = y_max;
    m.z_min      = z_min;
    m.z_max      = z_max;
    m.delta_x    = (x_max - x_min) / num_cell_x;
    m.delta_y    = (y_max - y_min) / num_cell_y;
    m.delta_z    = (z_max - z_min) / num_cell_z;
    return m;
}

