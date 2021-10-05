#ifndef PIC_VERT_HDF5_IO
#define PIC_VERT_HDF5_IO

#include "hdf5.h"     // type   hid_t
#include "meshes.h"   // type   cartesian_mesh_2d
#include "variadic.h" // macros VARIADIC, NUMARG4

/*
 * Create a new HDF5 file using the default properties.
 */
hid_t hdf5_ser_file_create(char* filename);

/*
 * Terminates access to an HDF5 file.
 */
void hdf5_ser_file_close(hid_t file_id);

/*
 * Write a 2d array of double in an HDF5 file.
 */
void hdf5_ser_write_dble_array_2d(hid_t file_id, hsize_t array_dims[2], const void* array, char* dsetname);

/*
 * Create the XML file and begin to write first lines.
 * You get the FILE*.
 */
FILE* xml_file_create(char* file_name);

/*
 * Write the description of a scalar field on a 2D mesh.
 * @param[in] output_file is the FILE* of your xml file
 * @param[in] filename is the file name where the heavy data are 
 * @param[in] nnodes_x1 - nodes number along direction 1
 * @param[in] nnodes_x2 - nodes number along direction 2
 *
 * The file named filename must exist.
 */
void xml_dataitem_2d(FILE* output_file, char* filename, int nnodes_x1, int nnodes_x2);

/*
 * Write the description of a 2D strutured grid
 * mesh with its nodes coordinates contains in filename-x1 and filename-x2.
 * @param[in] output_file is the FILE* of your xml file
 * @param[in] x1filename is the file name where the coordinates x1 are
 * @param[in] x2filename is the file name where the coordinates x2 are
 * @param[in] x1dsetname is the dataset name of coordinates x1 are
 * @param[in] x2dsetname is the dataset name of coordinates x2 are
 * @param[in] nnodes_x1 - nodes number along direction 1
 * @param[in] nnodes_x2 - nodes number along direction 2
 * @param[in] gridtype - "Uniform" or "Collection"
 *
 * The file named x*filename-x*dsetname.h5 with dataset x*dsetname must exist.
 * Low level version where you have to set dataset names in hdf5 files
 */
void xml_grid_geometry_2d_low_level(FILE* output_file, char* x1filename, int nnodes_x1,
        char* x2filename, int nnodes_x2, char* x1dsetname, char* x2dsetname, char* gridtype);

/*
 * Write the description of a 2D strutured grid
 * mesh with its nodes coordinates contains in mesh_name-x1 and mesh_name-x2.
 * @param[in] output_file is the FILE* or your xml file
 * @param[in] mesh_name is the file name where the coordinates data are
 * @param[in] nnodes_x1 - nodes number along direction 1
 * @param[in] nnodes_x2 - nodes number along direction 2
 *
 * The file named filename.h5 with dataset x1 and x2 must exist.
 */
void xml_grid_geometry_2d_high_level(FILE* output_file, char* mesh_name, int nnodes_x1, int nnodes_x2);

// Open a XDMF format file for a 2d plot
FILE* xdmf_open_2d(char* file_name, char* mesh_name, int nnodes_x, int nnodes_y);

/*
 * Write the description of a scalar field on a 2D mesh.
 * @param[in] output_file is the FILE* or your xml file
 * @param[in] fieldname the dataset name where the heavy data are 
 * @param[in] filename  the file name where the heavy data are 
 * @param[in] npoints_1 nodes or cells number along direction 1
 * @param[in] npoints_2 nodes or cells number along direction 2
 * @param[in] center    values are centered on nodes or cells 
 *
 * The file named filename.h5 with dataset fieldname must exist.
 */
void xml_field_2d(FILE* output_file, char* fieldname, char* filename,
        int npoints_1, int npoints_2, char* center);

/*
 * Write 2d array in binary or hdf5 file and the matching line in XDMF file
 * @param[in] center - "Node" or "Cell"
 */
void xdmf_array_2d(char* mesh_name, hsize_t array_dims[2], const void* array, char* array_name, FILE* xdmf_file, char* center);

// Close the XML file and finish to write last lines.
void xdmf_close(FILE** output_file);

// When calling with 3 arguments, the macro will set the fourth to "f".
#define plot_f_cartesian_mesh_2d_3(a, b, c   ) a, b, c, "f"
#define plot_f_cartesian_mesh_2d_4(a, b, c, d) a, b, c, d
#define plot_f_cartesian_mesh_2d(...) VARIADIC(plot_f_cartesian_mesh_2d, NUMARG4(__VA_ARGS__), __VA_ARGS__)

/*
 * Plot 2d distribution function for VisIt.
 * This routine will create a file named [array_name][iplot].xmf
 * Note: also calls the VARIADIC macro, but you wouldn't notice unless this portion of code doesn't compile :)
 *
 * @param[in] iplot   plot counter.
 * @param[in] f       function values.
 * @param[in] mesh_2d cartesian 2d mesh.
 * @param[in] array_name a name for the array. Optional, defaults to "f".
 */
void plot_f_cartesian_mesh_2d(int iplot, const void* f, cartesian_mesh_2d mesh_2d, char* array_name);

#endif // ifndef PIC_VERT_HDF5_IO

