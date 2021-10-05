#ifndef PIC_VERT_HDF5_IO
#define PIC_VERT_HDF5_IO

#include "compiler_test.h" // constant PIC_VERT_C11
#include "hdf5.h"          // type     hid_t
#include "meshes.h"        // type     cartesian_mesh_2d
#include "variadic.h"      // macros   VARIADIC, NUMARG32

/*
 * Create a new HDF5 file using the default properties.
 */
hid_t hdf5_ser_file_create(char* filename);

/*
 * Terminate access to an HDF5 file.
 */
void hdf5_ser_file_close(hid_t file_id);

/*
 * Write a 2d array of double in an HDF5 file.
 */
void hdf5_ser_write_dble_array_2d(hid_t file_id, hsize_t array_dims[2], const void* array, char* dsetname);

/*
 * Create the XML file and begin to write first lines.
 *
 * @return a pointer to the created file.
 */
FILE* xml_file_create(char* file_name);

/*
 * Write the description of a scalar field on a 2D mesh.
 *
 * The file named filename must exist.
 *
 * @param[in] output_file pointer to the xml file
 * @param[in] filename    file name where the heavy data are 
 * @param[in] nnodes_x1   number of nodes number along direction 1
 * @param[in] nnodes_x2   number of nodes number along direction 2
 */
void xml_dataitem_2d(FILE* output_file, char* filename, int nnodes_x1, int nnodes_x2);

/*
 * Write the description of a 2D strutured grid
 * mesh with its nodes coordinates contains in filename-x1 and filename-x2.
 *
 * The file named x*filename-x*dsetname.h5 with dataset x*dsetname must exist.
 * Low level version where you have to set dataset names in hdf5 files
 *
 * @param[in] output_file pointer to the xml file
 * @param[in] x1filename  file name where the coordinates x1 are
 * @param[in] x2filename  file name where the coordinates x2 are
 * @param[in] x1dsetname  dataset name of coordinates x1 are
 * @param[in] x2dsetname  dataset name of coordinates x2 are
 * @param[in] nnodes_x1   number of nodes number along direction 1
 * @param[in] nnodes_x2   number of nodes number along direction 2
 * @param[in] gridtype    "Uniform" or "Collection"
 */
void xml_grid_geometry_2d_low_level(FILE* output_file, char* x1filename, int nnodes_x1,
        char* x2filename, int nnodes_x2, char* x1dsetname, char* x2dsetname, char* gridtype);

/*
 * Write the description of a 2D strutured grid
 * mesh with its nodes coordinates contains in mesh_name-x1 and mesh_name-x2.
 *
 * The file named filename.h5 with dataset x1 and x2 must exist.
 *
 * @param[in] output_file pointer to the xml file
 * @param[in] mesh_name   file name where the coordinates data are
 * @param[in] nnodes_x1   number of nodes number along direction 1
 * @param[in] nnodes_x2   number of nodes number along direction 2
 */
void xml_grid_geometry_2d_high_level(FILE* output_file, char* mesh_name, int nnodes_x1, int nnodes_x2);

/*
 * Open an XDMF format file for a 2d plot.
 *
 * @param[in] mesh_name file name where the coordinates data are
 * @param[in] nnodes_x1 number of nodes number along direction 1
 * @param[in] nnodes_x2 number of nodes number along direction 2
 */
FILE* xdmf_open_2d(char* file_name, char* mesh_name, int nnodes_x1, int nnodes_x2);

/*
 * Write the description of a scalar field on a 2D mesh.
 *
 * The file named filename.h5 with dataset fieldname must exist.
 *
 * @param[in] output_file pointer to the xml file
 * @param[in] fieldname   dataset name where the heavy data are 
 * @param[in] filename    file name where the heavy data are 
 * @param[in] npoints_1   number of nodes or cells number along direction 1
 * @param[in] npoints_2   number of nodes or cells number along direction 2
 * @param[in] center      values are centered on ("Node" or "Cell")
 */
void xml_field_2d(FILE* output_file, char* fieldname, char* filename,
        int npoints_1, int npoints_2, char* center);

/*
 * Write 2d array in hdf5 file and the matching line in an XDMF file.
 *
 * @param[in] array_dims dimensions of the array to write
 * @param[in] array      array to write
 * @param[in] array_name name of the array
 * @param[in] xdmf_file  pointer to the xml file
 * @param[in] center     values are centered on ("Node" or "Cell")
 */
void xdmf_array_2d(char* mesh_name, hsize_t array_dims[2], const void* array,
        char* array_name, FILE* xdmf_file, char* center);

/*
 * Close the XML file and finish to write last lines.
 *
 * @param[in, out] xdmf_file, the FILE* or your xml file
 */
void xdmf_close(FILE** xdmf_file);

/*
 * Add the the good value of time in VisIt plot.
 *
 * @param[in] xdmf_file, the FILE* or your xml file
 * @param[in] time, the input time
 */
void xdmf_set_time(FILE* xdmf_file, double time);

// When calling with 3 arguments, the macro will set time to -1 and array_name to "f".
// There is a ambiguity with 4 arguments. Either time or array_name is missing. This
// is checked by the type of the 4th argument (that should be either double either char*).
// WARNING: This check needs C11 support from your compiler (_Generic). If your compiler
// does not support C11, you can use instead the P99 macros from Jens Gustedt:
// https://gustedt.wordpress.com/2012/01/02/emulating-c11-compiler-features-with-gcc-_generic/
// But the easiest choice is maybe to fill all the arguments of the function when you call it.
#define plot_f_cartesian_mesh_2d_3(a, b, c      ) a, b, c, -1, "f"
#if defined(PIC_VERT_C11)
#define plot_f_cartesian_mesh_2d_4(a, b, c, d   ) a, b, c, \
  _Generic((d), char*: -1, default: d), \
  _Generic((d), char*:  d, default: "f")
#endif
#define plot_f_cartesian_mesh_2d_5(a, b, c, d, e) a, b, c, d, e
#define plot_f_cartesian_mesh_2d(...) VARIADIC(plot_f_cartesian_mesh_2d, NUMARG32(__VA_ARGS__), __VA_ARGS__)

/*
 * Plot 2d distribution function for VisIt.
 * This routine will create a file named [array_name][iplot].xmf
 * Note: also calls the VARIADIC macro, but you wouldn't notice unless this portion of code doesn't compile :)
 *
 * @param[in] iplot   plot counter.
 * @param[in] f       function values.
 * @param[in] mesh_2d cartesian 2d mesh.
 * @param[in] time the plot time. Optional, defaults to -1.
 * @param[in] array_name a name for the array. Optional, defaults to "f".
 */
void plot_f_cartesian_mesh_2d(int iplot, const void* f, cartesian_mesh_2d mesh_2d,
        double time, char* array_name);

#endif // ifndef PIC_VERT_HDF5_IO

