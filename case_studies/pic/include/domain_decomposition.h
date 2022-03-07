#ifndef PIC_VERT_DOMAIN_DECOMPOSITION
#define PIC_VERT_DOMAIN_DECOMPOSITION

#include <stdbool.h> // type bool
#include "meshes.h"  // type cartesian_mesh_2d

/*****************************************************************************
 *                           Rectangular boxes                               *
 *****************************************************************************/

/*
 * Index limits: indicates for ex. the different points handled by a given
 * MPI processor, or the points overlapped with a neighbor...
 */
typedef struct box_2d {
    int i_min, i_max; // indexes for x
    int j_min, j_max; // indexes for y
} box_2d;

/*
 * Prints the limits of the box.
 *
 * @param box the box to print.
 */
void print_box_2d(box_2d box);

/*
 * Test if a box is empty.
 *
 * @param  box the box on which to check emptiness.
 * @return true iff box is empty.
 */
bool is_empty_box_2d(box_2d box);

/*
 * Return the number of different points in the box.
 *
 * @param  box the box on which to count the points.
 * @return the number of different points in the box.
 */
int number_points_box_2d(box_2d box);

/*
 * Intersects two boxes.
 *
 * @param[in]  b1, b2 : the two boxes to intersect.
 * @param[out] ans : the intersection of the two boxes (has to be initialized).
 * @return     false iff the intersection is empty.
 */
bool intersect_boxes_2d(box_2d b1, box_2d b2, box_2d* ans);



/*****************************************************************************
 *                           Parallel variables                              *
 *****************************************************************************/

/*
 *         y-axis
 *            ^        OVERLAP                             OVERLAP
 *            |       <———————>                           <———————>
 *            +       +———————+————————————————————————————+———————+ ^
 *            |       |       |                            |       | |
 *            |       |  N W  |            N M             |  N E  | | OVERLAP
 *            |       | (send)|           (send)           | (send)| |
 *      j_max +       |———————+————————————————————————————+———————| v
 *            |       |       |       !            !       |       |
 *            |       |       |  N W  !    N M     !  N E  |       |
 *            |       |       | (recv)!   (recv)   ! (recv)|       |
 *            |       |       |----------------------------|       |
 *            |       |       |       !            !       |       |
 *            |       |       |       !            !       |       |
 *            |       |  M W  |  M W  !This process!  M E  |  M E  |
 *            |       | (send)| (recv)!            ! (recv)| (send)|
 *            |       |       |       !            !       |       |
 *            |       |       |----------------------------|       |
 *            |       |       |       !            !       |       |
 *            |       |       |  S W  !            !  S E  |       |
 *            |       |       | (recv)!   (recv)   ! (recv)|       |
 *      j_min +       |———————+————————————————————————————+———————| ^
 *            |       |       |                            |       | |
 *            |       |  S W  |            S M             |  S E  | | OVERLAP
 *            |       | (send)|           (send)           | (send)| |
 *            +       +———————+————————————————————————————+———————+ v
 *            |
 *            |
 *            |
 *            +———————+———————+————————————————————————————+———————+————> x-axis
 *                           i_min                       i_max
 *
 * Each process handles the particles in a given sub-set of the physical space.
 * But it also keeps particles that leave a little bit this sub-set, to avoid unnecessary
 * communications of particles that could move back and forth the borders. This incurs
 * some overlapping of the sub-sets between processes. To compute rho, there is hence
 * the need to do a little communication between neighbors. The overlapped regions have
 * to be sent to the neighboring processes, and to be received from them.
 */

enum NEIGHBORS_2D {
    NEIGHBOR_NORTH_WEST,
    NEIGHBOR_NORTH_MIDL,
    NEIGHBOR_NORTH_EAST,
    NEIGHBOR_MIDDL_WEST,
    NEIGHBOR_MIDDL_EAST,
    NEIGHBOR_SOUTH_WEST,
    NEIGHBOR_SOUTH_MIDL,
    NEIGHBOR_SOUTH_EAST,
    NB_NEIGHBORS_2D // Always has to be last if you update this enum !
};

#define symmetric(neighbor) (NB_NEIGHBORS_2D - 1 - (neighbor))

#ifndef OVERLAP
#   define OVERLAP 4
#endif

#ifndef MARGIN
#   define MARGIN 6
#endif

typedef struct mpi_parameters {
    // Parallel architecture parameters
    int mpi_world_size; // number of MPI processes
    int mpi_rank;       // MPI rank of current process, in [0 ; world_size[
    
    // Physical parameters (physical space)
    cartesian_mesh_2d* spatial_mesh;
    
    // Spatial distribution rho
    int ncx_local;                      // Local ncx size
    int ncy_local;                      // Local ncy size
    double* rho_send_buf;               // array[ncx_local*ncy_local] for MPI_Allgatherv
    double* rho_recv_buf;               // array[ncx*ncy]             for MPI_Allgatherv
    int* recv_counts;                   // array[mpi_world_size]      for MPI_Allgatherv
    int* displs;                        // array[mpi_world_size]      for MPI_Allgatherv
    double** rho_2d;                    // array[ncx + 1][ncy + 1] corresponding to int_v f
    
    // Neighbor information for rho overlapping and particle exchange.
    int neighbors_ids[NB_NEIGHBORS_2D];         // neighbors_ids[i] is the mpi rank of the neighbor in direction i
    box_2d rho_send_box[NB_NEIGHBORS_2D];       // rho_send_box[i] is the subset of our    layout we send to   process neighbors_ids[i]
    box_2d rho_recv_box[NB_NEIGHBORS_2D];       // rho_recv_box[i] is the subset of global layout we recv from process neighbors_ids[i]
    int rho_boxes_sizes[NB_NEIGHBORS_2D];       // rho_boxes_sizes[i] = number_points_box_2d(rho_send_box[i])
                                                //                    = number_points_box_2d(rho_recv_bos[i]) thanks to symmetry.
    double* rho_send_bufs[NB_NEIGHBORS_2D];     // rho_send_bufs[i] is a buffer[rho_boxes_sizes[i]] for the send/recv of overlapped regions
    double* rho_recv_bufs[NB_NEIGHBORS_2D];     // rho_recv_bufs[i] is a buffer[rho_boxes_sizes[i]] for the send/recv of overlapped regions
    
    int num_overlapped_points;                  // Number  of     cells overlapped with our neighbors (outside of our managed rho_2d).
    int* overlapped_indices;                    // Indices of the cells overlapped with our neighbors (outside of our managed rho_2d).
    int send_area_sizes[NB_NEIGHBORS_2D];       // send_area_sizes[i]   is  the number  of     cells for which we send particles to process neighbors_ids[i]
    int* send_area_indices[NB_NEIGHBORS_2D];    // send_area_indices[i] are the indices of the cells for which we send particles to process neighbors_ids[i]
                                                // (they cannot be managed as boxes because in corners it is not a box)
    int num_particles_to_send[NB_NEIGHBORS_2D]; // Number of particles to send to   process neighbors_ids[i]
    int num_particles_to_recv[NB_NEIGHBORS_2D]; // Number of particles to recv from process neighbors_ids[i]
    int current_send_size[NB_NEIGHBORS_2D];     // Current size of the send buffers to   process neighbors_ids[i]
    int current_recv_size[NB_NEIGHBORS_2D];     // Current size of the recv buffers from process neighbors_ids[i]
    int*    ic_send[NB_NEIGHBORS_2D];           // Buffers for particles to be sent.
    float*  dx_send[NB_NEIGHBORS_2D];           // Buffers for particles to be sent.
    float*  dy_send[NB_NEIGHBORS_2D];           // Buffers for particles to be sent.
    double* vx_send[NB_NEIGHBORS_2D];           // Buffers for particles to be sent.
    double* vy_send[NB_NEIGHBORS_2D];           // Buffers for particles to be sent.
    int*    ic_recv[NB_NEIGHBORS_2D];           // Buffers for particles to be received.
    float*  dx_recv[NB_NEIGHBORS_2D];           // Buffers for particles to be received.
    float*  dy_recv[NB_NEIGHBORS_2D];           // Buffers for particles to be received.
    double* vx_recv[NB_NEIGHBORS_2D];           // Buffers for particles to be received.
    double* vy_recv[NB_NEIGHBORS_2D];           // Buffers for particles to be received.
    
    // How is the data spread among processors ?
    box_2d* layout_2d;                  // Portion of the full grid each processor has.
    int num_procs_x;                    // Number of procs on the x-axis
    int num_procs_y;                    // Number of procs on the y-axis
} mpi_parameters;

/*
 * Fill the automatic variables once the important ones have been fixed.
 */
void init_mpi_parameters(mpi_parameters* par_variables);

/*
 * Cuts the 2d mesh ncx x ncy into world_size boxes ; each MPI process will
 * handle one of those boxes.
 * Affect more MPI processes to the dimensions that have bigger number of points.
 *
 * Required fields : mpi_world_size, spatial_mesh.
 * Updated fields : num_procs_x, num_procs_y.
 * @param[in, out] par_variables
 */
void affect_processors_to_2d_mesh(mpi_parameters* par_variables);

/*
 * Auxiliary function that splits a range of indices described by 2 integers
 * into a given number of intervals.
 * The function tries to partition the original interval equitably.
 */
void split_array_indices_aux(int** intervals_array,
        int start_index, int interval_segment_length,
        int min, int max);

int** split_array_indices(int num_elements, int num_intervals);

/*
 * Creates the initial layout, depending on the number of processors available
 * and the number of cells in each direction.
 * TODO: load-balancing : split the full domain in subdomains on which the
 * integral of f is more or less constant. For small perturbations, we don't
 * care.
 *
 * Required fields : mpi_rank, spatial_mesh, num_procs_x, num_procs_y.
 * Updated fields : ncx_local, ncy_local.
 *
 * @param[in, out] par_variables
 */
void initialize_layout_with_distributed_2d_array(mpi_parameters* par_variables);



/*****************************************************************************
 *                                Update rho                                 *
 *****************************************************************************/

/*
 * Computes the integral of f on v.
 *
 * @param[in, out] par_variables
 */
void update_rho_2d(mpi_parameters* par_variables);

#endif // ifndef PIC_VERT_DOMAIN_DECOMPOSITION
