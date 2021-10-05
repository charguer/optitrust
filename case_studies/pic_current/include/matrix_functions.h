#ifndef PIC_VERT_MATRIX_FUNCTIONS
#define PIC_VERT_MATRIX_FUNCTIONS

#include "parameters.h" // constants PIC_VERT_MAX_ALIGNMENT, VEC_ALIGN, CHUNK_SIZE
#include "rho.h"        // constants NB_CORNERS_2D, NB_CORNERS_3D
#include "variadic.h"   // macros    VARIADIC, NUMARG32

/*****************************************************************************
 *                           Array functions                                 *
 *                               INT*                                        *
 *****************************************************************************/

/*
 * Array allocation - int* format.
 * @param[in] nbCell is the number of cells of the array to be allocated.
 * @return    a newly allocated nbCell array.
 */
int* allocate_int_array(int nbCell);

/*
 * Array deallocation - int* format.
 * @param[in, out] a is the nbCell array to deallocate.
 * @param[in]      nbCell is the number of cells of the array to be deallocated.
 */
void deallocate_int_array(int* a, int nbCell);



/*****************************************************************************
 *                           Array functions                                 *
 *                              DOUBLE*                                      *
 *****************************************************************************/

/*
 * Array allocation - double* format.
 * @param[in] nbCell is the number of cells of the array to be allocated.
 * @return    a newly allocated nbCell array.
 */
double* allocate_array(int nbCell);

/*
 * Array deallocation - double* format.
 * @param[in, out] a is the nbCell array to deallocate.
 * @param[in]      nbCell is the number of cells of the array to be deallocated.
 */
void deallocate_array(double* a, int nbCell);



/*****************************************************************************
 *                           Matrix functions                                *
 *                                INT**                                      *
 *****************************************************************************/

/*
 * Matrix allocation - int** format.
 * @param[in] nbRow is the number of rows of the matrix to be allocated.
 * @param[in] nbCol is the number of cols of the matrix to be allocated.
 * @return    a newly allocated (nbRow x nbCol) matrix.
 */
int** allocate_int_matrix(int nbRow, int nbCol);

// When calling with 2 arguments, the macro will set nbBytesAlignment to PIC_VERT_MAX_ALIGNMENT.
#define allocate_aligned_int_matrix_2(a, b   ) a, b, PIC_VERT_MAX_ALIGNMENT
#define allocate_aligned_int_matrix_3(a, b, c) a, b, c
#define allocate_aligned_int_matrix(...) VARIADIC(allocate_aligned_int_matrix, NUMARG32(__VA_ARGS__), __VA_ARGS__)

/*
 * Matrix allocation - int** format.
 * Note: also calls the VARIADIC macro, but you wouldn't notice unless this portion of code doesn't compile :)
 *
 * @param[in] nbRow is the number of rows of the matrix to be allocated.
 * @param[in] nbCol is the number of cols of the matrix to be allocated.
 * @param[in] nbBytesAlignment is the number of bytes required for alignement. Optional, defaults to PIC_VERT_MAX_ALIGNMENT.
 * @return    a newly allocated (nbRow x nbCol) matrix, where each row is aligned on nbBytesAlignment.
 */
int** allocate_aligned_int_matrix(int nbRow, int nbCol, int nbBytesAlignment);

/*
 * Matrix deallocation - int** format.
 * @param[in, out] a is the (nbRow x nbCol) matrix to deallocate.
 * @param[in]      nbRow is the number of rows of the matrix to be deallocated.
 * @param[in]      nbCol is the number of cols of the matrix to be deallocated.
 */
void deallocate_int_matrix(int** a, int nbRow, int nbCol);

/*
 * Matrix pretty printing - int** format.
 * @param[in] nbRow is the number of rows of the matrix to be printed.
 * @param[in] nbCol is the number of cols of the matrix to be printed.
 * @param[in] a is a (nbRow x nbCol) matrix.
 * @param[in] decalage is the number of spaces you want to add to each line
 *            before the printing. Useful if you want to print two matrices
 *            that will be multiplied, e.g. :
 *                1 2
 *                3 4
 *            5 6
 *            7 8
 */
void pretty_print_int_matrix_decalage(int nbRow, int nbCol, int** a, int decalage);

/*
 * Matrix pretty printing - int** format.
 * @param[in] nbRow is the number of rows of the matrix to be printed.
 * @param[in] nbCol is the number of cols of the matrix to be printed.
 * @param[in] a is a (nbRow x nbCol) matrix.
 */
void pretty_print_int_matrix(int nbRow, int nbCol, int** a);



/*****************************************************************************
 *                           Matrix functions                                *
 *                              DOUBLE**                                     *
 *****************************************************************************/

/*
 * Matrix allocation - double** format.
 * @param[in] nbRow is the number of rows of the matrix to be allocated.
 * @param[in] nbCol is the number of cols of the matrix to be allocated.
 * @return    a newly allocated (nbRow x nbCol) matrix.
 */
double** allocate_matrix(int nbRow, int nbCol);

// When calling with 2 arguments, the macro will set nbBytesAlignment to PIC_VERT_MAX_ALIGNMENT.
#define allocate_aligned_double_matrix_2(a, b   ) a, b, PIC_VERT_MAX_ALIGNMENT
#define allocate_aligned_double_matrix_3(a, b, c) a, b, c
#define allocate_aligned_double_matrix(...) VARIADIC(allocate_aligned_double_matrix, NUMARG32(__VA_ARGS__), __VA_ARGS__)

/*
 * Matrix allocation - double** format.
 * Note: also calls the VARIADIC macro, but you wouldn't notice unless this portion of code doesn't compile :)
 *
 * @param[in] nbRow is the number of rows of the matrix to be allocated.
 * @param[in] nbCol is the number of cols of the matrix to be allocated.
 * @param[in] nbBytesAlignment is the number of bytes required for alignement. Optional, defaults to PIC_VERT_MAX_ALIGNMENT.
 * @return    a newly allocated (nbRow x nbCol) matrix, where each row is aligned on nbBytesAlignment.
 */
double** allocate_aligned_double_matrix(int nbRow, int nbCol, int nbBytesAlignment);

/*
 * Matrix deallocation - double** format.
 * @param[in, out] a is the (nbRow x nbCol) matrix to deallocate.
 * @param[in]      nbRow is the number of rows of the matrix to be deallocated.
 * @param[in]      nbCol is the number of cols of the matrix to be deallocated.
 */
void deallocate_matrix(double** a, int nbRow, int nbCol);

/*
 * Matrix pretty printing - double** format.
 * @param[in] nbRow is the number of rows of the matrix to be printed.
 * @param[in] nbCol is the number of cols of the matrix to be printed.
 * @param[in] a is a (nbRow x nbCol) matrix.
 * @param[in] decalage is the number of spaces you want to add to each line
 *            before the printing. Useful if you want to print two matrices
 *            that will be multiplied, e.g. :
 *                1 2
 *                3 4
 *            5 6
 *            7 8
 */
void pretty_print_matrix_decalage(int nbRow, int nbCol, double** a, int decalage);

/*
 * Matrix pretty printing - double** format.
 * @param[in] nbRow is the number of rows of the matrix to be printed.
 * @param[in] nbCol is the number of cols of the matrix to be printed.
 * @param[in] a is a (nbRow x nbCol) matrix.
 */
void pretty_print_matrix(int nbRow, int nbCol, double** a);



/*****************************************************************************
 *                           Matrix functions                                *
 *                              DOUBLE*                                      *
 * In the following, the matrices are viewed as arrays of double.            *
 * On traditional integers this means they are linearized :                  *
 * the traditional a[i][j] is written instead a[i * nbCol + j]               *
 *****************************************************************************/

#define MATRIX_INDEX(matrix, i, j, nbCol) matrix[i * nbCol + j]

/*
 * Matrix allocation - double* format.
 * @param[in] nbRow is the number of rows of the matrix to be allocated.
 * @param[in] nbCol is the number of cols of the matrix to be allocated.
 * @return    a newly allocated (nbRow x nbCol) matrix.
 */
double* allocateMatrix(int nbRow, int nbCol);

/*
 * Matrix deallocation - double* format.
 * @param[in, out] a is the (nbRow x nbCol) matrix to deallocate.
 * @param[in]      nbRow is the number of rows of the matrix to be deallocated.
 * @param[in]      nbCol is the number of cols of the matrix to be deallocated.
 */
void deallocateMatrix(double* a, int nbRow, int nbCol);

/*
 * Matrix pretty printing - double* format.
 * @param[in] nbRow is the number of rows of the matrix to be printed.
 * @param[in] nbCol is the number of cols of the matrix to be printed.
 * @param[in] a is a (nbRow x nbCol) matrix.
 * @param[in] decalage is the number of spaces you want to add to each line
 *            before the printing. Useful if you want to print two matrices
 *            that will be multiplied, e.g. :
 *                1 2
 *                3 4
 *            5 6
 *            7 8
 */
void prettyPrintMatrixDecalage(int nbRow, int nbCol, double* a, int decalage);

/*
 * Matrix pretty printing - double* format.
 * @param[in] nbRow is the number of rows of the matrix to be printed.
 * @param[in] nbCol is the number of cols of the matrix to be printed.
 * @param[in] a is a (nbRow x nbCol) matrix.
 */
void prettyPrintMatrix(int nbRow, int nbCol, double* a);



/*****************************************************************************
 *                          3d-Array functions                               *
 *                              DOUBLE***                                    *
 *****************************************************************************/

/*
 * 3d-Array allocation - double*** format.
 * @param[in] nbRow is the number of cells in the dimension 1 of the 3d-Array to be allocated.
 * @param[in] nbCol is the number of cells in the dimension 2 of the 3d-Array to be allocated.
 * @param[in] nbRod is the number of cells in the dimension 3 of the 3d-Array to be allocated.
 * @return    a newly allocated (nbRow x nbCol x nbRod) 3d-Array.
 */
double*** allocate_3d_array(int nbRow, int nbCol, int nbRod);

// When calling with 3 arguments, the macro will set nbBytesAlignment to PIC_VERT_MAX_ALIGNMENT.
#define allocate_aligned_3d_array_3(a, b, c   ) a, b, c, PIC_VERT_MAX_ALIGNMENT
#define allocate_aligned_3d_array_4(a, b, c, d) a, b, c, d
#define allocate_aligned_3d_array(...) VARIADIC(allocate_aligned_3d_array, NUMARG32(__VA_ARGS__), __VA_ARGS__)

/*
 * 3d-Array allocation - double*** format.
 * Note: also calls the VARIADIC macro, but you wouldn't notice unless this portion of code doesn't compile :)
 *
 * @param[in] nbRow is the number of cells in the dimension 1 of the 3d-Array to be allocated.
 * @param[in] nbCol is the number of cells in the dimension 2 of the 3d-Array to be allocated.
 * @param[in] nbRod is the number of cells in the dimension 3 of the 3d-Array to be allocated.
 * @param[in] nbBytesAlignment is the number of bytes required for alignement. Optional, defaults to PIC_VERT_MAX_ALIGNMENT.
 * @return    a newly allocated (nbRow x nbCol x nbRod) 3d-Array, where each col is aligned on nbBytesAlignment.
 */
double*** allocate_aligned_3d_array(int nbRow, int nbCol, int nbRod, int nbBytesAlignment);

/*
 * 3d-Array deallocation - double*** format.
 * @param[in, out] a is the (nbRow x nbCol x nbRod) 3d-Array to deallocate.
 * @param[in]      nbRow is the number of cells in the dimension 1 of the 3d-Array to be deallocated.
 * @param[in]      nbCol is the number of cells in the dimension 2 of the 3d-Array to be deallocated.
 * @param[in]      nbRod is the number of cells in the dimension 3 of the 3d-Array to be deallocated.
 */
void deallocate_3d_array(double*** a, int nbRow, int nbCol, int nbRod);



/*****************************************************************************
 *                          4d-Array functions                               *
 *                              DOUBLE****                                   *
 *****************************************************************************/

/*
 * 4d-Array allocation - double**** format.
 * @param[in] nb1 is the number of cells in the dimension 1 of the 4d-Array to be allocated.
 * @param[in] nb2 is the number of cells in the dimension 2 of the 4d-Array to be allocated.
 * @param[in] nb3 is the number of cells in the dimension 3 of the 4d-Array to be allocated.
 * @param[in] nb4 is the number of cells in the dimension 4 of the 4d-Array to be allocated.
 * @return    a newly allocated (nb1 x nb2 x nb3 x nb4) 4d-Array.
 */
double**** allocate_4d_array(int nb1, int nb2, int nb3, int nb4);

/*
 * 4d-Array deallocation - double**** format.
 * @param[in, out] a is the (nb1 x nb2 x nb3 x nb4) 4d-Array to deallocate.
 * @param[in]      nb1 is the number of cells in the dimension 1 of the 4d-Array to be deallocated.
 * @param[in]      nb2 is the number of cells in the dimension 2 of the 4d-Array to be deallocated.
 * @param[in]      nb3 is the number of cells in the dimension 3 of the 4d-Array to be deallocated.
 * @param[in]      nb4 is the number of cells in the dimension 4 of the 4d-Array to be deallocated.
 */
void deallocate_4d_array(double**** a, int nb1, int nb2, int nb3, int nb4);



/*****************************************************************************
 *                           Matrix functions                                *
 *                       struct { int array[]; }*                            *
 *****************************************************************************/

typedef struct aligned_int_array {
    int array[CHUNK_SIZE] __attribute__((aligned(VEC_ALIGN)));
} aligned_int_array;

/*
 * Matrix allocation - struct { int array[]; }* format.
 * @param[in] nbCell is the number of rows of the matrix to be allocated.
 * @return    a newly allocated (nbCell x CHUNK_SIZE) matrix.
 */
aligned_int_array* allocate_aligned_int_array_array(int nbCell);

/*
 * Matrix deallocation - struct { int array[]; }* format.
 * @param[in, out] a is the (nbCell x CHUNK_SIZE) matrix to deallocate.
 * @param[in]      nbCell is the number of rows of the matrix to be deallocated.
 */
void deallocate_aligned_int_array_array(aligned_int_array* a, int nbCell);

#endif // ifndef PIC_VERT_MATRIX_FUNCTIONS

