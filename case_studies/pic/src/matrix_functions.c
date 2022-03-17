#include <stdio.h>            // function  fprintf (output strings on a stream)
                              // constant  stderr (standard error output stream)
#include <stdlib.h>           // functions malloc, free ((de)allocate memory)
                              //           exit (error handling)
                              // constant  EXIT_FAILURE (error handling)
                              // type      size_t
#include <math.h>             // function  log10
#include "matrix_functions.h" // types     aligned_int_array, aligned_double_array_cell2d, aligned_double_array_cell3d

/*
 * Number of characters needed to print an integer.
 * @param[in] n is the integer to print.
 * @return    the number of characters needed to print n.
 */
static inline int length(int n) {
    if (n == 0)
        return 1;
    else if (n < 0)
        return (int)log10(-n) + 2;
    else
        return (int)log10(n) + 1;
}



/*****************************************************************************
 *                           Array functions                                 *
 *                               INT*                                        *
 *****************************************************************************/

/*
 * Array allocation - int* format.
 * @param[in] nbCell is the number of cells of the array to be allocated.
 * @return    a newly allocated nbCell array.
 */
int* allocate_int_array(int nbCell) {
    return malloc(nbCell * sizeof(int));
}

/*
 * Array deallocation - int* format.
 * @param[in, out] a is the nbCell array to deallocate.
 * @param[in]      nbCell is the number of cells of the array to be deallocated.
 */
void deallocate_int_array(int* a, int nbCell) {
    free(a);
}



/*****************************************************************************
 *                           Array functions                                 *
 *                              DOUBLE*                                      *
 *****************************************************************************/

/*
 * Array allocation - double* format.
 * @param[in] nbCell is the number of cells of the array to be allocated.
 * @return    a newly allocated nbCell array.
 */
double* allocate_array(int nbCell) {
    return malloc(nbCell * sizeof(double));
}

/*
 * Array deallocation - double* format.
 * @param[in, out] a is the nbCell array to deallocate.
 * @param[in]      nbCell is the number of cells of the array to be deallocated.
 */
void deallocate_array(double* a, int nbCell) {
    free(a);
}



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
int** allocate_int_matrix(int nbRow, int nbCol) {
    int** a = malloc(nbRow * sizeof(int*));
    if (!a) {
        fprintf(stderr, "allocate_int_matrix(%d, %d) : malloc error.\n", nbRow, nbCol);
        exit(EXIT_FAILURE);
    }
    for (size_t i = 0; i < nbRow; i++) {
        a[i] = malloc(nbCol * sizeof(int));
        if (!a[i]) {
            fprintf(stderr, "allocate_int_matrix(%d, %d) : malloc error.\n", nbRow, nbCol);
            exit(EXIT_FAILURE);
        }
    }
    return a;
}

/*
 * Matrix allocation - int** format.
 * Note: also calls the VARIADIC macro, but you wouldn't notice unless this portion of code doesn't compile :)
 *
 * @param[in] nbRow is the number of rows of the matrix to be allocated.
 * @param[in] nbCol is the number of cols of the matrix to be allocated.
 * @param[in] nbBytesAlignment is the number of bytes required for alignement. Optional, defaults to PIC_VERT_MAX_ALIGNMENT.
 * @return    a newly allocated (nbRow x nbCol) matrix, where each row is aligned on nbBytesAlignment.
 */
int** allocate_aligned_int_matrix(int nbRow, int nbCol, int nbBytesAlignment) {
    int** a = malloc(nbRow * sizeof(int*));
    if (!a) {
        fprintf(stderr, "allocate_aligned_int_matrix(%d, %d) : malloc error.\n", nbRow, nbCol);
        exit(EXIT_FAILURE);
    }
    for (size_t i = 0; i < nbRow; i++) {
        if (posix_memalign((void**)&a[i], nbBytesAlignment, nbCol * sizeof(int))) {
            fprintf(stderr, "allocate_aligned_int_matrix(%d, %d) : posix_memalign error.\n", nbRow, nbCol);
            exit(EXIT_FAILURE);
        }
    }
    return a;
}

/*
 * Matrix deallocation - int** format.
 * @param[in, out] a is the (nbRow x nbCol) matrix to deallocate.
 * @param[in]      nbRow is the number of rows of the matrix to be deallocated.
 * @param[in]      nbCol is the number of cols of the matrix to be deallocated.
 */
void deallocate_int_matrix(int** a, int nbRow, int nbCol) {
    for (size_t i = 0; i < nbRow; i++)
        free(a[i]);
    free(a);
}

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
void pretty_print_int_matrix_decalage(int nbRow, int nbCol, int** a, int decalage) {
    size_t i, j, k;
    int* maxLength = allocate_int_array(nbCol);
    int** lengthes = allocate_int_matrix(nbRow, nbCol);
    
    // memset(maxLength, 0, nbCol) doesn't work, it's not chars (works under Linux but not under Windows)
    for (j = 0; j < nbCol; j++)
        maxLength[j] = 0;
    
    for (j = 0; j < nbCol; j++)
        for (i = 0; i < nbRow; i++) {
            lengthes[i][j] = length(a[i][j]);
            if (lengthes[i][j] > maxLength[j])
                maxLength[j] = lengthes[i][j];
        }
    
    for (i = 0; i < nbRow; i++) {
        for (k = 0; k < decalage; k++)
            fprintf(stdout, " ");
        for (j = 0; j < nbCol; j++) {
            for (k = 0; k < maxLength[j] - lengthes[i][j]; k++)
                fprintf(stdout, " ");
            fprintf (stdout, "%d ", a[i][j]);
        }
        fprintf (stdout, "\n");
    }
    fprintf (stdout, "\n");
    deallocate_int_array(maxLength, nbCol);
    deallocate_int_matrix(lengthes, nbRow, nbCol);
}

/*
 * Matrix pretty printing - int** format.
 * @param[in] nbRow is the number of rows of the matrix to be printed.
 * @param[in] nbCol is the number of cols of the matrix to be printed.
 * @param[in] a is a (nbRow x nbCol) matrix.
 */
void pretty_print_int_matrix(int nbRow, int nbCol, int** a) {
    pretty_print_int_matrix_decalage(nbRow, nbCol, a, 0);
}



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
double** allocate_matrix(int nbRow, int nbCol) {
    double** a = malloc(nbRow * sizeof(double*));
    if (!a) {
        fprintf(stderr, "allocate_matrix(%d, %d) : malloc error.\n", nbRow, nbCol);
        exit(EXIT_FAILURE);
    }
    for (size_t i = 0; i < nbRow; i++) {
        a[i] = malloc(nbCol * sizeof(double));
        if (!a[i]) {
            fprintf(stderr, "allocate_matrix(%d, %d) : malloc error.\n", nbRow, nbCol);
            exit(EXIT_FAILURE);
        }
    }
    return a;
}

/*
 * Matrix allocation - double** format.
 * Note: also calls the VARIADIC macro, but you wouldn't notice unless this portion of code doesn't compile :)
 *
 * @param[in] nbRow is the number of rows of the matrix to be allocated.
 * @param[in] nbCol is the number of cols of the matrix to be allocated.
 * @param[in] nbBytesAlignment is the number of bytes required for alignement. Optional, defaults to PIC_VERT_MAX_ALIGNMENT.
 * @return    a newly allocated (nbRow x nbCol) matrix, where each row is aligned on nbBytesAlignment.
 */
double** allocate_aligned_double_matrix(int nbRow, int nbCol, int nbBytesAlignment) {
    double** a = malloc(nbRow * sizeof(double*));
    if (!a) {
        fprintf(stderr, "allocate_aligned_double_matrix(%d, %d) : malloc error.\n", nbRow, nbCol);
        exit(EXIT_FAILURE);
    }
    for (size_t i = 0; i < nbRow; i++) {
        if (posix_memalign((void**)&a[i], nbBytesAlignment, nbCol * sizeof(double))) {
            fprintf(stderr, "allocate_aligned_double_matrix(%d, %d) : posix_memalign error.\n", nbRow, nbCol);
            exit(EXIT_FAILURE);
        }
    }
    return a;
}

/*
 * Matrix deallocation - double** format.
 * @param[in, out] a is the (nbRow x nbCol) matrix to deallocate.
 * @param[in]      nbRow is the number of rows of the matrix to be deallocated.
 * @param[in]      nbCol is the number of cols of the matrix to be deallocated.
 */
void deallocate_matrix(double** a, int nbRow, int nbCol) {
    for (size_t i = 0; i < nbRow; i++)
        free(a[i]);
    free(a);
}

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
void pretty_print_matrix_decalage(int nbRow, int nbCol, double** a, int decalage) {
    size_t i, j, k;
    int* maxLength = allocate_int_array(nbCol);
    int** lengthes = allocate_int_matrix(nbRow, nbCol);
    
    // memset(maxLength, 0, nbCol) doesn't work, it's not chars (works under Linux but not under Windows)
    for (j = 0; j < nbCol; j++)
        maxLength[j] = 0;
    
    for (j = 0; j < nbCol; j++)
        for (i = 0; i < nbRow; i++) {
            lengthes[i][j] = length((int)a[i][j]);
            if (lengthes[i][j] > maxLength[j])
                maxLength[j] = lengthes[i][j];
        }
    
    for (i = 0; i < nbRow; i++) {
        for (k = 0; k < decalage; k++)
            fprintf(stdout, " ");
        for (j = 0; j < nbCol; j++) {
            for (k = 0; k < maxLength[j] - lengthes[i][j]; k++)
                fprintf(stdout, " ");
            fprintf (stdout, "%.3f ", a[i][j]);
        }
        fprintf (stdout, "\n");
    }
    fprintf (stdout, "\n");
    deallocate_int_array(maxLength, nbCol);
    deallocate_int_matrix(lengthes, nbRow, nbCol);
}

/*
 * Matrix pretty printing - double** format.
 * @param[in] nbRow is the number of rows of the matrix to be printed.
 * @param[in] nbCol is the number of cols of the matrix to be printed.
 * @param[in] a is a (nbRow x nbCol) matrix.
 */
void pretty_print_matrix(int nbRow, int nbCol, double** a) {
    pretty_print_matrix_decalage(nbRow, nbCol, a, 0);
}



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
double* allocateMatrix(int nbRow, int nbCol) {
    return malloc(nbRow * nbCol * sizeof(double));
}

/*
 * Matrix deallocation - double* format.
 * @param[in, out] a is the (nbRow x nbCol) matrix to deallocate.
 * @param[in]      nbRow is the number of rows of the matrix to be deallocated.
 * @param[in]      nbCol is the number of cols of the matrix to be deallocated.
 */
void deallocateMatrix(double* a, int nbRow, int nbCol) {
    free(a);
}

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
void prettyPrintMatrixDecalage(int nbRow, int nbCol, double* a, int decalage) {
    size_t i, j, k;
    int* maxLength = allocate_int_array(nbCol);
    int** lengthes = allocate_int_matrix(nbRow, nbCol);
    
    // memset(maxLength, 0, nbCol) doesn't work, it's not chars (works under Linux but not under Windows)
    for (j = 0; j < nbCol; j++)
        maxLength[j] = 0;
    
    for (j = 0; j < nbCol; j++)
        for (i = 0; i < nbRow; i++) {
            lengthes[i][j] = length((int)a[i * nbCol + j]);
            if (lengthes[i][j] > maxLength[j])
                maxLength[j] = lengthes[i][j];
        }
    
    for (i = 0; i < nbRow; i++) {
        for (k = 0; k < decalage; k++)
            fprintf(stdout, " ");
        for (j = 0; j < nbCol; j++) {
            for (k = 0; k < maxLength[j] - lengthes[i][j]; k++)
                fprintf(stdout, " ");
            fprintf (stdout, "%.3f ", a[i * nbCol + j]);
        }
        fprintf (stdout, "\n");
    }
    fprintf (stdout, "\n");
    deallocate_int_array(maxLength, nbCol);
    deallocate_int_matrix(lengthes, nbRow, nbCol);
}

/*
 * Matrix pretty printing - double* format.
 * @param[in] nbRow is the number of rows of the matrix to be printed.
 * @param[in] nbCol is the number of cols of the matrix to be printed.
 * @param[in] a is a (nbRow x nbCol) matrix.
 */
void prettyPrintMatrix(int nbRow, int nbCol, double* a) {
    prettyPrintMatrixDecalage(nbRow, nbCol, a, 0);
}



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
double*** allocate_3d_array(int nbRow, int nbCol, int nbRod) {
    double*** a = malloc(nbRow * sizeof(double**));
    if (!a) {
        fprintf(stderr, "allocate_3d_array(%d, %d, %d) : malloc error.\n", nbRow, nbCol, nbRod);
        exit(EXIT_FAILURE);
    }
    for (size_t i = 0; i < nbRow; i++) {
        a[i] = malloc(nbCol * sizeof(double*));
        if (!a[i]) {
            fprintf(stderr, "allocate_3d_array(%d, %d, %d) : malloc error.\n", nbRow, nbCol, nbRod);
            exit(EXIT_FAILURE);
        }
        for (size_t j = 0; j < nbCol; j++) {
            a[i][j] = malloc(nbRod * sizeof(double));
            if (!a[i][j]) {
                fprintf(stderr, "allocate_3d_array(%d, %d, %d) : malloc error.\n", nbRow, nbCol, nbRod);
                exit(EXIT_FAILURE);
            }
        }
    }
    return a;
}

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
double*** allocate_aligned_3d_array(int nbRow, int nbCol, int nbRod, int nbBytesAlignment) {
    double*** a = malloc(nbRow * sizeof(double**));
    if (!a) {
        fprintf(stderr, "allocate_aligned_3d_array(%d, %d, %d) : malloc error.\n", nbRow, nbCol, nbRod);
        exit(EXIT_FAILURE);
    }
    for (size_t i = 0; i < nbRow; i++) {
        a[i] = malloc(nbCol * sizeof(double*));
        if (!a[i]) {
            fprintf(stderr, "allocate_aligned_3d_array(%d, %d, %d) : malloc error.\n", nbRow, nbCol, nbRod);
            exit(EXIT_FAILURE);
        }
        for (size_t j = 0; j < nbCol; j++) {
            if (posix_memalign((void**)&a[i][j], nbBytesAlignment, nbRod * sizeof(double))) {
                fprintf(stderr, "allocate_aligned_3d_array(%d, %d, %d) : posix_memalign error.\n", nbRow, nbCol, nbRod);
                exit(EXIT_FAILURE);
            }
        }
    }
    return a;
}

/*
 * 3d-Array deallocation - double*** format.
 * @param[in, out] a is the (nbRow x nbCol x nbRod) 3d-Array to deallocate.
 * @param[in]      nbRow is the number of cells in the dimension 1 of the 3d-Array to be deallocated.
 * @param[in]      nbCol is the number of cells in the dimension 2 of the 3d-Array to be deallocated.
 * @param[in]      nbRod is the number of cells in the dimension 3 of the 3d-Array to be deallocated.
 */
void deallocate_3d_array(double*** a, int nbRow, int nbCol, int nbRod) {
    for (size_t i = 0; i < nbRow; i++) {
        for (size_t j = 0; j < nbCol; j++) {
            free(a[i][j]);
        }
        free(a[i]);
    }
    free(a);
}



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
double**** allocate_4d_array(int nb1, int nb2, int nb3, int nb4) {
    double**** a = malloc(nb1 * sizeof(double***));
    if (!a) {
        fprintf(stderr, "allocate_4d_array(%d, %d, %d, %d) : malloc error.\n", nb1, nb2, nb3, nb4);
        exit(EXIT_FAILURE);
    }
    for (size_t i = 0; i < nb1; i++) {
        a[i] = malloc(nb2 * sizeof(double**));
        if (!a[i]) {
            fprintf(stderr, "allocate_4d_array(%d, %d, %d, %d) : malloc error.\n", nb1, nb2, nb3, nb4);
            exit(EXIT_FAILURE);
        }
        for (size_t j = 0; j < nb2; j++) {
            a[i][j] = malloc(nb3 * sizeof(double*));
            if (!a[i][j]) {
                fprintf(stderr, "allocate_4d_array(%d, %d, %d, %d) : malloc error.\n", nb1, nb2, nb3, nb4);
                exit(EXIT_FAILURE);
            }
            for (size_t k = 0; k < nb3; k++) {
                a[i][j][k] = malloc(nb4 * sizeof(double));
                if (!a[i][j][k]) {
                    fprintf(stderr, "allocate_4d_array(%d, %d, %d, %d) : malloc error.\n", nb1, nb2, nb3, nb4);
                    exit(EXIT_FAILURE);
                }
            }
        }
    }
    return a;
}

/*
 * 4d-Array deallocation - double**** format.
 * @param[in, out] a is the (nb1 x nb2 x nb3 x nb4) 4d-Array to deallocate.
 * @param[in]      nb1 is the number of cells in the dimension 1 of the 4d-Array to be deallocated.
 * @param[in]      nb2 is the number of cells in the dimension 2 of the 4d-Array to be deallocated.
 * @param[in]      nb3 is the number of cells in the dimension 3 of the 4d-Array to be deallocated.
 * @param[in]      nb4 is the number of cells in the dimension 4 of the 4d-Array to be deallocated.
 */
void deallocate_4d_array(double**** a, int nb1, int nb2, int nb3, int nb4) {
    for (size_t i = 0; i < nb1; i++) {
        for (size_t j = 0; j < nb2; j++) {
            for (size_t k = 0; k < nb3; k++) {
                free(a[i][j][k]);
            }
            free(a[i][j]);
        }
        free(a[i]);
    }
    free(a);
}



/*****************************************************************************
 *                           Matrix functions                                *
 *                       struct { int array[]; }*                            *
 *****************************************************************************/

/*
 * Matrix allocation - struct { int array[]; }* format.
 * @param[in] nbCell is the number of rows of the matrix to be allocated.
 * @return    a newly allocated (nbCell x CHUNK_SIZE) matrix.
 */
aligned_int_array* allocate_aligned_int_array_array(int nbCell) {
    aligned_int_array* a;
    // For an unknown reason, the array have to be allocated aligned (despite the __attribute__((aligned(VEC_ALIGN)))
    // in the type declaration), else it causes a segfault because of unaligned memory if compiled with gcc.
    if (posix_memalign((void**)&a, VEC_ALIGN, nbCell * sizeof(aligned_int_array))) {
        fprintf(stderr, "allocate_aligned_int_array_array: posix_memalign failed to initialize memory.\n");
        exit(EXIT_FAILURE);
    }
    return a;
}

/*
 * Matrix deallocation - struct { int array[]; }* format.
 * @param[in, out] a is the (nbCell x CHUNK_SIZE) matrix to deallocate.
 * @param[in]      nbCell is the number of rows of the matrix to be deallocated.
 */
void deallocate_aligned_int_array_array(aligned_int_array* a, int nbCell) {
    free(a);
}

