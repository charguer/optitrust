#include <stdio.h>
#include <stdlib.h>
#include <string.h>

float ** genmat(float ** matrix, size_t matrix_size, size_t submatrix_size) {
  int null_entry, init_val = 1325;
  float * p;

  /* generating the structure */
  for (size_t ii = 0; ii < matrix_size; ii++) {
    for (size_t jj = 0; jj < matrix_size; jj++) {
      /* computing null entries */
      null_entry = 0;
      if ((ii < jj) && (ii % 3 != 0)) null_entry = 1;
      if ((ii > jj) && (jj % 3 != 0)) null_entry = 1;
      if (ii % 2 == 1) null_entry = 1;
      if (jj % 2 == 1) null_entry = 1;
      if (ii == jj) null_entry = 0;
      if (ii == jj - 1) null_entry = 0;
      if (ii - 1 == jj) null_entry = 0; 

      /* allocating matrix */
      if (!null_entry) {
        matrix[ii * matrix_size + jj] =
          (float *) malloc(
            submatrix_size * submatrix_size * sizeof(float)
          );
            
        if (matrix[ii * matrix_size + jj] == NULL) return NULL;

        /* initializing matrix */
        p = matrix[ii * matrix_size + jj];
        for (size_t i = 0; i < submatrix_size; i++) {
          for (size_t j = 0; j < submatrix_size; j++) {
            init_val = (3125 * init_val) % 65536;
            (*p) = (float) ((init_val - 32768.0) / 16384.0);
            p++;
          }
        }
      } else {
        matrix[ii * matrix_size + jj] = NULL;
      }
    }
  }
  return matrix;
}

int store_structure(
  const char * output, const char * name, float ** matrix, size_t matrix_size
) {
  FILE * file = fopen(output, "w");
  if (file == NULL) {
    fprintf(stderr, "Error: Failed to open the file for writing.\n");
    return 1;
  }

  int written;

  for (size_t ii = 0; ii < matrix_size; ii++) {
    for (size_t jj = 0; jj < matrix_size; jj++) {
      if (matrix[ii * matrix_size+jj] != NULL) {
        written = fprintf(file, "x");
        if (written < 0) {
          fprintf(stderr, "Error: Failed to write to the file.\n");
          fclose(file);
          return 1;
        }
      } else {
        written = fprintf(file, " ");
        if (written < 0) {
          fprintf(stderr, "Error: Failed to write to the file.\n");
          fclose(file);
          return 1;
        }
      }
    }

    written = fprintf(file, "\n");
    if (written < 0) {
      fprintf(stderr, "Error: Failed to write to the file.\n");
      fclose(file);
      return 1;
    }
  }

  written = fprintf(file, "\n");
  if (written < 0) {
    fprintf(stderr, "Error: Failed to write to the file.\n");
    fclose(file);
    return 1;
  }

  fclose(file);
  return 0;
}

int store_matrix(
  const char * output, const char * name,
  float ** matrix, size_t matrix_size, size_t submatrix_size
) {
  FILE * file = fopen(output, "w");
  if (file == NULL) {
    fprintf(stderr, "Error: Failed to open the file for writing.\n");
    return 1;
  }

  float * p; int written;

  for (size_t ii = 0; ii < matrix_size; ii++) {
    for (size_t jj = 0; jj < matrix_size; jj++) {
      if (matrix[ii * matrix_size + jj] != NULL) {
        for (size_t i = 0; i < submatrix_size; i++) {
          p = matrix[ii * matrix_size + jj] + i * submatrix_size;
          for (size_t j = 0; j < submatrix_size; j++) {
            written = fprintf(
              file, "%s[%lu][%lu] = %f\n", name,
              ii * submatrix_size + i, jj * submatrix_size + j,
              *p
            );
            if (written < 0) {
              fprintf(stderr, "Error: Failed to write to the file.\n");
              fclose(file);
              return 1;
            }
            p++;
          }
        }
      }
    }
  }

  fclose(file);
  return 0;
}

float * allocate_clean_block(size_t submatrix_size) {
  float * p, * q;
  p = (float *) malloc(submatrix_size * submatrix_size * sizeof(float));
  q = p;

  if (p != NULL) {
    for (size_t i = 0; i < submatrix_size; i++) {
      for (size_t j = 0; j < submatrix_size; j++) {
        (*p) = 0.0;
        p++;
      }
    }
  } else {
    fprintf(stderr, "Error: Failed to allocate memory for a block.\n");
    return NULL;
  }

  return q;
}

void lu0(float * diag, size_t submatrix_size){
  for (size_t k = 0; k < submatrix_size; k++) {
    for (size_t i = k + 1; i < submatrix_size; i++) {
      diag[i * submatrix_size + k] = 
        diag[i * submatrix_size + k] / diag[k * submatrix_size + k];
      for (size_t j = k + 1; j < submatrix_size; j++) {
        diag[i * submatrix_size + j] =
          diag[i * submatrix_size + j] - diag[i * submatrix_size + k] *
          diag[k * submatrix_size + j];
      }
    }
  }
}

void bdiv(float * diag, float * row, size_t submatrix_size) {
  for (size_t i = 0; i < submatrix_size; i++) {
    for (size_t k = 0; k < submatrix_size; k++) {
      row[i * submatrix_size + k] =
        row[i * submatrix_size + k] / diag[k * submatrix_size + k];
      for (size_t j = k + 1; j < submatrix_size; j++) {
        row[i * submatrix_size + j] =
          row[i * submatrix_size + j] - row[i * submatrix_size + k] *
          diag[k * submatrix_size + j];
      }
    }
  }
}

void bmod(float * row, float * col, float * inner, size_t submatrix_size){
  for (size_t i = 0; i < submatrix_size; i++) {
    for (size_t j = 0; j < submatrix_size; j++) {
      for (size_t k = 0; k < submatrix_size; k++) {
        inner[i * submatrix_size + j] =
          inner[i * submatrix_size + j] - row[i * submatrix_size + k] *
          col[k * submatrix_size + j];
      }
    }
  }
}

void fwd(float * diag, float * col, size_t submatrix_size) {
  for (size_t j = 0; j < submatrix_size; j++) {
    for (size_t k = 0; k < submatrix_size; k++) { 
      for (size_t i = k + 1; i < submatrix_size; i++) {
        col[i * submatrix_size + j] =
          col[i * submatrix_size + j] - diag[i * submatrix_size + k] *
          col[k * submatrix_size + j];
      }
    }
  }
}

int sparselu(float ** matrix, size_t matrix_size, size_t submatrix_size) {
  for (size_t kk = 0; kk < matrix_size; kk++) {
    lu0(matrix[kk * matrix_size + kk], submatrix_size);
    for (size_t jj = kk + 1; jj < matrix_size; jj++) {
      if (matrix[kk * matrix_size + jj] != NULL) {
        fwd(
          matrix[kk * matrix_size + kk], matrix[kk * matrix_size + jj],
          submatrix_size
        );
      }
    }
    for (size_t ii = kk + 1; ii < matrix_size; ii++) {
      if (matrix[ii * matrix_size + kk] != NULL) {
        bdiv(
          matrix[kk * matrix_size + kk], matrix[ii * matrix_size + kk],
          submatrix_size
        );
      }
    }
    for (size_t ii = kk + 1; ii < matrix_size; ii++) {
      if (matrix[ii * matrix_size + kk] != NULL) {
        for (size_t jj = kk + 1; jj < matrix_size; jj++) {
          if (matrix[kk * matrix_size + jj] != NULL) {
            matrix[ii * matrix_size + jj] = 
              !matrix[ii * matrix_size + jj] ? 
                allocate_clean_block(submatrix_size) : 
                  matrix[ii * matrix_size + jj];
            bmod(
              matrix[ii * matrix_size + kk], matrix[kk * matrix_size + jj],
              matrix[ii * matrix_size + jj], submatrix_size
            );
          }
        }
      }
    }
  }
  return 0;
}

int main(int argc, char ** argv) {
  size_t matrix_size = 50LU, submatrix_size = 100LU;
  char * struct_A = NULL, * struct_LU = NULL;
  char * matrix_A = NULL, * matrix_LU = NULL;
  int need_free = 0; float ** matrix = NULL;
  int error = 0;

  if(argc > 6) {
    matrix_size = strtoul(argv[1], NULL, 0);
    submatrix_size = strtoul(argv[2], NULL, 0);
    struct_A = argv[3];
    struct_LU = argv[4];
    matrix_A = argv[5];
    matrix_LU = argv[6];
  } else {
    struct_A = (char *) malloc(9 * sizeof(char));
    struct_LU = (char *) malloc(10 * sizeof(char));
    matrix_A = (char *) malloc(9 * sizeof(char));
    matrix_LU = (char *) malloc(10 * sizeof(char));
    if (
      struct_A == NULL || struct_LU == NULL || 
      matrix_A == NULL || matrix_LU == NULL
    ) {
      fprintf(stderr, "Error: Failed to reserve memory for the file names.\n");
      return 1;
    }
    struct_A = strncpy(struct_A, "A.struct", 9);
    struct_LU = strncpy(struct_LU, "LU.struct", 10);
    matrix_A = strncpy(matrix_A, "A.matrix", 9);
    matrix_LU = strncpy(matrix_LU, "LU.matrix", 10);
    need_free = 1;
  }

  matrix = (float **) malloc(matrix_size * matrix_size * sizeof(float *));

  if (matrix == NULL) {
    fprintf(stderr, "Error: Failed to reserve memory for the matrix.\n");
    return 1;
  }

  matrix = genmat(matrix, matrix_size, submatrix_size);

  if (matrix == NULL) {
    fprintf(stderr, "Error: Failed to generate the matrix.\n");
    return 1;
  }

  error = store_structure(struct_A, "A", matrix, matrix_size);

  if (error) {
    fprintf(stderr, "Error: Failed to store the structure of the matrix.\n");
    return 1;
  }

  error = store_matrix(matrix_A, "A", matrix, matrix_size, submatrix_size);

  if (error) {
    fprintf(stderr, "Error: Failed to store the matrix.\n");
    return 1;
  }
  
  error = sparselu(matrix, matrix_size, submatrix_size);

  if (error) {
    fprintf(stderr, "Error: Failed to perform the LU decomposition.\n");
    return 1;
  }
  
  error = store_structure(struct_LU, "LU", matrix, matrix_size);

  if (error) {
    fprintf(stderr, "Error: Failed to store the structure of the LU matrix.\n");
    return 1;
  }

  error = store_matrix(matrix_LU, "LU", matrix, matrix_size, submatrix_size);

  if (error) {
    fprintf(stderr, "Error: Failed to store the LU matrix.\n");
    return 1;
  }

  if (need_free) {
    free(struct_A);
    free(struct_LU);
    free(matrix_A);
    free(matrix_LU);
  }

  return 0;
}
