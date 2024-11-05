#include <stdio.h>
#include <stdlib.h>

float ** genmat(float ** matrix, int matrix_size, int submatrix_size) {
  int null_entry, init_val = 1325;
  float * p;

  /* generating the structure */
  for (int ii = 0; ii < matrix_size; ii++) {
    for (int jj = 0; jj < matrix_size; jj++) {
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
        for (int i = 0; i < submatrix_size; i++) {
          for (int j = 0; j < submatrix_size; j++) {
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

float * allocate_clean_block(int submatrix_size) {
  float * p, * q;
  p = (float *) malloc(submatrix_size * submatrix_size * sizeof(float));
  q = p;

  if (p != NULL) {
    for (int i = 0; i < submatrix_size; i++) {
      for (int j = 0; j < submatrix_size; j++) {
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

void lu0(float * diag, int submatrix_size){
  for (int k = 0; k < submatrix_size; k++) {
    for (int i = k + 1; i < submatrix_size; i++) {
      diag[i * submatrix_size + k] = 
        diag[i * submatrix_size + k] / diag[k * submatrix_size + k];
      for (int j = k + 1; j < submatrix_size; j++) {
        diag[i * submatrix_size + j] =
          diag[i * submatrix_size + j] - diag[i * submatrix_size + k] *
          diag[k * submatrix_size + j];
      }
    }
  }
}

void bdiv(float * diag, float * row, int submatrix_size) {
  for (int i = 0; i < submatrix_size; i++) {
    for (int k = 0; k < submatrix_size; k++) {
      row[i * submatrix_size + k] =
        row[i * submatrix_size + k] / diag[k * submatrix_size + k];
      for (int j = k + 1; j < submatrix_size; j++) {
        row[i * submatrix_size + j] =
          row[i * submatrix_size + j] - row[i * submatrix_size + k] *
          diag[k * submatrix_size + j];
      }
    }
  }
}

void bmod(float * row, float * col, float * inner, int submatrix_size){
  for (int i = 0; i < submatrix_size; i++) {
    for (int j = 0; j < submatrix_size; j++) {
      for (int k = 0; k < submatrix_size; k++) {
        inner[i * submatrix_size + j] =
          inner[i * submatrix_size + j] - row[i * submatrix_size + k] *
          col[k * submatrix_size + j];
      }
    }
  }
}

void fwd(float * diag, float * col, int submatrix_size) {
  for (int j = 0; j < submatrix_size; j++) {
    for (int k = 0; k < submatrix_size; k++) { 
      for (int i = k + 1; i < submatrix_size; i++) {
        col[i * submatrix_size + j] =
          col[i * submatrix_size + j] - diag[i * submatrix_size + k] *
          col[k * submatrix_size + j];
      }
    }
  }
}

int sparselu(float ** matrix, int matrix_size, int submatrix_size) {
  for (int kk = 0; kk < matrix_size; kk++) {
    lu0(matrix[kk * matrix_size + kk], submatrix_size);
    for (int jj = kk + 1; jj < matrix_size; jj++) {
      if (matrix[kk * matrix_size + jj] != NULL) {
        fwd(
          matrix[kk * matrix_size + kk], matrix[kk * matrix_size + jj],
          submatrix_size
        );
      }
    }
    for (int ii = kk + 1; ii < matrix_size; ii++) {
      if (matrix[ii * matrix_size + kk] != NULL) {
        bdiv(
          matrix[kk * matrix_size + kk], matrix[ii * matrix_size + kk],
          submatrix_size
        );
      }
    }
    for (int ii = kk + 1; ii < matrix_size; ii++) {
      if (matrix[ii * matrix_size + kk] != NULL) {
        for (int jj = kk + 1; jj < matrix_size; jj++) {
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
  int matrix_size = 50, submatrix_size = 100;
  float ** matrix = NULL; int error = 0;

  if(argc > 2) {
    matrix_size = atoi(argv[1]);
    submatrix_size = atoi(argv[2]);
  }

  matrix =
    (float **) malloc((size_t) matrix_size * matrix_size * sizeof(float *));

  if (matrix == NULL) {
    fprintf(stderr, "Error: Failed to reserve memory for the matrix.\n");
    return 1;
  }

  matrix = genmat(matrix, matrix_size, submatrix_size);

  if (matrix == NULL) {
    fprintf(stderr, "Error: Failed to generate the matrix.\n");
    return 1;
  }
  
  error = sparselu(matrix, matrix_size, submatrix_size);

  if (error) {
    fprintf(stderr, "Error: Failed to perform the LU decomposition.\n");
    return 1;
  }

  for (int i = 0; i < matrix_size; i++) {
    if (matrix[i] != NULL) {
      free(matrix[i]);
    }
  }

  free(matrix);

  return 0;
}
