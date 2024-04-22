#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "tools.hpp"

float* allocate_clean_block(size_t submatrix_size) {
  float *p, *q;
  p = (float*)malloc(submatrix_size * submatrix_size * sizeof(float));
  q = p;
  if (p != NULL) {
    for (int i = 0; i < submatrix_size; i++) {
      for (int j = 0; j < submatrix_size; j++) {
        *p = 0.f;
        p++;
      }
    }
  } else {
    fprintf(stderr, "Error: Failed to allocate memory for a block.\n");
    return NULL;
  }
  return q;
}

void lu0(float* diag, size_t submatrix_size) {
  for (int k = 0; k < submatrix_size; k++) {
    for (int i = k + 1; i < submatrix_size; i++) {
      diag[i * submatrix_size + k] = diag[i * submatrix_size + k] / diag[k * submatrix_size + k];
      for (int j = k + 1; j < submatrix_size; j++) {
        diag[i * submatrix_size + j] = diag[i * submatrix_size + j] - diag[i * submatrix_size + k] * diag[k * submatrix_size + j];
      }
    }
  }
}

void bdiv(float* diag, float* row, size_t submatrix_size) {
  for (int i = 0; i < submatrix_size; i++) {
    for (int k = 0; k < submatrix_size; k++) {
      row[i * submatrix_size + k] = row[i * submatrix_size + k] / diag[k * submatrix_size + k];
      for (int j = k + 1; j < submatrix_size; j++) {
        row[i * submatrix_size + j] = row[i * submatrix_size + j] - row[i * submatrix_size + k] * diag[k * submatrix_size + j];
      }
    }
  }
}

void bmod(float* row, float* col, float* inner, size_t submatrix_size) {
  for (int i = 0; i < submatrix_size; i++) {
    for (int j = 0; j < submatrix_size; j++) {
      for (int k = 0; k < submatrix_size; k++) {
        inner[i * submatrix_size + j] = inner[i * submatrix_size + j] - row[i * submatrix_size + k] * col[k * submatrix_size + j];
      }
    }
  }
}

void fwd(float* diag, float* col, size_t submatrix_size) {
  for (int j = 0; j < submatrix_size; j++) {
    for (int k = 0; k < submatrix_size; k++) {
      for (int i = k + 1; i < submatrix_size; i++) {
        col[i * submatrix_size + j] = col[i * submatrix_size + j] - diag[i * submatrix_size + k] * col[k * submatrix_size + j];
      }
    }
  }
}

int sparselu(float** matrix, const size_t matrix_size, const size_t submatrix_size) {
  int __apac_result;
#pragma omp taskgroup
  {
#pragma omp taskwait depend(in : matrix_size)
#pragma omp taskwait depend(in : matrix_size) depend(inout : matrix[ii * matrix_size + jj], matrix[ii * matrix_size + kk], matrix[kk * matrix_size + jj])
    for (int kk = 0; kk < matrix_size; kk++) {
#pragma omp taskwait depend(in : kk, matrix_size)
#pragma omp task default(shared) depend(in : submatrix_size) depend(inout : matrix[kk * matrix_size + kk]) firstprivate(matrix_size, kk)
      lu0(matrix[kk * matrix_size + kk], submatrix_size);
#pragma omp taskwait depend(in : kk, matrix_size)
#pragma omp taskwait depend(in : kk, matrix_size) depend(inout : matrix[kk * matrix_size + jj])
      for (int jj = kk + 1; jj < matrix_size; jj++) {
#pragma omp taskwait depend(in : jj, kk, matrix_size)
#pragma omp taskwait depend(in : jj, kk, matrix_size) depend(inout : matrix[kk * matrix_size + jj])
        if (matrix[kk * matrix_size + jj] != NULL) {
#pragma omp taskwait depend(in : jj, kk, matrix_size)
#pragma omp task default(shared) depend(in : matrix[kk * matrix_size + kk], submatrix_size) depend(inout : matrix[kk * matrix_size + jj]) firstprivate(matrix_size, kk, jj)
          fwd(matrix[kk * matrix_size + kk], matrix[kk * matrix_size + jj], submatrix_size);
        }
      }
#pragma omp taskwait depend(in : kk, matrix_size)
#pragma omp taskwait depend(in : kk, matrix_size) depend(inout : matrix[ii * matrix_size + kk])
      for (int ii = kk + 1; ii < matrix_size; ii++) {
#pragma omp taskwait depend(in : ii, kk, matrix_size)
#pragma omp taskwait depend(in : ii, kk, matrix_size) depend(inout : matrix[ii * matrix_size + kk])
        if (matrix[ii * matrix_size + kk] != NULL) {
#pragma omp taskwait depend(in : ii, kk, matrix_size)
#pragma omp task default(shared) depend(in : matrix[kk * matrix_size + kk], submatrix_size) depend(inout : matrix[ii * matrix_size + kk]) firstprivate(matrix_size, kk, ii)
          bdiv(matrix[kk * matrix_size + kk], matrix[ii * matrix_size + kk], submatrix_size);
        }
      }
#pragma omp taskwait depend(in : kk, matrix_size)
#pragma omp taskwait depend(in : kk, matrix[ii * matrix_size + kk], matrix[kk * matrix_size + jj], matrix_size) depend(inout : matrix[ii * matrix_size + jj])
      for (int ii = kk + 1; ii < matrix_size; ii++) {
#pragma omp taskwait depend(in : ii, kk, matrix_size)
#pragma omp taskwait depend(in : ii, kk, matrix[ii * matrix_size + kk], matrix[kk * matrix_size + jj], matrix_size) depend(inout : matrix[ii * matrix_size + jj])
        if (matrix[ii * matrix_size + kk] != NULL) {
#pragma omp taskwait depend(in : ii, kk, matrix_size)
#pragma omp taskwait depend(in : ii, kk, matrix[kk * matrix_size + jj], matrix_size) depend(inout : matrix[ii * matrix_size + jj])
          for (int jj = kk + 1; jj < matrix_size; jj++) {
#pragma omp taskwait depend(in : ii, jj, kk, matrix_size)
#pragma omp taskwait depend(in : ii, jj, kk, matrix[kk * matrix_size + jj], matrix_size) depend(inout : matrix[ii * matrix_size + jj])
            if (matrix[kk * matrix_size + jj] != NULL) {
#pragma omp taskwait depend(in : ii, jj, matrix_size)
#pragma omp taskwait depend(in : ii, jj, matrix_size) depend(inout : matrix[ii * matrix_size + jj])
              if (matrix[ii * matrix_size + jj] == NULL) {
#pragma omp taskwait depend(in : ii, jj, matrix_size)
#pragma omp task default(shared) depend(inout : matrix[ii * matrix_size + jj], submatrix_size) firstprivate(matrix_size, jj, ii)
                matrix[ii * matrix_size + jj] = allocate_clean_block(submatrix_size);
#pragma omp taskwait depend(in : ii, jj, matrix_size)
#pragma omp taskwait depend(in : ii, jj, matrix[ii * matrix_size + jj], matrix_size)
                if (matrix[ii * matrix_size + jj] == NULL) {
#pragma omp taskwait
                  __apac_result = 1;
                  goto __apac_exit;
                }
              }
#pragma omp taskwait depend(in : ii, jj, kk, matrix_size)
#pragma omp task default(shared) depend(in : matrix[ii * matrix_size + kk], matrix[kk * matrix_size + jj], submatrix_size) depend(inout : matrix[ii * matrix_size + jj]) firstprivate(matrix_size, kk, jj, ii)
              bmod(matrix[ii * matrix_size + kk], matrix[kk * matrix_size + jj], matrix[ii * matrix_size + jj], submatrix_size);
            }
          }
        }
      }
    }
#pragma omp taskwait
    __apac_result = 0;
    goto __apac_exit;
  __apac_exit:;
  }
  return __apac_result;
}

int main(const int argc, char** argv) {
  int __apac_result;
#pragma omp parallel
#pragma omp master
#pragma omp taskgroup
  {
    size_t matrix_size = 50, submatrix_size = 100;
    char *struct_A = NULL, *struct_LU = NULL;
    char *matrix_A = NULL, *matrix_LU = NULL;
    int need_free = 0;
    if (argc > 6) {
      matrix_size = strtoul(argv[1], NULL, 0);
      submatrix_size = strtoul(argv[2], NULL, 0);
      struct_A = argv[3];
      struct_LU = argv[4];
      matrix_A = argv[5];
      matrix_LU = argv[6];
    } else {
      struct_A = (char*)malloc(9 * sizeof(char));
      struct_LU = (char*)malloc(10 * sizeof(char));
      matrix_A = (char*)malloc(9 * sizeof(char));
      matrix_LU = (char*)malloc(10 * sizeof(char));
      if (struct_A == NULL || struct_LU == NULL || matrix_A == NULL || matrix_LU == NULL) {
        fprintf(stderr, "Error: Failed to reserve memory for the file names.\n");
        __apac_result = 1;
        goto __apac_exit;
      }
      struct_A = strncpy(struct_A, "A.struct", 9);
      struct_LU = strncpy(struct_LU, "LU.struct", 10);
      matrix_A = strncpy(matrix_A, "A.matrix", 9);
      matrix_LU = strncpy(matrix_LU, "LU.matrix", 10);
      need_free = 1;
    }
    float** matrix = (float**)malloc(matrix_size * matrix_size * sizeof(float*));
    if (matrix == NULL) {
      fprintf(stderr, "Error: Failed to reserve memory for the matrix.\n");
      __apac_result = 1;
      goto __apac_exit;
    }
    matrix = genmat(matrix, matrix_size, submatrix_size);
    if (matrix == NULL) {
      fprintf(stderr, "Error: Failed to generate the matrix.\n");
      __apac_result = 1;
      goto __apac_exit;
    }
    int error = 0;
    error = store_structure(struct_A, "A", matrix, matrix_size);
    if (error) {
      fprintf(stderr, "Error: Failed to store the structure of the matrix.\n");
      __apac_result = 1;
      goto __apac_exit;
    }
    error = store_matrix(matrix_A, "A", matrix, matrix_size, submatrix_size);
    if (error) {
      fprintf(stderr, "Error: Failed to store the matrix.\n");
      __apac_result = 1;
      goto __apac_exit;
    }
#pragma omp task default(shared) depend(in : matrix_size, submatrix_size) depend(inout : error, matrix[0][0])
    error = sparselu(matrix, matrix_size, submatrix_size);
#pragma omp taskwait
    if (error) {
      fprintf(stderr, "Error: Failed to perform the LU decomposition.\n");
      __apac_result = 1;
      goto __apac_exit;
    }
    error = store_structure(struct_LU, "LU", matrix, matrix_size);
    if (error) {
      fprintf(stderr, "Error: Failed to store the structure of the LU matrix.\n");
      __apac_result = 1;
      goto __apac_exit;
    }
    error = store_matrix(matrix_LU, "LU", matrix, matrix_size, submatrix_size);
    if (need_free) {
      free(struct_A);
      free(struct_LU);
      free(matrix_A);
      free(matrix_LU);
    }
    if (error) {
      fprintf(stderr, "Error: Failed to store the LU matrix.\n");
      __apac_result = 1;
      goto __apac_exit;
    }
    __apac_result = 0;
    goto __apac_exit;
  __apac_exit:;
  }
  return __apac_result;
}
