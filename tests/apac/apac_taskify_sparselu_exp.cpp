#include <stdio.h>
#include <stdlib.h>
#include <string.h>

float** genmat(float** matrix, const size_t matrix_size, const size_t submatrix_size) {
  int null_entry, init_val = 1325;
  float* p;
  for (int ii = 0; ii < matrix_size; ii++) {
    for (int jj = 0; jj < matrix_size; jj++) {
      null_entry = 0;
      if (ii < jj && ii % 3 != 0) null_entry = 1;
      if (ii > jj && jj % 3 != 0) null_entry = 1;
      if (ii % 2 == 1) null_entry = 1;
      if (jj % 2 == 1) null_entry = 1;
      if (ii == jj) null_entry = 0;
      if (ii == jj - 1) null_entry = 0;
      if (ii - 1 == jj) null_entry = 0;
      if (!null_entry) {
        matrix[ii * matrix_size + jj] = (float*)malloc(submatrix_size * submatrix_size * sizeof(float));
        if (matrix[ii * matrix_size + jj] == NULL) return NULL;
        p = matrix[ii * matrix_size + jj];
        for (int i = 0; i < submatrix_size; i++) {
          for (int j = 0; j < submatrix_size; j++) {
            init_val = 3125 * init_val % 65536;
            *p = (float)((init_val - 32768.) / 16384.);
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

int store_structure(const char* output, const char* name, const float* const* matrix, const size_t matrix_size) {
  FILE* file = fopen(output, "w");
  if (file == NULL) {
    fprintf(stderr, "Error: Failed to open the file for writing.\n");
    return 1;
  }
  int written;
  for (int ii = 0; ii < matrix_size; ii++) {
    for (int jj = 0; jj < matrix_size; jj++) {
      if (matrix[ii * matrix_size + jj] != NULL) {
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

int store_matrix(const char* output, const char* name, float** matrix, const size_t matrix_size, const size_t submatrix_size) {
  FILE* file = fopen(output, "w");
  if (file == NULL) {
    fprintf(stderr, "Error: Failed to open the file for writing.\n");
    return 1;
  }
  float* p;
  int written;
  for (int ii = 0; ii < matrix_size; ii++) {
    for (int jj = 0; jj < matrix_size; jj++) {
      if (matrix[ii * matrix_size + jj] != NULL) {
        for (int i = 0; i < submatrix_size; i++) {
          p = matrix[ii * matrix_size + jj] + i * submatrix_size;
          for (int j = 0; j < submatrix_size; j++) {
            written = fprintf(file, "%s[%lu][%lu] = %f\n", name, ii * submatrix_size + i, jj * submatrix_size + j, *p);
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

float* allocate_clean_block(const size_t submatrix_size) {
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

void lu0(float* diag, const size_t submatrix_size) {
  for (int k = 0; k < submatrix_size; k++) {
    for (int i = k + 1; i < submatrix_size; i++) {
      diag[i * submatrix_size + k] = diag[i * submatrix_size + k] / diag[k * submatrix_size + k];
      for (int j = k + 1; j < submatrix_size; j++) {
        diag[i * submatrix_size + j] = diag[i * submatrix_size + j] - diag[i * submatrix_size + k] * diag[k * submatrix_size + j];
      }
    }
  }
}

void bdiv(const float* diag, float* row, const size_t submatrix_size) {
  for (int i = 0; i < submatrix_size; i++) {
    for (int k = 0; k < submatrix_size; k++) {
      row[i * submatrix_size + k] = row[i * submatrix_size + k] / diag[k * submatrix_size + k];
      for (int j = k + 1; j < submatrix_size; j++) {
        row[i * submatrix_size + j] = row[i * submatrix_size + j] - row[i * submatrix_size + k] * diag[k * submatrix_size + j];
      }
    }
  }
}

void bmod(const float* row, const float* col, float* inner, const size_t submatrix_size) {
  for (int i = 0; i < submatrix_size; i++) {
    for (int j = 0; j < submatrix_size; j++) {
      for (int k = 0; k < submatrix_size; k++) {
        inner[i * submatrix_size + j] = inner[i * submatrix_size + j] - row[i * submatrix_size + k] * col[k * submatrix_size + j];
      }
    }
  }
}

void fwd(const float* diag, float* col, const size_t submatrix_size) {
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
    for (int kk = 0; kk < matrix_size; kk++) {
#pragma omp task default(shared) depend(in : matrix, matrix[kk * matrix_size + kk], matrix_size, submatrix_size) depend(inout : matrix[kk * matrix_size + kk][0]) firstprivate(kk)
      lu0(matrix[kk * matrix_size + kk], submatrix_size);
      for (int jj = kk + 1; jj < matrix_size; jj++) {
#pragma omp taskwait depend(in : matrix, matrix[kk * matrix_size + jj], matrix_size)
        if (matrix[kk * matrix_size + jj] != NULL) {
#pragma omp task default(shared) depend(in : matrix, matrix[kk * matrix_size + jj], matrix[kk * matrix_size + kk], matrix[kk * matrix_size + kk][0], matrix_size, submatrix_size) depend(inout : matrix[kk * matrix_size + jj][0]) firstprivate(kk, jj)
          fwd(matrix[kk * matrix_size + kk], matrix[kk * matrix_size + jj], submatrix_size);
        }
      }
      for (int ii = kk + 1; ii < matrix_size; ii++) {
#pragma omp taskwait depend(in : matrix, matrix[ii * matrix_size + kk], matrix_size)
        if (matrix[ii * matrix_size + kk] != NULL) {
#pragma omp task default(shared) depend(in : matrix, matrix[ii * matrix_size + kk], matrix[kk * matrix_size + kk], matrix[kk * matrix_size + kk][0], matrix_size, submatrix_size) depend(inout : matrix[ii * matrix_size + kk][0]) firstprivate(kk, ii)
          bdiv(matrix[kk * matrix_size + kk], matrix[ii * matrix_size + kk], submatrix_size);
        }
      }
      for (int ii = kk + 1; ii < matrix_size; ii++) {
#pragma omp taskwait depend(in : matrix, matrix[ii * matrix_size + kk], matrix_size)
        if (matrix[ii * matrix_size + kk] != NULL) {
          for (int jj = kk + 1; jj < matrix_size; jj++) {
#pragma omp taskwait depend(in : matrix, matrix[kk * matrix_size + jj], matrix_size)
            if (matrix[kk * matrix_size + jj] != NULL) {
#pragma omp task default(shared) depend(in : matrix, matrix[ii * matrix_size + kk], matrix[ii * matrix_size + kk][0], matrix[kk * matrix_size + jj], matrix[kk * matrix_size + jj][0], matrix_size, submatrix_size) depend(inout : matrix[ii * matrix_size + jj], matrix[ii * matrix_size + jj][0]) firstprivate(kk, jj, ii)
              {
                matrix[ii * matrix_size + jj] = (!matrix[ii * matrix_size + jj] ? allocate_clean_block(submatrix_size) : matrix[ii * matrix_size + jj]);
                bmod(matrix[ii * matrix_size + kk], matrix[kk * matrix_size + jj], matrix[ii * matrix_size + jj], submatrix_size);
              }
            }
          }
        }
      }
    }
    __apac_result = 0;
    goto __apac_exit;
  __apac_exit:;
  }
  return __apac_result;
}

int main(int argc, char** argv) {
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
    void* __apac_var1;
    __apac_var1 = malloc(matrix_size * matrix_size * sizeof(float*));
    float** matrix = (float**)__apac_var1;
    if (matrix == NULL) {
      fprintf(stderr, "Error: Failed to reserve memory for the matrix.\n");
      __apac_result = 1;
      goto __apac_exit;
    }
#pragma omp task default(shared) depend(in : matrix_size, submatrix_size) depend(inout : matrix, matrix[0], matrix[0][0])
    matrix = genmat(matrix, matrix_size, submatrix_size);
#pragma omp taskwait depend(in : matrix)
    if (matrix == NULL) {
      fprintf(stderr, "Error: Failed to generate the matrix.\n");
#pragma omp taskwait
      __apac_result = 1;
      goto __apac_exit;
    }
    int error = 0;
#pragma omp task default(shared) depend(in : argv, matrix, matrix[0], matrix[0][0], matrix_size) depend(inout : error)
    error = store_structure(struct_A, "A", matrix, matrix_size);
#pragma omp taskwait depend(in : error)
    if (error) {
      fprintf(stderr, "Error: Failed to store the structure of the matrix.\n");
#pragma omp taskwait
      __apac_result = 1;
      goto __apac_exit;
    }
#pragma omp task default(shared) depend(in : argv, matrix, matrix_size, submatrix_size) depend(inout : error, matrix[0], matrix[0][0])
    error = store_matrix(matrix_A, "A", matrix, matrix_size, submatrix_size);
#pragma omp taskwait depend(in : error)
    if (error) {
      fprintf(stderr, "Error: Failed to store the matrix.\n");
#pragma omp taskwait
      __apac_result = 1;
      goto __apac_exit;
    }
#pragma omp task default(shared) depend(in : matrix, matrix_size, submatrix_size) depend(inout : error, matrix[0], matrix[0][0])
    error = sparselu(matrix, matrix_size, submatrix_size);
#pragma omp taskwait depend(in : error)
    if (error) {
      fprintf(stderr, "Error: Failed to perform the LU decomposition.\n");
#pragma omp taskwait
      __apac_result = 1;
      goto __apac_exit;
    }
#pragma omp task default(shared) depend(in : argv, matrix, matrix[0], matrix[0][0], matrix_size) depend(inout : error)
    error = store_structure(struct_LU, "LU", matrix, matrix_size);
#pragma omp taskwait depend(in : error)
    if (error) {
      fprintf(stderr, "Error: Failed to store the structure of the LU matrix.\n");
#pragma omp taskwait
      __apac_result = 1;
      goto __apac_exit;
    }
#pragma omp task default(shared) depend(in : argv, matrix, matrix_size, submatrix_size) depend(inout : error, matrix[0], matrix[0][0])
    error = store_matrix(matrix_LU, "LU", matrix, matrix_size, submatrix_size);
#pragma omp taskwait
    if (error) {
      fprintf(stderr, "Error: Failed to store the LU matrix.\n");
      __apac_result = 1;
      goto __apac_exit;
    }
    if (need_free) {
      free(struct_A);
      free(struct_LU);
      free(matrix_A);
      free(matrix_LU);
    }
    __apac_result = 0;
    goto __apac_exit;
  __apac_exit:;
  }
  return __apac_result;
}
