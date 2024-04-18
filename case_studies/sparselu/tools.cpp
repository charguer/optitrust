#include "tools.hpp"

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