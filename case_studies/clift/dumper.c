#include "dumper.h"
#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

void fwrite_double(FILE *f, double v) { fwrite(&v, sizeof(v), 1, f); }

void fwrite_int64(FILE *f, int64_t v) { fwrite(&v, sizeof(v), 1, f); }

void fwrite_int(FILE *f, int v) { fwrite(&v, sizeof(v), 1, f); }

void fwrite_float(FILE *f, float v) { fwrite(&v, sizeof(v), 1, f); }

void fread_double(FILE *f, double *v) { fread(v, sizeof(*v), 1, f); }

void fread_int64(FILE *f, int64_t *v) { fread(v, sizeof(*v), 1, f); }

void fread_int(FILE *f, int *v) { fread(v, sizeof(*v), 1, f); }

void fread_float(FILE *f, float *v) { fread(v, sizeof(*v), 1, f); }

void fwrite_array_double(const char *filename, const double *array, int size) {
  FILE *f = fopen(filename, "wb");
  if (!f) {
    perror("fopen");
    exit(EXIT_FAILURE);
  }
  for (int i = 0; i < size; i++)
    fwrite_double(f, array[i]);
  fclose(f);
}

void fwrite_array_int(const char *filename, const int *array, int size) {
  FILE *f = fopen(filename, "wb");
  if (!f) {
    perror("fopen");
    exit(EXIT_FAILURE);
  }
  for (int i = 0; i < size; i++)
    fwrite_int(f, array[i]);
  fclose(f);
}

void fwrite_array_float(const char *basename, const float *array, int size,
                        int pos, int layer) {
  char filename[256];
  snprintf(filename, sizeof(filename), "%s_%d_%d", basename, pos, layer);
  FILE *f = fopen(filename, "wb");
  if (!f) {
    perror("fopen");
    exit(EXIT_FAILURE);
  }
  for (int i = 0; i < size; i++)
    fwrite_float(f, array[i]);
  fclose(f);
}

void fwrite_array_int64(const char *filename, const int64_t *array, int size) {
  FILE *f = fopen(filename, "wb");
  if (!f) {
    perror("fopen");
    exit(EXIT_FAILURE);
  }
  for (int i = 0; i < size; i++)
    fwrite_int64(f, array[i]);
  fclose(f);
}

void fread_array_double(const char *filename, double *array, int size) {
  FILE *f = fopen(filename, "rb");
  if (!f) {
    perror("fopen");
    exit(EXIT_FAILURE);
  }
  for (int i = 0; i < size; i++)
    fread_double(f, &array[i]);
  fclose(f);
}

void fread_array_int(const char *filename, int *array, int size) {
  FILE *f = fopen(filename, "rb");
  if (!f) {
    perror("fopen");
    exit(EXIT_FAILURE);
  }
  for (int i = 0; i < size; i++)
    fread_int(f, &array[i]);
  fclose(f);
}

void fread_array_float(const char *filename, float *array, int size) {
  FILE *f = fopen(filename, "rb");
  if (!f) {
    perror("fopen");
    exit(EXIT_FAILURE);
  }
  for (int i = 0; i < size; i++)
    fread_float(f, &array[i]);
  fclose(f);
}

void fread_array_int64(const char *filename, int64_t *array, int size) {
  FILE *f = fopen(filename, "rb");
  if (!f) {
    perror("fopen");
    exit(EXIT_FAILURE);
  }
  for (int i = 0; i < size; i++)
    fread_int64(f, &array[i]);
  fclose(f);
}
