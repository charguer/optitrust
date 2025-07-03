#ifndef DUMPER_H
#define DUMPER_H

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void fwrite_double(FILE *f, double v);
void fwrite_int64(FILE *f, int64_t v);
void fwrite_int(FILE *f, int v);
void fwrite_float(FILE *f, float v);

void fread_double(FILE *f, double *v);
void fread_int64(FILE *f, int64_t *v);
void fread_int(FILE *f, int *v);
void fread_float(FILE *f, float *v);

void fwrite_array_double(const char *filename, const double *array, int size);
void fwrite_array_int(const char *filename, const int *array, int size);
void fwrite_array_float(const char *basename, const float *array, int size,
                        int pos, int layer);
void fwrite_array_int64(const char *filename, const int64_t *array, int size);

void fread_array_double(const char *filename, double *array, int size);
void fread_array_int(const char *filename, int *array, int size);
void fread_array_float(const char *filename, float *array, int size);
void fread_array_int64(const char *filename, int64_t *array, int size);

#ifdef DUMPER
#define DUMP(fun_call) fun_call;
#define COND_DUMP(fun_call, condition)                                         \
  if (condition)                                                               \
    fun_call;
#define COND l <= 2 || l == 15// pas obligé de faire des define pour les cond
#else
#define DUMP(fun_call)
#define COND_DUMP(fun_call, condition)
#endif

#endif
