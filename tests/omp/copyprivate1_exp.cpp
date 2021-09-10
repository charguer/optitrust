#include <stdio.h>

float x;

#pragma omp threadprivate(x, y)

float y;

void init(float a, float b) {
#pragma omp parallel copyprivate(a, b, x, y)
  { scanf("%f %f %f %f", (&a), (&b), (&x), (&y)); }
}