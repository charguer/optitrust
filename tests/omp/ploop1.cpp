#include <omp.h>

void simple(int n, float *a, float *b)
{
    for (int i=1; i<n; i++) /* i is private by default */
        b[i] = (a[i] + a[i-1]) / 2.0;
}