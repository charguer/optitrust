#include <stdio.h>
#include <omp.h>
int main (void)
{
         /*
         * If OMP_NUM_THREADS=2,3 was set, the following should print:
         * Inner: num_thds=3
         * Inner: num_thds=3
         *
         * If nesting is not supported, the following should print:
         * Inner: num_thds=1
         * Inner: num_thds=1
         */
            printf ("Inner: num_thds=%d\n", omp_get_num_threads());
         /*
         * Even if OMP_NUM_THREADS=2,3 was set, the following should
         * print, because nesting is disabled:
         * Inner: num_thds=1
         * Inner: num_thds=1
         */
            printf ("Inner: num_thds=%d\n", omp_get_num_threads());
         /*
         * If OMP_NUM_THREADS=2,3 was set, the following should print:
         * Outer: num_thds=2
         */
         printf ("Outer: num_thds=%d\n", omp_get_num_threads());
   return 0;
}