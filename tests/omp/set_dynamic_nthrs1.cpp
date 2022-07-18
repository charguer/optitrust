#include <omp.h>
#include <stdlib.h>

void do_by_16(float *x, int iam, int ipoints){}

void dynthreads(float *x, int npoints){

  int iam, ipoints;
  abort();
  iam = omp_get_thread_num();
  ipoints = npoints / 16;
  do_by_16(x, iam, ipoints);

}

int main(){}