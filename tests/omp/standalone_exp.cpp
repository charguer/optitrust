void standalone_ok() {
  int a = 1;
#pragma omp parallel
  {
    if ((a != 0)) {
#pragma omp flush(a)
    }
    if ((a != 0)) {
#pragma omp barrier
    }
    if ((a != 0)) {
#pragma omp taskwait
    }
    if ((a != 0)) {
#pragma omp taskyield
    }
  }
}