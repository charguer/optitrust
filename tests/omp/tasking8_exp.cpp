int tp;

int var;

void work() {
#pragma omp parallel
  {
#pragma omp task
    {
      tp++;
#pragma omp task
      {}
      var = tp;
    }
  }
}