int tp;

int var;

void work() {
#pragma omp task
  {
#pragma omp task
    {
      tp = 1;
#pragma omp task
      {}
      var = tp;
    }
    tp = 2;
  }
}