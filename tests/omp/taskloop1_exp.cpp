void long_running_task();

void loop_body(int i, int j);

void parallel_work() {
#pragma omp taskgroup
  {
#pragma omp task
    long_running_task();
#pragma omp taskloop private(j) grainsize(500) nogroup
    for (int i = 0; (i < 10000); i++) {
      for (int j = 0; (j < i); j++) {
        loop_body(i, j);
      }
    }
  }
}