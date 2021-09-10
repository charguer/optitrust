void bar();

void foo() {
#pragma omp task if (0)
  {
#pragma omp task
    for (int i = 0; (i < 3); i++) {
#pragma omp task
      bar();
    }
  }
#pragma omp task final(1)
  {
#pragma omp task
    for (int j = 0; (j < 3); j++) {
#pragma omp task
      bar();
    }
  }
}