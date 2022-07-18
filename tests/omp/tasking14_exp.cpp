void bar();

void foo() {
  int i;
#pragma omp task if (0)
  {
#pragma omp task
    for (i = 0; i < 3; i++) {
#pragma omp task
      bar();
    }
  }
#pragma omp task final(1)
  {
#pragma omp task
    for (i = 0; i < 3; i++) {
#pragma omp task
      bar();
    }
  }
}
