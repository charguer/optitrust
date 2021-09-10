void work();

void foo() {
#pragma omp parallel num_threads(16) proc_bind(spread)
  { work(); }
}

void bar() {
#pragma omp parallel proc_bind(close) num_threads(4)
  { work(); }
}

void foo1() {
#pragma omp parallel num_threads(16) proc_bind(close)
  { work(); }
}

void bar1() {
#pragma omp parallel proc_bind(master) num_threads(4)
  { work(); }
}

int main() {
#pragma omp parallel proc_bind(spread) num_threads(4)
  { work(); }
  return 0;
}