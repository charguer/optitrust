int increment_counter_2() {
  int counter = 0;
#pragma omp threadprivate(counter)
  counter++;
  return counter;
}