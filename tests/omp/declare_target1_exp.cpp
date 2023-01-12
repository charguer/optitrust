#pragma omp declare target
void fib(int N);

#pragma omp end declare target
const int THRESHOLD = 1000000;

void fib_wrapper(int n) {
#pragma omp target if (n > THRESHOLD)
  { fib(n); }
}
