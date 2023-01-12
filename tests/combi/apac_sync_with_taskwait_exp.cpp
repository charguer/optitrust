int f() {
  int a;
  int b;
#pragma omp taskwait depend(inout : b, a)
  if (a > b) {
    int c = a + b;
#pragma omp taskwait depend(inout : a, c)
    while (c > a) {
      c++;
#pragma omp taskwait depend(inout : a, c)
    }
  }
#pragma omp taskwait depend(inout : a)
  return a;
}
