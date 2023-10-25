int f(int a, int b) { return a + b; }

int g(int& a) { return 0; }

int h() {
  int a;
  int b;
#pragma omp task shared(a) depend(inout : a)
  { a = 1; }
#pragma omp task shared(b) depend(inout : b)
  { b++; }
#pragma omp task shared(b) depend(inout : b)
  { g(b); }
#pragma omp task shared(a) depend(inout : a)
  { a = 2; }
#pragma omp task depend(in : b, a)
  { f(a, b); }
#pragma omp task shared(a) depend(inout : a)
  { a = 3; }
}
