int f(const int a, const int b) { return a + b; }

int g(const int& a) { return 0; }

int h() {
  int __apac_result;
#pragma omp taskgroup
  {
    int a;
    int b;
    a = 1;
    a = 2;
#pragma omp taskwait depend(inout : b)
    b++;
#pragma omp task default(shared) depend(in : b) depend(inout : a)
    {
      f(a, b);
      a = 3;
    }
#pragma omp task default(shared) depend(in : b)
    g(b);
  __apac_exit:;
  }
  return __apac_result;
}
