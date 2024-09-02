int f(int a, int b) { return a + b; }

int g(const int& a) { return 0; }

int h() {
  int __apac_result;
#pragma omp taskgroup
  {
    int a;
    int b;
    a = 1;
    a = 2;
    b++;
#pragma omp task default(shared) depend(in : b)
    g(b);
#pragma omp task default(shared) depend(in : b) depend(inout : a)
    {
      f(a, b);
      a = 3;
    }
#pragma omp taskwait
  __apac_exit:;
  }
  return __apac_result;
}
