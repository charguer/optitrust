void g(int* tab) { tab[0] += 42; }

void f(int* tab1, int* tab2, const int size) {
#pragma omp taskgroup
  {
#pragma omp task default(shared) depend(inout : tab1[0])
    g(tab1);
    int a = 2, b = 0;
#pragma omp task default(shared) depend(in : b) depend(inout : tab1[2])
    tab1[2] = 2 + b;
#pragma omp task default(shared) depend(in : tab1[3]) depend(inout : tab2[3])
    tab2[3] = 4 * tab1[3];
#pragma omp task default(shared) depend(in : tab2[0]) depend(inout : tab1[0])
    tab1[0] = tab2[0];
#pragma omp task default(shared) depend(in : a) depend(inout : tab2[4])
    tab2[4] = 4 * a;
#pragma omp task default(shared) depend(inout : tab1[0])
    g(tab1);
#pragma omp task default(shared) depend(inout : tab2[0])
    g(tab2);
  __apac_exit:;
  }
}
