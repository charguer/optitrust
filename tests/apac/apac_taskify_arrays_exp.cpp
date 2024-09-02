void g(int* tab) { tab[0] += 42; }

void f(int* tab1, int* tab2, int size) {
#pragma omp taskgroup
  {
    int a = 2, b = 0;
#pragma omp task default(shared) depend(in : tab1) depend(inout : tab1[0])
    g(tab1);
#pragma omp taskwait depend(in : tab1, tab2, tab2[0]) depend(inout : tab1[0])
    tab1[0] = tab2[0];
#pragma omp taskwait depend(in : b, tab1) depend(inout : tab1[2])
    tab1[2] = 2 + b;
#pragma omp taskwait depend(in : tab1, tab1[3], tab2) depend(inout : tab2[3])
    tab2[3] = 4 * tab1[3];
    tab2[4] = 4 * a;
#pragma omp task default(shared) depend(in : tab1) depend(inout : tab1[0])
    g(tab1);
#pragma omp task default(shared) depend(in : tab2) depend(inout : tab2[0])
    g(tab2);
#pragma omp taskwait
  __apac_exit:;
  }
}
