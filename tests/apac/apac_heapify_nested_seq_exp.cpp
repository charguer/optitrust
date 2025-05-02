int main() {
  {
    int* a = new int(0);
    int* b = new int[5]{1, 2, 3, 4, 5};
    while (*a < 10) {
      int* c = new int();
      *c = 1;
      (*a)++;
#pragma omp task default(shared) depend(inout : c[0]) firstprivate(c)
      delete c;
    }
    *a = 1;
    b[1] = 2;
#pragma omp task default(shared) depend(inout : a[0]) firstprivate(a)
    delete a;
#pragma omp task default(shared) depend(inout : b[0]) firstprivate(b)
    delete[] b;
    return 1;
  }
  return 1;
}
