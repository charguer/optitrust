int main() {
  {
    int* a = new int(0);
    int* b = new int[5]{1, 2, 3, 4, 5};
    while (*a < 10) {
      int* c = new int();
      *c = 1;
      (*a)++;
#pragma omp task default(shared) depend(inout : c)
      delete c;
    }
    *a = 1;
    b[1] = 2;
#pragma omp task default(shared) depend(inout : a)
    delete a;
#pragma omp task default(shared) depend(inout : b)
    delete[] b;
    return 1;
  }
  return 1;
}
