int main() {
  int* a = new int();
  int* b = new int[5]({1, 2, 3, 4, 5});
  {
    while (true) {
      int b;
      b = 1;
      break;
    }
#pragma omp task depend(inout : *b, *a) firstprivate(b, a)
    {
      delete[] b;
      delete a;
    }
    return 1;
  }
#pragma omp task depend(inout : *b, *a) firstprivate(b, a)
  {
    delete[] b;
    delete a;
  }
  return 1;
}
