int main() {
  {
    int* a = new int();
    int* b = new int[5]{1, 2, 3, 4, 5};
    while (true) {
      int b;
      b = 1;
      break;
    }
    *a = 1;
    b[1] = 2;
#pragma omp task depend(inout : *b, *a)
    {
      delete[] b;
      delete a;
    }
    return 1;
  }
  return 1;
}
