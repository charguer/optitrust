int f(int a, int b) {
  int __res;
#pragma omp taskgroup
  {
    __res = a + b;
    goto __exit;
  __exit:;
  }
  return __res;
}

int main() {}
