int g(int a, int b) {
  int __apac_result;
#pragma omp taskgroup
  {
    __apac_result = a + b + a;
    goto __apac_exit;
  __apac_exit:;
  }
  return __apac_result;
}

int f() {
  int __apac_result;
#pragma omp taskgroup
  {
    const int x = 3;
    int y = 5;
    int z = 3;
    __apac_result = g(x, g(y, z));
    goto __apac_exit;
  __apac_exit:;
  }
  return __apac_result;
}

void h() {
#pragma omp taskgroup
  {
    int a = g(f(), f());
  __apac_exit:;
  }
}

int main() {
  int __apac_result;
#pragma omp parallel
#pragma omp master
#pragma omp taskgroup
  {
    f();
  __apac_exit:;
  }
  return __apac_result;
}
