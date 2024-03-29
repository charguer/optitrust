int g(int a, int b) {
  int __res;
#pragma omp taskgroup
  {
    __res = a + b + a;
    goto __exit;
  __exit:;
  }
  return __res;
}

int f() {
  int __res;
#pragma omp taskgroup
  {
    const int x = 3;
    int y = 5;
    int z = 3;
    __res = g(x, g(y, z));
    goto __exit;
  __exit:;
  }
  return __res;
}

void h() {
#pragma omp taskgroup
  {
    int a = g(f(), f());
  __exit:;
  }
}

int main() {
  int __res;
#pragma omp parallel
#pragma omp master
#pragma omp taskgroup
  {
    f();
  __exit:;
  }
  return __res;
}
