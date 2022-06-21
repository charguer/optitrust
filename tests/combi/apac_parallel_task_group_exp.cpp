#pragma omp parallel
#pragma omp master
#pragma omp taskgroup
int g(int a, int b) {
  int __res;
  {
    int __res;
    {
      {
        __res = a + b + a;
        goto __exit;
      }
    }
  __exit:;
    {
      __res = __res;
      goto __exit;
    }
  }
__exit:;
  return __res;
}

#pragma omp parallel
#pragma omp master
#pragma omp taskgroup
int f() {
  int __res;
  {
    int __res;
    {
      const int x = 3;
      int y = 5;
      int z = 3;
      {
        __res = g(x, g(y, z));
        goto __exit;
      }
    }
  __exit:;
    {
      __res = __res;
      goto __exit;
    }
  }
__exit:;
  return __res;
}

#pragma omp parallel
#pragma omp master
#pragma omp taskgroup
void h() {
  {
    { int a = g(f(), f()); }
  __exit:;
  }
__exit:;
}

int main() { f(); }
