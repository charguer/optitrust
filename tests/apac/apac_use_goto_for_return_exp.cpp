int g(int a, int b) {
  int __res;
  {
    __res = a + b + a;
    goto __apac_exit;
  __apac_exit:;
  }
  return __res;
}

int f() {
  int __res;
  {
    const int x = 3;
    int y = 5;
    int z = 3;
    __res = g(x, g(y, z));
    goto __apac_exit;
  __apac_exit:;
  }
  return __res;
}

void h() {
  {
    int a = g(f(), f());
  __apac_exit:;
  }
}

float i(float pi, float a) {
  float __res;
  {
    float coef = 11.7f;
    if (a < 0.) {
      __res = coef * a;
      goto __apac_exit;
    }
    __res = coef * pi * a;
    goto __apac_exit;
  __apac_exit:;
  }
  return __res;
}

int main() {
  int __res;
  {
    f();
  __apac_exit:;
  }
  return __res;
}
