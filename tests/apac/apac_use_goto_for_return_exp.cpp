int g(int a, int b) {
  int __apac_result;
  {
    __apac_result = a + b + a;
    goto __apac_exit;
  __apac_exit:;
  }
  return __apac_result;
}

int f() {
  int __apac_result;
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
  {
    int a = g(f(), f());
  __apac_exit:;
  }
}

float i(float pi, float a) {
  float __apac_result;
  {
    float coef = 11.7f;
    if (a < 0.) {
      __apac_result = coef * a;
      goto __apac_exit;
    }
    __apac_result = coef * pi * a;
    goto __apac_exit;
  __apac_exit:;
  }
  return __apac_result;
}

int main() {
  int __apac_result;
  {
    f();
  __apac_exit:;
  }
  return __apac_result;
}
