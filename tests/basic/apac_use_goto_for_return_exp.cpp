int g(int a, int b) {
  int __res;
  {
    __res = a + b + a;
    goto __exit;
  }
__exit:;
  return __res;
}

int f() {
  int __res;
  {
    const int x = 3;
    int y = 5;
    int z = 3;
    __res = g(x, g(y, z));
    goto __exit;
  }
__exit:;
  return __res;
}

void h() {
  { int a = g(f(), f()); }
__exit:;
}

int main() { f(); }
