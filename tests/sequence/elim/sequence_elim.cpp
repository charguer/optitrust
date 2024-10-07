int main() {
  int x = 3;
  {
    int a = 8;
    int b = 9;
  }
  {
    int u = 2;
  }
  {
    int y = 1;
    int z = 5;
  }
  ({
    const int res = 42;
    res;
  });
  int c = ({
    const int res = 2;
    res;
  });
  c += 3 * ({
    const int k = 5;
    const int res = k * k;
    res;
  });
  return 0;
}

