int main() {
  int *t;
  int a = 2;
  {
    int a = 0 * 2;
    t[0] = a;
  }
  {
    int a = 1 * 2;
    t[1] = a;
  }
  {
    int a = 2 * 2;
    t[2] = a;
  }
  return 0;
}
