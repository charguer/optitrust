int main() {
  int *t;
  int a = 2;
  {
    int a = ((a + 0) * 2);
    t[(a + 0)] = a;
  }
  {
    int a = ((a + 1) * 2);
    t[(a + 1)] = a;
  }
  {
    int a = ((a + 2) * 2);
    t[(a + 2)] = a;
  }
  return 0;
}