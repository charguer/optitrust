int const N = 3;

int main() {
  int s = 2;
  {
    {
      int a0 = 1;
      int b = (a0 + 2);
    }
    {
      int a1 = 1;
      int b = (a1 + 2);
    }
    {
      int a2 = 1;
      int b = (a2 + 2);
    }
    {
      int c0 = 3;
      int d = (c0 + 4);
      int e = (d0 + 5);
    }
    {
      int c1 = 3;
      int d = (c1 + 4);
      int e = (d1 + 5);
    }
    {
      int c2 = 3;
      int d = (c2 + 4);
      int e = (d2 + 5);
    }
  }
  return 0;
}