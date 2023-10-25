
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
  for (int i = 0; i < 2; i++) {
    {
      int s0 = i;
    }
  }
  for (int s1 = 0; s1 < 2; s1++) {
    {
      int s1 = 0;
      // x = s1;
    }
    x = s1;
  }
  return 0;
}

