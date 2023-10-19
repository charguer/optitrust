
int f(int a, int b) {
  return a + b;
}

int main()
{
  int x = 0;
  x = f(5,x);
  x = x + 3;
  x = 3 + x;
  x = x - 2;
  return 0;
}
