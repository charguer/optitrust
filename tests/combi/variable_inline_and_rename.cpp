// void f(int a);
void f(int);

int main()
{
  int x = 5;
  x = 6;
  x = 7;
  const int y = x;
  int z = x;
  f(y);
  return 0;
}
