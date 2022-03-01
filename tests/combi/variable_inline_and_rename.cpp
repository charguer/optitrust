// void f(int a);
void f(int);

int test_const_const()
{
  const int x = 5;
  const int y = x;
  int z = x + y;
}

int test_nonconst_const()
{
  int x = 5;
  x = 6;
  x = 7;
  const int y = x;
  int z = x + y;
}

int test_const_nonconst()
{
  const int x = 5;
  int y = x;
  int z = x + y;
}

int test_nonconst_nonconst()
{
  int x = 5;
  x = 6;
  x = 7;
  int y = x;
  int z = x + y;
}
