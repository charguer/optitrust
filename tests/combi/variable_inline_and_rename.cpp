

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

typedef struct {
  int x;
  int y;
} vect;

int test_nonconstvect_nonconstvect(){
  vect a;
  a.x = 6;
  a.x = 7;
  vect b = a;
  int c = b.x;
  int d = b.y;
}

