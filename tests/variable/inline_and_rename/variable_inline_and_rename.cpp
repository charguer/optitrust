

void test_const_const()
{
  const int x = 5;
  const int y = x;
  int z = x + y;
}

void test_nonconst_const()
{
  int x = 5;
  x = 6;
  x = 7;
  const int y = x;
  int z = x + y;
}

void test_const_nonconst()
{
  const int x = 5;
  int y = x;
  int z = x + y;
}

void test_nonconst_nonconst()
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


void test_const_const_vect(){
  const vect a =  {6, 7};
  const vect b = a;
  int c = b.x;
  int d = b.y;
}

void test_nonconst_const_vect(){
  vect a;
  a.x = 6;
  a.x = 7;
  const vect b = a;
  int c = b.x;
  int d = b.y;
}

void test_const_nonconst_vect(){
  const vect a = {6, 7};
  vect b = a;
  int c = b.x;
  int d = b.y;
}

void test_nonconst_nonconst_vect(){
  vect a;
  a.x = 6;
  a.x = 7;
  vect b = a;
  int c = b.x;
  int d = b.y;
}

