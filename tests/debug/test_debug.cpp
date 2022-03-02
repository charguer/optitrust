// void f(int a);


typedef struct {
  int x;
  int y;
} vect;

void f(int);


int test_nonconst_nonconst()
{

  vect a;

  a.x = 6;
  a.x = 7;

  vect b = a;

  int c = b.x;
  int d = b.y;

}
