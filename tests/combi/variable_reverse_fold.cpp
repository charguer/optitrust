void f(int a) { a = a+1:}



int main(int argc, char const *argv[])
{
  int x = 5;
  x = 6;
  x = 7;
  int y = x;
  f(y);
  for(int i = 0; i < 10; i++) {
    y++;
  }

  return 0;
}
