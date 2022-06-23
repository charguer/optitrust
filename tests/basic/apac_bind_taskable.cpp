
int g(int a, int b) {
  return a + b + a;
}

int f(int x) {
  int y = 5;
  int z = 3;
  return y + z;
}



void test_invariant (){
  int x = 10;
  int a;
  a = f(x);
  f(x);
}


void test (){

  int x = 10;

  int b = f(x) + g(0, 1);

}


int main() {
  int x = 10;

  int b = f(x) + g(0, 1);

  const int c = f(x);

  int d = f(x);

  int e = g(f(x), x);

  return 0;

}
