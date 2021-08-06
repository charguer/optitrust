
int f(int x, int y){
  return x+y;
}

float f1(int x, float y){
  return x+y;
}



int main() {

  { const int x = 1;
    const int y = 2; }

  { int x = 1;
    int y = 2; }

  { int x = 1;
    int y = 2;
    int z = 3; }

  { int x = 1;
    int y = 2;
    x++; }

  int a;
  a = f(1,2);
  int b = f1(1,2.);

}