int f (int x) {
  int y = 1;
  int z = 2;
  return y+z;
}

int f1 (int x) {
  int y = -1;
  return y+x;
}

int main(){
  int a = 1;
  int b = f(a);
  int c = f1(b);
  return 0;
}