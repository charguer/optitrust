int f (int x) {
  int y = 1;
  return y+x;
}

int f1 (int x) {
  int y = -1;
  return y+x;
}

int main(){
  int a = 5;
  int b = 6;
  a = f(a);
  int x = 6;
  int c = f1(4);
  return 0;
}