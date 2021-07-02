int f(int x) {
  return x+1;
}


int g(int x) {
  int y = x + x;
  return y + y;
}

int main(){
  int x = 3;
  int y = f(g(x));
  return 0;
}
