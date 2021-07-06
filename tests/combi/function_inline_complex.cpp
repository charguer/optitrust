int f(int x) {
  return x+1;
}

int g(int x, int y, int z, int w) {
  int p = x + y + z + w;
  return p + p;
}

int h( int x) {
  return x-1;
}

int m( int x, int y) {
  return x-y;
}

void n(int x){
  x = x + 1;
  return;
}

int main(){
  int x = 3;
  int u, v, w;
  int t = f(g(h(4), u, m(v,2), w+1));
  return 0;
}
