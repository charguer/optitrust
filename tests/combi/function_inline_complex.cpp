int f(int x) {
  return x+1;
}

int g(int x, int y, int z, int w) {
  int p = x + x + y + z + w;
  return p + p;
}

int h(int x) {
  return x-1;
}

int m(int x, int y) {
  return x-y;
}

int main() {
 
  int u = 1;
  int v = 2;
  int w = 3;
  int t = f(g(h(4), u, m(v,2), w+1));
  return 0;

}

