int f(int x) {
  return x+1;
}

int g(int x, int y, int z, int w) {
  int p = x + y + z + w;
  return p + p;
}

int h(int x) {
  return x-1;
}

int m(int x, int y) {
  return x-y;
}

int main() {
  int u, v, w;
  int t = g(h(4), u, m(v,2), w+1);  
  return 0;
}

void main2() {
  int u, v, w;  
  int t = f(g(h(4), u, m(v,2), w+1));
}
