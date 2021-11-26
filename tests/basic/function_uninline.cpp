

void g(int x, int y) {}

void gtwice(int x) {
  g(x, x);
}

void test_trivial() {
  gtwice_body: { g(3, 3); }
}

void f(int x) {
  int a = x+1;
  g(a, x);
}

void test_basic() {
  int r = 5;
  fbody:{
    int b = (r+2)+1;
    g(b, r+2);
  }
}

void iter_nat_for(int n, void body(int)) {
  for (int i = 0; i < n; i++) {
    body(i);
  }
}

int test_ho() {
  int s = 0;
  int m = 3;
  hobody: {
    for (int j = 0; j < m; j++) {
      {
        s += 2*j;
        s -= j;
      }
    }
  }
}
